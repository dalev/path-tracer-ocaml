open! Base
open Lwt.Syntax
open Path_tracer
module Image = Bimage.Image
module Pixel = Bimage.Pixel

module Args = struct
  type t =
    { width: int
    ; height: int
    ; spp: int
    ; output: string
    ; no_progress: bool
    ; max_bounces: int }

  let parse () =
    let width = ref 600 in
    let height = ref 300 in
    let spp = ref 1 in
    let file = ref "output.png" in
    let no_progress = ref false in
    let max_bounces = ref 4 in
    let usage_msg =
      Printf.sprintf "Defaults: width = %d, height = %d, output = %s" !width !height !file
    in
    Caml.Arg.parse
      [ ("-width", Set_int width, "<integer> image width")
      ; ("-height", Set_int height, "<integer> image height")
      ; ("-samples-per-pixel", Set_int spp, "<integer> samples-per-pixel")
      ; ("-o", Set_string file, "<file> output file")
      ; ("-no-progress", Set no_progress, "suppress progress monitor")
      ; ("-max-bounces", Set_int max_bounces, "<integer> max ray bounces") ]
      (fun (_ : string) -> failwith "No anonymous arguments expected")
      usage_msg ;
    { width= !width
    ; height= !height
    ; spp= !spp
    ; output= !file
    ; no_progress= !no_progress
    ; max_bounces= !max_bounces }
end

let color_space = Bimage.rgb
let mkImage width height = Image.v Bimage.f64 color_space width height

let camera aspect =
  let eye = P3.create ~x:13.0 ~y:2.0 ~z:4.5 in
  let target = P3.origin in
  let up = V3.unit_y in
  Camera.create ~eye ~target ~up ~aspect ~vertical_fov_deg:20.0

module Shirley_spheres = struct
  let p3 x y z = P3.create ~x ~y ~z
  let solid_tex r g b = Texture.solid (Color.create ~r ~g ~b)
  let solid_lambertian r g b = Material.lambertian @@ solid_tex r g b

  let ground =
    let a = solid_tex 0.2 0.3 0.1 in
    let b = solid_tex 0.9 0.9 0.9 in
    let checks = Material.lambertian @@ Texture.checker ~width:1000 ~height:2000 a b in
    Shape.sphere ~material:checks ~center:(p3 0.0 (-1000.0) 0.0) ~radius:1000.0

  let big_spheres =
    let glass = Material.glass in
    let metal = Material.metal (solid_tex 0.7 0.6 0.5) in
    let blue = solid_lambertian 0.0 0.1 0.4 in
    let radius = 1.0 in
    [ Shape.sphere ~material:glass ~center:(p3 (-4.0) 1.0 0.0) ~radius
    ; Shape.sphere ~material:metal ~center:(p3 0.0 1.0 0.0) ~radius
    ; Shape.sphere ~material:blue ~center:(p3 4.0 1.0 0.0) ~radius ]

  let randomf () = Random.float 1.0

  let random_v3 () =
    let x = randomf () in
    let y = randomf () in
    let z = randomf () in
    V3.create ~x ~y ~z

  let random_lambertian () =
    let c = Color.of_v3 V3.Infix.(random_v3 () * random_v3 ()) in
    Material.lambertian (Texture.solid c)

  let random_material () =
    let open Float.O in
    let roll = randomf () in
    if roll < 0.8 then
      random_lambertian ()
    else if roll < 0.95 then
      let z = (0.5 * randomf ()) + 0.5 in
      Material.metal (solid_tex z z z)
    else
      Material.glass

  let perturb x = Float.of_int x +. (0.9 *. randomf ())

  let small_sphere a b =
    let x = perturb a in
    let z = perturb b in
    let radius = 0.2 in
    let center = p3 x radius z in
    let p = p3 4.0 radius 0.0 in
    if Float.( > ) (V3.quadrance (V3.of_points ~src:center ~tgt:p)) 0.81 then
      let material = random_material () in
      Some (Shape.sphere ~material ~center ~radius)
    else
      None

  let spheres () =
    let rng = List.range (-11) 11 ~start:`inclusive ~stop:`inclusive in
    ground :: big_spheres
    @ List.concat_map rng ~f:(fun a ->
          List.filter_map rng ~f:(fun b -> small_sphere a b) )
end

let background ray =
  let one = V3.of_float 1.0 in
  let escape_color = V3.create ~x:0.5 ~y:0.7 ~z:1.0 in
  let d = V3.normalize (Ray.direction ray) in
  let t = 0.5 *. (V3.dot d V3.unit_y +. 1.0) in
  Color.of_v3 (V3.lerp t one escape_color)

let main args =
  let {Args.width; height; spp; output; no_progress; max_bounces} = args in
  let img = mkImage width height in
  let write_pixel ~x ~y {Color.r; g; b} =
    let px = Pixel.empty Bimage.rgb in
    Pixel.set px 0 r ; Pixel.set px 1 g ; Pixel.set px 2 b ; Image.set_pixel img x y px
  in
  let i =
    Integrator.create ~width ~height ~write_pixel ~max_bounces ~samples_per_pixel:spp
  in
  let spheres = Random.init 42 ; Shirley_spheres.spheres () in
  let* () = Lwt_io.printf "dim = %d x %d;\n" width height in
  let* () = Lwt_io.printf "#spheres = %d\n" (List.length spheres) in
  let update_progress =
    if no_progress then
      None
    else
      Some (fun pct -> Lwt_io.printf "\rProgress: %3.1f%%" pct) in
  let* () =
    Integrator.render ?update_progress i
      (Scene.create (camera (width // height)) spheres ~background)
  in
  let* () = Lwt_io.printf "\n" in
  let* () =
    match Bimage_io.write output img with
    | Ok () -> Lwt.return_unit
    | Error (`File_not_found f) -> Lwt_io.printf "File not found: %s" f
    | Error (#Bimage.Error.t as other) -> Lwt.return @@ Bimage.Error.unwrap (Error other)
  in
  Lwt_io.printf "Done\n"

let () = Lwt_main.run @@ main (Args.parse ())
