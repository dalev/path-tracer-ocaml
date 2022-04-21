open Base
open Stdio
open Path_tracer
module Image = Bimage.Image

module Args = struct
  type t =
    { width : int
    ; height : int
    ; samples_per_pixel : int
    ; output : string
    ; no_progress : bool
    ; max_bounces : int
    }

  let parse ?(specs = []) () =
    let width = ref 600 in
    let height = ref !width in
    let spp = ref 1 in
    let file = ref "output.png" in
    let no_progress = ref false in
    let max_bounces = ref 4 in
    let usage_msg =
      Printf.sprintf "Defaults: width = %d, height = %d, output = %s" !width !height !file
    in
    let specs =
      specs
      @ Caml.Arg.
          [ "-width", Set_int width, "<integer> image width"
          ; "-height", Set_int height, "<integer> image height"
          ; "-samples-per-pixel", Set_int spp, "<integer> samples-per-pixel"
          ; "-o", Set_string file, "<file> output file"
          ; "-no-progress", Set no_progress, "suppress progress monitor"
          ; "-max-bounces", Set_int max_bounces, "<integer> max ray bounces"
          ]
    in
    Caml.Arg.parse
      specs
      (fun (_ : string) -> failwith "No anonymous arguments expected")
      usage_msg;
    { width = !width
    ; height = !height
    ; samples_per_pixel = !spp
    ; output = !file
    ; no_progress = !no_progress
    ; max_bounces = !max_bounces
    }
  ;;
end

let with_elapsed_time f =
  let start = Time_now.nanoseconds_since_unix_epoch () in
  let x = f () in
  let stop = Time_now.nanoseconds_since_unix_epoch () in
  let elapsed = Int63.(stop - start) in
  elapsed, x
;;

module Film : sig
  type t

  val create : width:int -> height:int -> output:string -> samples_per_pixel:int -> t
  val save_exn : t -> unit
  val write_pixel : t -> x:int -> y:int -> Color.t -> unit
end = struct
  type t =
    { raw_img : [ `Rgb ] Bimage.image_f64
    ; output : string
    ; samples_per_pixel : int
    ; filter_kernel : Filter_kernel.t
    }

  let color_space = Bimage.rgb
  let mkImage width height = Image.v Bimage.f64 color_space width height
  let pixel_radius = 1

  let create ~width ~height ~output ~samples_per_pixel =
    let raw_img = mkImage width height in
    let filter_kernel = Filter_kernel.Binomial.create ~order:5 ~pixel_radius in
    { raw_img; output; samples_per_pixel; filter_kernel }
  ;;

  let write_pixel t ~x ~y color =
    let in_bounds x y =
      0 <= x && x < t.raw_img.Image.width && 0 <= y && y < t.raw_img.Image.height
    in
    Filter_kernel.iter t.filter_kernel ~f:(fun ~dx ~dy weight ->
        let x = x + dx
        and y = y + dy in
        if in_bounds x y
        then (
          let incr ch v =
            let v = v *. weight in
            let a = Image.get t.raw_img x y ch in
            Image.set t.raw_img x y ch (a +. v)
          in
          let r, g, b = Color.to_rgb color in
          incr 0 r;
          incr 1 g;
          incr 2 b))
  ;;

  let save_exn t =
    let spp_inv = 1 // t.samples_per_pixel in
    let gamma f = Float.sqrt (f *. spp_inv) in
    let gamma_encoded_img = Bimage.Image.map_inplace gamma t.raw_img in
    match Bimage_unix.Stb.write t.output gamma_encoded_img with
    | Ok () -> ()
    | Error (#Bimage.Error.t as other) -> Bimage.Error.unwrap (Error other)
  ;;
end

module Make (Scene : sig
  val camera : Camera.t
  val intersect : Ray.t -> Hit.t option
  val background : Ray.t -> Color.t
end) =
struct
  let run { Args.width; height; max_bounces; samples_per_pixel; no_progress; output } =
    let film = Film.create ~width ~height ~output ~samples_per_pixel in
    let write_pixel = Film.write_pixel film in
    let i =
      Integrator.create
        ~width
        ~height
        ~write_pixel
        ~max_bounces
        ~samples_per_pixel
        ~intersect:Scene.intersect
        ~background:Scene.background
        ~camera:Scene.camera
        ~diffuse_plus_light:Pdf.diffuse
    in
    let elapsed =
      fst
      @@ with_elapsed_time (fun () ->
             if no_progress
             then Integrator.render i ~update_progress:ignore
             else (
               let total = Integrator.count_tiles i in
               let p =
                 let open Progress.Line in
                 list
                   [ spinner ()
                   ; elapsed ()
                   ; bar ~style:`ASCII total
                   ; count_to total
                   ; spacer 4
                   ]
               in
               let config =
                 let min_interval = Some (Progress.Duration.of_sec 0.2) in
                 Progress.Config.v ~min_interval ()
               in
               Progress.with_reporter ~config p (fun report ->
                   let update_progress () = report 1 in
                   Integrator.render i ~update_progress)))
    in
    Film.save_exn film;
    printf "rendered in: %.3f ms\n" (Float.of_int63 elapsed *. 1e-6)
  ;;
end
