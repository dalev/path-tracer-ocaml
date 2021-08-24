open Base
open Path_tracer
open Stdio
module L = Low_discrepancy_sequence.Simple
module Task = Domainslib.Task

module Args = struct
  type t =
    { width : int
    ; height : int
    ; iterations : int
    ; max_bounces : int
    ; photon_count : int
    ; alpha : float
    ; output : string
    }

  let parse ?(specs = []) () =
    let width = ref 600 in
    let height = ref !width in
    let iterations = ref 10 in
    let photon_count = ref 75_000 in
    let file = ref "output.png" in
    let alpha = ref (2 // 3) in
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
          ; "-iterations", Set_int iterations, "<integer> # photon-map iterations"
          ; "-photon-count", Set_int photon_count, "<integer> #photons per iteration"
          ; "-alpha", Set_float alpha, "<float-in-(0,1)> photon-map alpha"
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
    ; iterations = !iterations
    ; max_bounces = !max_bounces
    ; photon_count = !photon_count
    ; alpha = !alpha
    ; output = !file
    }
  ;;
end

type sampler = offset:int -> dimension:int -> float

module Point_light = struct
  type t =
    { position : P3.t
    ; color : Color.t
    }

  let create ~position ~power ~color =
    let color = Color.scale color power in
    { position; color }
  ;;

  let random_direction u v =
    let open Float.O in
    let theta = 2.0 * Float.pi * u in
    let phi = Float.acos (1.0 - (2.0 * v)) in
    let sin_phi = Float.sin phi in
    V3.create
      ~x:(sin_phi * Float.cos theta)
      ~y:(sin_phi * Float.sin theta)
      ~z:(Float.cos phi)
  ;;

  let random_ray t u v =
    let dir = random_direction u v in
    Ray.create t.position dir
  ;;
end

module Spot_light = struct
  type t =
    { shader_space : Shader_space.t
    ; color : Color.t
    }

  let angle = 0.5 *. 30.0 *. Float.pi /. 180.0
  let disk_radius = Float.atan angle

  let create ~position ~direction ~color ~power =
    let color = Color.scale color power in
    let shader_space = Shader_space.create (V3.normalize direction) position in
    { shader_space; color }
  ;;

  let random_ray t u v =
    let open Float.O in
    let r = disk_radius * Float.sqrt u in
    let theta = v * 2.0 * Float.pi in
    let x = r * Float.cos theta in
    let y = r * Float.sin theta in
    let z = 1.0 in
    Shader_space.world_ray t.shader_space @@ V3.create ~x ~y ~z
  ;;
end

module Light = struct
  type t =
    | Point of Point_light.t
    | Spot of Spot_light.t

  let random_ray t u v =
    match t with
    | Point l -> Point_light.random_ray l u v
    | Spot l -> Spot_light.random_ray l u v
  ;;

  let color = function
    | Point l -> l.Point_light.color
    | Spot l -> l.Spot_light.color
  ;;

  let create_spot ~position ~direction ~color ~power =
    Spot (Spot_light.create ~position ~direction ~color ~power)
  ;;

  let create_point ~position ~color ~power =
    Point (Point_light.create ~position ~color ~power)
  ;;
end

module Photon = struct
  type t =
    { shader_space : Shader_space.t (* local geom of diffuse interaction *)
    ; wi : V3.t
    ; flux : Color.t
    ; radius : float
          (* CR dalev: it's a little unfortunate that we store the 
    radius with each photon -- it'll be the same for all photons in each
    Photon_map.  But to satisfy the Shape_tree functor signature, we need to be
    able to compute the bbox from [Photon.t]. *)
    }

  let create shader_space ray flux ~radius =
    let wi = V3.normalize @@ V3.Infix.( ~- ) (Ray.direction ray) in
    { wi; shader_space; flux; radius }
  ;;

  let center t = Shader_space.world_origin t.shader_space
end

module Photon_map : sig
  type t

  (* call [f] on each photon whose radius includes the given point *)
  val fold_neighbors : t -> P3.t -> init:'a -> f:('a -> Photon.t -> 'a) -> 'a
  val length : t -> int

  val create
    :  Task.pool
    -> sampler
    -> scene_intersect:(Ray.t -> Hit.t option)
    -> radius:float
    -> photon_count:int
    -> max_bounces:int
    -> lights:Light.t list
    -> t
end = struct
  module Tree = Shape_tree.Make (Shape_tree.Array_leaf (struct
    include Photon

    let length_cutoff = 8

    let bbox t =
      let center = Shader_space.world_origin t.shader_space in
      let radius = P3.create ~x:t.radius ~y:t.radius ~z:t.radius in
      let min = P3.Infix.(center - radius)
      and max = P3.Infix.(center + radius) in
      Bbox.create ~min ~max
    ;;

    type hit = unit

    let hit_t () = -1.0
    let depth _ = 0
    let length _ = 1

    let intersect (_ : t) (_ : Ray.t) ~t_min:_ ~t_max:_ =
      failwith "Photon_map.Tree.intersect: not used"
    ;;
  end))

  type t =
    { tree : Tree.t
    ; length : int
    }

  let length t = t.length

  let fold_neighbors t point ~init ~f =
    let open Float.O in
    Tree.fold_neighbors t.tree point ~init ~f:(fun acc a ->
        Array.fold a ~init:acc ~f:(fun acc p ->
            let v = V3.of_points ~src:(Photon.center p) ~tgt:point in
            if V3.quadrance v < Float.square p.radius then f acc p else acc))
  ;;

  let trace_photon light sampler max_bounces intersect ~radius =
    let photons = ref [] in
    let take_2d d' =
      let u = sampler ~dimension:d'
      and v = sampler ~dimension:(d' + 1) in
      u, v
    in
    let rec loop ray flux max_bounces dim =
      if max_bounces > 0
      then (
        let max_bounces = max_bounces - 1 in
        let u, v = take_2d dim in
        let dim = dim + 2 in
        match intersect ray with
        | None -> ()
        | Some h ->
          (match Hit.scatter h u with
          | Absorb -> ()
          | Specular (ray, color) -> loop ray Color.Infix.(flux * color) max_bounces dim
          | Diffuse color ->
            let ss = Hit.shader_space h in
            let flux = Color.Infix.(flux * color) in
            photons := Photon.create ss ray flux ~radius :: !photons;
            let color_max = Color.max_coord color in
            let open Float.O in
            if u <= color_max
            then (
              let cm_inv = 1.0 / color_max in
              let flux = Color.scale flux cm_inv in
              let u = u * cm_inv in
              let dir = Shader_space.unit_square_to_hemisphere u v in
              let ray = Shader_space.world_ray ss dir in
              loop ray flux max_bounces dim)))
    in
    let u, v = take_2d 0 in
    let ray = Light.random_ray light u v in
    let dimension = 2 in
    loop ray (Light.color light) max_bounces dimension;
    !photons
  ;;

  let create pool sampler ~scene_intersect ~radius ~photon_count ~max_bounces ~lights =
    let module Chan = Domainslib.Chan in
    let c = Chan.make_unbounded () in
    let collector =
      Caml.Domain.spawn (fun () ->
          let rec loop photons =
            match Chan.recv c with
            | Some ps -> loop @@ List.rev_append ps photons
            | None -> photons
          in
          loop [])
    in
    (* CR dalev: reuses sampler for different lights, fix me.  What we actually want
    is to divide [photon_count] among all the lights in proportion to each light's
    power. *)
    List.iter lights ~f:(fun light ->
        Task.parallel_for pool ~start:0 ~finish:(photon_count - 1) ~body:(fun i ->
            let s ~dimension = sampler ~offset:i ~dimension in
            Chan.send c @@ Some (trace_photon light s max_bounces scene_intersect ~radius)));
    Chan.send c None;
    let photons = Caml.Domain.join collector in
    let length = List.length photons in
    if length = 0 then failwith "BUG: no photons";
    let num_bins = 8 in
    { tree = Tree.create ~pool ~num_bins photons; length }
  ;;
end

module Make (Scene : sig
  val bbox : Bbox.t
  val camera : Camera.t
  val lights : Light.t list
  val intersect : Ray.t -> Hit.t option
  val args : Args.t
end) =
struct
  open Scene

  let { Args.photon_count
      ; width
      ; height
      ; alpha
      ; iterations
      ; max_bounces
      ; output = file_name
      }
    =
    args
  ;;

  let inv_photon_count = 1 // photon_count

  let init_radius2 =
    let { P3.x; y; z } = P3.Infix.( - ) (Bbox.max bbox) (Bbox.min bbox) in
    let a = (x +. y +. z) /. 3.0 in
    let b = (width + height) // 2 in
    Float.square @@ (a /. b)
  ;;

  let create_blank_image () = Bimage.Image.v Bimage.Type.f64 Bimage.Color.rgb width height

  let render_image img pool sampler pmap =
    let module Image = Bimage.Image in
    let module Pixel = Bimage.Pixel in
    let write_pixel ~x ~y color =
      let y = height - 1 - y in
      let p = Image.get_pixel img x y in
      let r, g, b = Color.to_rgb color in
      Pixel.set p 0 (r +. Pixel.get p 0);
      Pixel.set p 1 (g +. Pixel.get p 1);
      Pixel.set p 2 (b +. Pixel.get p 2);
      Image.set_pixel img x y p
    in
    let inv_widthf = 1 // width in
    let inv_heightf = 1 // height in
    let estimate_color ~x ~y sampler =
      let rec loop ray beta max_bounces dimension =
        if max_bounces <= 0
        then Color.black
        else (
          match intersect ray with
          | None -> Color.black
          | Some h ->
            let max_bounces = max_bounces - 1 in
            let u = sampler ~dimension in
            (match Hit.scatter h u with
            | Absorb -> Color.black
            | Specular (ray, color) ->
              let beta = Color.Infix.(color * beta) in
              loop ray beta max_bounces (dimension + 1)
            | Diffuse color ->
              let beta = Color.Infix.(color * beta) in
              let shader_space = Hit.shader_space h in
              let hit_point = Shader_space.world_origin shader_space in
              let hit_normal = Shader_space.world_normal shader_space in
              let open Float.O in
              let distance p =
                let tgt = Photon.center p in
                Float.sqrt @@ V3.quadrance (V3.of_points ~src:hit_point ~tgt)
              in
              (* This is the cone filter suggested in 'Global Illumination Using Photon Maps' [Jensen 1996] *)
              let k = 1.0 in
              let weight ~radius p = 1.0 - (distance p / (k * radius)) in
              let normalizer = 1.0 - (2.0 / (3.0 * k)) in
              let neighbors =
                Photon_map.fold_neighbors pmap hit_point ~init:[] ~f:(fun neighbors p ->
                    let p_ss = p.Photon.shader_space in
                    let p_normal = Shader_space.world_normal p_ss in
                    if V3.dot p_normal hit_normal > 1e-3
                    then p :: neighbors
                    else neighbors)
              in
              (match neighbors with
              | [] -> Color.black
              | hd :: _ as neighbors ->
                let radius = hd.Photon.radius in
                let area = Float.pi * Float.square radius in
                let flux =
                  List.fold neighbors ~init:Color.black ~f:(fun sum_flux p ->
                      (* CR dalev: do we need to divide by pdf of eye ray bouncing towards the incoming photon? *)
                      (* let pdf = V3.dot hit_normal p.Photon.wi / Float.pi in *)
                      let pdf = 1.0 in
                      let w = weight ~radius p / pdf in
                      let flux = Color.scale p.Photon.flux w in
                      Color.Infix.(sum_flux + flux))
                in
                let open Color.Infix in
                Color.scale (beta * flux) (1.0 / (area *. normalizer)))))
      in
      let dx = sampler ~dimension:0
      and dy = sampler ~dimension:1 in
      let cx = inv_widthf *. (dx +. Float.of_int x)
      and cy = inv_heightf *. (dy +. Float.of_int y) in
      let dimension = 2 in
      loop (Camera.ray camera cx cy) Color.white max_bounces dimension
    in
    Task.parallel_for
      ~chunk_size:(32 * 32)
      pool
      ~start:0
      ~finish:((width * height) - 1)
      ~body:(fun pixel ->
        let x = pixel % width
        and y = pixel / width in
        let sampler ~dimension = sampler ~offset:pixel ~dimension in
        let color' = estimate_color ~x ~y sampler in
        let color = Color.scale color' inv_photon_count in
        write_pixel ~x ~y color)
  ;;

  let radius2 i =
    assert (i >= 1);
    let product = ref 1.0 in
    for k = 1 to i - 1 do
      let k = Float.of_int k in
      product := !product *. (k +. alpha) /. k
    done;
    !product *. init_radius2 /. Float.of_int i
  ;;

  let radius i = Float.sqrt @@ radius2 i

  let offset_sampler s base ~offset ~dimension =
    L.get s ~offset:(offset + base) ~dimension
  ;;

  let save_image img =
    match Bimage_io.write file_name img with
    | Ok () -> ()
    | Error (`File_not_found s) -> failwith @@ "file not found: " ^ s
    | Error (#Bimage.Error.t as e) -> Bimage.Error.exc e
  ;;

  let go pool =
    printf "#max-bounces = %d\n" max_bounces;
    printf "#photons/iter = %d\n" photon_count;
    printf "#iterations = %d\n" iterations;
    printf "-----\n%!";
    let p_sampler = L.create ~dimensions:(2 + (2 * max_bounces)) in
    let e_sampler =
      (* eye paths terminate at the first diffuse interaction, and so they only use
      one sample dimension per bounce. *)
      L.create ~dimensions:(2 + max_bounces)
    in
    let img_sum = create_blank_image () in
    let img_avg = create_blank_image () in
    for i = 0 to iterations - 1 do
      let radius = radius (i + 1) in
      printf "#iteration = %d, radius = %.3f\n%!" i radius;
      let pmap =
        Photon_map.create
          pool
          (offset_sampler p_sampler (i * photon_count))
          ~scene_intersect:Scene.intersect
          ~photon_count
          ~max_bounces
          ~radius
          ~lights
      in
      printf "  photon map length = %d\n%!" (Photon_map.length pmap);
      let eye_sample_base = i * width * height in
      render_image img_sum pool (offset_sampler e_sampler eye_sample_base) pmap;
      let n = 1 // (i + 1) in
      let sum_data = img_sum.Bimage.Image.data in
      let avg_data = img_avg.Bimage.Image.data in
      let gamma x = x **. (1.0 /. 2.2) in
      Task.parallel_for
        pool
        ~start:0
        ~finish:((3 * width * height) - 1)
        ~body:(fun i ->
          let f = Bigarray.Array1.get sum_data i in
          Bigarray.Array1.set avg_data i (gamma (f *. n)));
      save_image img_avg
    done
  ;;
end
