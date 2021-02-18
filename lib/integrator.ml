open! Base
open! Stdio
module Task = Domainslib.Task
module Channel = Domainslib.Chan

type t = {
  width : int;
  height : int;
  write_pixel : x:int -> y:int -> r:float -> g:float -> b:float -> unit;
  samples_per_pixel : int;
  max_bounces : int;
  max_threads : int;
}

let create ~width ~height ~write_pixel ~samples_per_pixel ~max_bounces
    ~max_threads =
  { width; height; write_pixel; samples_per_pixel; max_bounces; max_threads }

module Tile = struct
  type t = { row : int; col : int; width : int; height : int }

  let area t = t.width * t.height

  let split_width t =
    let half_w = t.width / 2 in
    let lhs = { t with width = half_w } in
    let rhs = { t with col = t.col + half_w; width = t.width - half_w } in
    (lhs, rhs)

  let split_height t =
    let half_h = t.height / 2 in
    let lhs = { t with height = half_h } in
    let rhs = { t with row = t.row + half_h; height = t.height - half_h } in
    (lhs, rhs)

  let split_once t =
    if t.width > t.height then split_width t else split_height t

  let split t ~max_area =
    let rec loop t =
      if area t <= max_area then [ t ]
      else
        let lhs, rhs = split_once t in
        loop lhs @ loop rhs
    in
    loop t

  let create ~width ~height = { row = 0; col = 0; width; height }
end

let trace_ray ray scene max_bounces =
  let diffuse_plus_light = Scene.diffuse_plus_light_pdf scene in
  let rec loop ray max_bounces =
    if max_bounces <= 0 then Color.black
    else
      match Scene.intersect scene ray with
      | None -> Scene.background scene ray
      | Some h -> (
          let emit = Hit.emit h in
          (* CR dalev: sampler *)
          let u = 0.23 in
          let v = 0.79 in
          match (Hit.scatter h : Scatter.t) with
          | Absorb -> emit
          | Specular (scattered_ray, attenuation) ->
              let open Color.Infix in
              emit + (attenuation * loop scattered_ray (max_bounces - 1))
          | Diffuse attenuation ->
              let open Color.Infix in
              let ss = Hit.shader_space h in
              let dir = Pdf.sample diffuse_plus_light ss u v in
              let diffuse_pd = Pdf.eval Pdf.diffuse dir ss in
              if Float.( = ) diffuse_pd 0.0 then emit
              else
                let divisor = Pdf.eval diffuse_plus_light dir ss in
                let pd = diffuse_pd /. divisor in
                if not (Float.is_finite pd) then emit
                else
                  let scattered_ray =
                    Ray.create (Hit.point h) (Shader_space.rotate_inv ss dir)
                  in
                  emit
                  + Color.scale attenuation pd
                    * loop scattered_ray (max_bounces - 1))
  in
  loop ray max_bounces

let render_tile t tile scene write_pixel =
  let x0 = tile.Tile.col in
  let y0 = tile.Tile.row in
  let widthf = 1.0 /. Float.of_int t.width in
  let heightf = 1.0 /. Float.of_int t.height in
  for y = y0 to y0 + tile.Tile.height do
    let yf = Float.of_int (t.height - 1 - y) in
    for x = x0 to x0 + tile.Tile.width do
      let xf = Float.of_int x in
      (* CR dalev: sampler *)
      let dx = 0.5 and dy = 0.5 in
      let cx = (xf +. dx) *. widthf in
      let cy = (yf +. dy) *. heightf in
      let ray = Scene.camera_ray scene cx cy in
      let color = trace_ray ray scene t.max_bounces in
      let r, g, b = Color.rgb color in
      write_pixel ~x ~y ~r ~g ~b
    done
  done

type tick = Tick | Done

let spawn_ticker update_progress channel num_tiles =
  let module Domain = Caml.Domain in
  let d =
    Domain.spawn (fun () ->
        let rec loop count =
          match Channel.recv channel with
          | Tick ->
              let count' = count + 1 in
              update_progress (count' // num_tiles);
              loop count'
          | Done -> ()
        in
        loop 0)
  in
  fun () -> Domain.join d

let render_parallel ?(update_progress = ignore) t scene tiles =
  let num_tiles = List.length tiles in
  let num_domains = 8 in
  let pool = Task.setup_pool ~num_domains in
  let channel = Channel.make_bounded num_domains in
  let join_ticker = spawn_ticker update_progress channel num_tiles in
  let tasks =
    List.map tiles ~f:(fun tile ->
        let _bvh_counters = () in
        Task.async pool (fun () ->
            let _tile_sampler = () in
            render_tile t tile scene t.write_pixel;
            Channel.send channel Tick))
  in
  List.fold tasks ~init:() ~f:(fun () promise -> Task.await pool promise);
  Channel.send channel Done;
  join_ticker ();
  Task.teardown_pool pool

let render ?(update_progress = ignore) t scene =
  let max_area = 32 * 32 in
  let tiles =
    Tile.split ~max_area (Tile.create ~width:t.width ~height:t.height)
  in
  let num_tiles = List.length tiles in
  printf "#tiles = %d\n" num_tiles;
  if t.max_threads > 1 then render_parallel ~update_progress t scene tiles
  else
    List.iteri tiles ~f:(fun i tile ->
        let _bvh_counters = () in
        let _tile_sampler = () in
        render_tile t tile scene t.write_pixel;
        update_progress ((i + 1) * 100 // num_tiles))
