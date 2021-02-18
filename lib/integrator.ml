open! Base

type t = {
  width : int;
  height : int;
  write_pixel : x:int -> y:int -> r:float -> g:float -> b:float -> unit;
}

let create ~width ~height ~write_pixel = { width; height; write_pixel }

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

let render_tile t tile scene write_pixel =
  let x0 = tile.Tile.col in
  let y0 = tile.Tile.row in
  for y = y0 to y0 + tile.Tile.height do
    let cy = (t.height - 1 - y) // t.height in
    for x = x0 to x0 + tile.Tile.width do
      let cx = x // t.width in
      let ray = Scene.camera_ray scene cx cy in
      let color =
        match Scene.intersect scene ray with
        | None -> Scene.background scene ray
        | Some _ -> Color.create ~r:0.7 ~g:0.1 ~b:0.2
      in
      let r, g, b = Color.rgb color in
      write_pixel ~x ~y ~r ~g ~b
    done
  done

let render t scene =
  let module Task = Domainslib.Task in
  let max_area = 32 * 32 in
  let tiles =
    Tile.split ~max_area (Tile.create ~width:t.width ~height:t.height)
  in
  let pool = Task.setup_pool ~num_domains:7 in
  let tasks =
    List.map tiles ~f:(fun tile ->
        let _bvh_counters = () in
        Task.async pool (fun () ->
            let _tile_sampler = () in
            render_tile t tile scene t.write_pixel))
  in
  List.fold tasks ~init:() ~f:(fun () promise -> Task.await pool promise);
  Task.teardown_pool pool
