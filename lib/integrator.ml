open! Base
open! Stdio

type t =
  { width: int
  ; height: int
  ; write_pixel: x:int -> y:int -> Color.t -> unit
  ; samples_per_pixel: int
  ; max_bounces: int }

let create ~width ~height ~write_pixel ~samples_per_pixel ~max_bounces =
  {width; height; write_pixel; samples_per_pixel; max_bounces}

module Tile = struct
  type t = {row: int; col: int; width: int; height: int}

  let area t = t.width * t.height

  let split_width t =
    let half_w = t.width / 2 in
    let lhs = {t with width= half_w} in
    let rhs = {t with col= t.col + half_w; width= t.width - half_w} in
    (lhs, rhs)

  let split_height t =
    let half_h = t.height / 2 in
    let lhs = {t with height= half_h} in
    let rhs = {t with row= t.row + half_h; height= t.height - half_h} in
    (lhs, rhs)

  let split_once t = if t.width > t.height then split_width t else split_height t

  let split t ~max_area =
    let rec loop t =
      if area t <= max_area then [t]
      else
        let lhs, rhs = split_once t in
        loop lhs @ loop rhs in
    loop t

  let create ~width ~height = {row= 0; col= 0; width; height}
end

let trace_ray ray scene max_bounces samples =
  let take_2d =
    let module L = Low_discrepancy in
    let samples_index = ref 2 in
    fun () ->
      let j = !samples_index in
      let u = L.Sample.get samples j and v = L.Sample.get samples (j + 1) in
      samples_index := j + 2 ;
      (u, v) in
  let diffuse_plus_light = Scene.diffuse_plus_light_pdf scene in
  let rec loop ray max_bounces =
    if max_bounces <= 0 then Color.black
    else
      let max_bounces = max_bounces - 1 in
      match Scene.intersect scene ray with
      | None -> Scene.background scene ray
      | Some h -> (
          let emit = Hit.emit h in
          let u, v = take_2d () in
          match (Hit.scatter h u : Scatter.t) with
          | Absorb -> emit
          | Specular (scatter_dir, attenuation) ->
              let scattered_ray = Ray.create (Hit.point h) scatter_dir in
              let open Color.Infix in
              emit + (attenuation * loop scattered_ray max_bounces)
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
                    Ray.create (Hit.point h) (Shader_space.rotate_inv ss dir) in
                  emit + (Color.scale attenuation pd * loop scattered_ray max_bounces) )
  in
  loop ray max_bounces

let gamma = Color.map ~f:Float.sqrt

let render_tile t tile scene write_pixel tile_sampler =
  let x0 = tile.Tile.col in
  let y0 = tile.Tile.row in
  let widthf = 1 // t.width in
  let heightf = 1 // t.height in
  let spp_invf = 1 // t.samples_per_pixel in
  for y = y0 to y0 + tile.Tile.height do
    let yf = Float.of_int (t.height - 1 - y) in
    for x = x0 to x0 + tile.Tile.width do
      let xf = Float.of_int x in
      let color = ref Color.black in
      for _ = 1 to t.samples_per_pixel do
        let s = Low_discrepancy.step tile_sampler in
        let dx = Low_discrepancy.Sample.get s 0 and dy = Low_discrepancy.Sample.get s 1 in
        let cx = (xf +. dx) *. widthf in
        let cy = (yf +. dy) *. heightf in
        let ray = Scene.camera_ray scene cx cy in
        color := Color.Infix.( + ) !color @@ trace_ray ray scene t.max_bounces s
      done ;
      write_pixel ~x ~y @@ gamma (Color.scale !color spp_invf)
    done
  done

let create_tile_samplers t tiles =
  let s = ref (Low_discrepancy.create (2 + (2 * (t.max_bounces + 1)))) in
  List.map tiles ~f:(fun tile ->
      let n = t.samples_per_pixel * Tile.area tile in
      let tile_sampler, suffix = Low_discrepancy.split_at !s n in
      s := suffix ;
      (tile, tile_sampler) )

let render ?(update_progress = ignore) t scene =
  let max_area = 32 * 32 in
  let tiles = Tile.split ~max_area (Tile.create ~width:t.width ~height:t.height) in
  let num_tiles = List.length tiles in
  let tiles_and_samplers = create_tile_samplers t tiles in
  printf "#tiles = %d\n" num_tiles ;
  List.iteri tiles_and_samplers ~f:(fun i (tile, sampler) ->
      render_tile t tile scene t.write_pixel sampler ;
      update_progress ((i + 1) * 100 // num_tiles) )
