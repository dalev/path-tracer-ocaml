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

let render_tile tile write_pixel widthf heightf =
  let x0 = tile.Tile.col in
  let y0 = tile.Tile.row in
  let g = 0.0 in
  for y = y0 to y0 + tile.Tile.height do
    let b = Float.of_int y /. heightf in
    for x = x0 to x0 + tile.Tile.width do
      let r = Float.of_int x /. widthf in
      write_pixel ~x ~y ~r ~g ~b
    done
  done

let render t _scene =
  let widthf = Float.of_int t.width in
  let heightf = Float.of_int t.height in
  let max_area = 32 * 32 in
  let tiles =
    Tile.split ~max_area (Tile.create ~width:t.width ~height:t.height)
  in
  List.iter tiles ~f:(fun tile -> render_tile tile t.write_pixel widthf heightf)
