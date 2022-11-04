open! Base
module Image = Bimage.Image

type image = (float, Bigarray.float64_elt, [ `Rgb ]) Image.t

type t =
  { tile : Tile.t
  ; pixels : image
  ; filter_kernel : Filter_kernel.t
  }

let border t = Filter_kernel.pixel_radius t.filter_kernel

let create tile ~filter_kernel =
  let border = Filter_kernel.pixel_radius filter_kernel in
  let width = Tile.width tile + (2 * border)
  and height = Tile.height tile + (2 * border) in
  let pixels = Image.v Bimage.f64 Bimage.rgb width height in
  { tile; pixels; filter_kernel }
;;

let write_pixel t ~x ~y color =
  let border = border t in
  let x = x + border
  and y = y + border in
  Filter_kernel.iter t.filter_kernel ~f:(fun ~dx ~dy weight ->
    let x = x + dx
    and y = y + dy in
    let incr ch v =
      let a = Image.get t.pixels x y ch in
      Image.set t.pixels x y ch (Caml.Float.fma weight v a)
    in
    let r, g, b = Color.to_rgb color in
    incr 0 r;
    incr 1 g;
    incr 2 b)
;;

let iter t ~f =
  let tile = t.tile in
  let border = border t in
  for local_y = 0 to t.pixels.Image.height - 1 do
    let global_y = local_y + tile.Tile.row - border in
    for local_x = 0 to t.pixels.Image.width - 1 do
      let global_x = local_x + tile.Tile.col - border in
      let img_ref = Image.get t.pixels local_x local_y in
      let r = img_ref 0
      and g = img_ref 1
      and b = img_ref 2 in
      f ~global_x ~global_y (Color.create ~r ~g ~b)
    done
  done
;;
