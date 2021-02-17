open! Base

type t = {
  width : int;
  height : int;
  write_pixel : x:int -> y:int -> r:float -> g:float -> b:float -> unit;
}

let create ~width ~height ~write_pixel = { width; height; write_pixel }

let render t _scene =
  let widthf = Float.of_int t.width in
  let heightf = Float.of_int t.height in
  for x = 0 to t.width - 1 do
    let xf = Float.of_int x in
    for y = 0 to t.height - 1 do
      let yf = Float.of_int y in
      t.write_pixel ~x ~y ~r:(xf /. widthf) ~g:0.0 ~b:(yf /. heightf)
    done
  done
