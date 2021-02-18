open! Base
open Stdio
open Path_tracer
module Image = Bimage.Image
module Color = Bimage.Color
module Pixel = Bimage.Pixel

module Args = struct
  type t = { width : int; height : int; spp : int; output : string }

  let parse () =
    let width = ref 600 in
    let height = ref 300 in
    let spp = ref 1 in
    let file = ref "shirley-spheres.png" in
    let usage_msg =
      Printf.sprintf "Defaults: width = %d, height = %d, output = %s" !width
        !height !file
    in
    Caml.Arg.parse
      [
        ("-width", Set_int width, "<integer> image width");
        ("-height", Set_int height, "<integer> image height");
        ("-samples-per-pixel", Set_int spp, "<integer> samples-per-pixel");
        ("-o", Set_string file, "<file> output file");
      ]
      (fun (_ : string) -> failwith "No anonymous arguments expected")
      usage_msg;
    { width = !width; height = !height; spp = !spp; output = !file }
end

let color_space = Bimage.rgb

let mkImage width height = Image.v Bimage.f64 color_space width height

let _camera =
  let eye = P3.create ~x:13.0 ~y:2.0 ~z:4.5 in
  let target = P3.origin in
  let up = V3.unit_y in
  Camera.create ~eye ~target ~up

let main args =
  let { Args.width; height; spp = _; output } = args in
  let img = mkImage width height in
  let write_pixel ~x ~y ~r ~g ~b =
    let px = Pixel.empty Bimage.rgb in
    Pixel.set px 0 r;
    Pixel.set px 1 g;
    Pixel.set px 2 b;
    Image.set_pixel img x y px
  in
  let i = Integrator.create ~width ~height ~write_pixel in
  Integrator.render i (Scene.create ());
  (match Bimage_io.write output img with
  | Ok () -> ()
  | Error (`File_not_found f) -> printf "File not found: %s" f
  | Error (#Bimage.Error.t as other) -> Bimage.Error.unwrap (Error other));
  printf "Done\n"

let () = main (Args.parse ())
