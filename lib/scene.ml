open Base

type t = { bvh : Bvh.t; background : Ray.t -> Color.t; camera : Camera.t }

let create camera shapes ~background =
  let bvh =
    List.map shapes ~f:(fun s -> Shape.transform s ~f:(Camera.transform camera))
    |> Bvh.create
  in
  { bvh; background; camera }

let camera_ray t cx cy = Camera.ray t.camera cx cy

let background t ray = t.background ray

let intersect t ray =
  let t_min = 0.0 and t_max = Float.max_finite_value in
  Bvh.intersect t.bvh ray ~t_min ~t_max

let diffuse_plus_light_pdf (_ : t) =
  (* CR dalev: area lights *)
  Pdf.diffuse
