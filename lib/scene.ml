open Base

type t = {
  shapes : Shape.t list;
  background : Ray.t -> Color.t;
  camera : Camera.t;
}

let create camera shapes ~background =
  let shapes =
    List.map shapes ~f:(fun s -> Shape.transform s ~f:(Camera.transform camera))
  in
  { shapes; background; camera }

let camera_ray t cx cy = Camera.ray t.camera cx cy

let background t ray = t.background ray

let intersect t ray =
  let t_min = 0.0 and t_max = Float.max_finite_value in
  if
    List.exists t.shapes ~f:(fun s ->
        Option.is_some (Shape.intersect s ray t_min t_max))
  then Some 1.0
  else None
