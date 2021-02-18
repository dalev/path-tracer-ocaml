open Base

type t = Shape.t list

let create shapes = shapes

let intersect t ray ~t_min ~t_max =
  List.fold t ~init:None ~f:(fun acc shape ->
      let t_max =
        match acc with Some (acc_t_hit, _) -> acc_t_hit | None -> t_max
      in
      match Shape.intersect shape ray t_min t_max with
      | Some t_hit -> Some (t_hit, shape)
      | None -> acc)
  |> Option.map ~f:(fun (t_hit, shape) -> Hit.create ~t_hit shape)
