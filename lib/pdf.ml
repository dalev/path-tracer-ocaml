open! Base

type t = Diffuse

let sample t (_ : Shader_space.t) u v =
  match t with Diffuse -> Shader_space.unit_square_to_hemisphere u v

let eval t (dir : V3.t) (_ : Shader_space.t) =
  let _, _, z = V3.coords @@ V3.normalize dir in
  match t with Diffuse -> if Float.is_negative z then 0.0 else z /. Float.pi

let diffuse = Diffuse
