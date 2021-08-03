type t =
  | Absorb
  | Specular of Ray.t * Color.t
  | Diffuse of Color.t
