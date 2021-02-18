type t = { t_hit : float; shape : Shape.t }

let create ~t_hit shape = { t_hit; shape }

let t_hit t = t.t_hit

let scatter t =
  let m = Shape.material t.shape in
  (* CR dalev: to do *)
  let tex_coord = Texture.Coord.create 0.0 0.0 in
  Material.scatter m tex_coord
