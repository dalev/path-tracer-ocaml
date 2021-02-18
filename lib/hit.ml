type t = {
  t_hit : float;
  point : P3.t;
  shape : Shape.t;
  shader_space : Shader_space.t;
  tex_coord : Texture.Coord.t;
}

let create ~t_hit shape ray =
  let point = Ray.point_at ray t_hit in
  let normal = Shape.normal shape point in
  let tex_coord = Shape.tex_coord shape ~surface:point ~normal in
  let shader_space = Shader_space.create normal point in
  { t_hit; point; shape; shader_space; tex_coord }

let t_hit t = t.t_hit

let point t = t.point

let scatter t =
  let m = Shape.material t.shape in
  Material.scatter m t.shader_space t.tex_coord

let material t = Shape.material t.shape

let emit t = Material.emit (material t) t.tex_coord

let shader_space t = t.shader_space
