type t = {shader_space: Shader_space.t; emit: Color.t; do_scatter: float -> Scatter.t}

let create ~t_hit shape ray =
  let point = Ray.point_at ray t_hit in
  let normal = Shape.normal shape point in
  let hit_front = V3.dot (Ray.direction ray) normal < 0.0 in
  let normal = if hit_front then normal else V3.Infix.( ~- ) normal in
  let tex_coord = Shape.tex_coord shape ~surface:point ~normal in
  let shader_space = Shader_space.create normal point in
  let m = Shape.material shape in
  let emit = Material.emit m tex_coord in
  let omega_i = Shader_space.omega_i shader_space ray in
  let do_scatter = Material.scatter m shader_space tex_coord ~omega_i ~hit_front in
  {shader_space; emit; do_scatter}

let scatter t u = t.do_scatter u
let emit t = t.emit
let shader_space t = t.shader_space
