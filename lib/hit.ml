type t =
  { t_hit: float
  ; point: P3.t
  ; shape: Shape.t
  ; shader_space: Shader_space.t
  ; tex_coord: Texture.Coord.t
  ; ray: Ray.t
  ; hit_front: bool }

let create ~t_hit shape ray =
  let point = Ray.point_at ray t_hit in
  let normal = Shape.normal shape point in
  let hit_front = V3.dot (Ray.direction ray) normal < 0.0 in
  let normal = if hit_front then normal else V3.Infix.( ~- ) normal in
  let tex_coord = Shape.tex_coord shape ~surface:point ~normal in
  let shader_space = Shader_space.create normal point in
  {t_hit; point; shape; shader_space; tex_coord; ray; hit_front}

let t_hit t = t.t_hit

let omega_i t =
  let v = V3.Infix.( ~- ) @@ V3.normalize @@ Ray.direction t.ray in
  Shader_space.rotate t.shader_space v

let point t = t.point

let scatter t u =
  let m = Shape.material t.shape in
  let hit_front = t.hit_front in
  Material.scatter m t.shader_space t.tex_coord ~omega_i:(omega_i t) ~hit_front u

let material t = Shape.material t.shape
let emit t = Material.emit (material t) t.tex_coord
let shader_space t = t.shader_space
