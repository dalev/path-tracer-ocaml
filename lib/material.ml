type t = Lambertian of Texture.t

let lambertian tex = Lambertian tex

let scatter t tex_coord =
  match t with Lambertian tex -> Scatter.Diffuse (Texture.eval tex tex_coord)
