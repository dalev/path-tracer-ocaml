open Base

type t =
  | Lambertian of Texture.t
  | Metal of Texture.t
  | Dielectric of {index: float; index_inv: float}

let lambertian tex = Lambertian tex
let metal tex = Metal tex
let dielectric index = Dielectric {index; index_inv= 1.0 /. index}
let glass = dielectric 1.5

let schlick_reflectance cos_theta index =
  let r0 = Float.square @@ ((1.0 -. index) /. (1.0 +. index)) in
  r0 +. ((1.0 -. r0) *. ((1.0 -. cos_theta) **. 5.0))

let scatter t ss tex_coord ~omega_i ~hit_front u =
  match t with
  | Lambertian tex -> Scatter.Diffuse (Texture.eval tex tex_coord)
  | Metal tex ->
      let omega_r = Shader_space.reflect ss omega_i in
      let _, _, z = V3.coords omega_r in
      if Float.( <= ) z 0.0 then
        Scatter.Absorb
      else
        let attenuation =
          let a = Texture.eval tex tex_coord in
          let s = (1.0 -. V3.z omega_i) **. 5.0 in
          let c = Color.scale Color.Infix.(Color.white - a) s in
          Color.Infix.(a + c) in
        let reflected = Shader_space.rotate_inv ss omega_r in
        Scatter.Specular (reflected, attenuation)
  | Dielectric {index; index_inv} ->
      let open Float.O in
      let wi_z = V3.z omega_i in
      let c = Float.clamp_exn wi_z ~min:0.0 ~max:1.0 in
      let s = Float.sqrt (1.0 - Float.square c) in
      let refract_ratio = if hit_front then index_inv else index in
      let wo =
        if refract_ratio * s > 1.0 || schlick_reflectance c refract_ratio > u then
          Shader_space.reflect ss omega_i
        else
          Shader_space.refract ss omega_i refract_ratio in
      Scatter.Specular (Shader_space.rotate_inv ss wo, Color.white)

let emit (_ : t) (_ : Texture.Coord.t) = Color.black
