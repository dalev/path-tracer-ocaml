type t

val create : Camera.t -> Shape.t list -> background:(Ray.t -> Color.t) -> t
val camera_ray : t -> float -> float -> Ray.t
val background : t -> Ray.t -> Color.t
val intersect : t -> Ray.t -> Hit.t option
val diffuse_plus_light_pdf : t -> Pdf.t
