type t
type image = (float, Bigarray.float64_elt, [ `Rgb ]) Bimage.Image.t

val create
  :  width:int
  -> height:int
  -> image:image
  -> samples_per_pixel:int
  -> max_bounces:int
  -> camera:Camera.t
  -> intersect:(Ray.t -> Hit.t option)
  -> background:(Ray.t -> Color.t)
  -> diffuse_plus_light:Pdf.t
  -> t

val render : update_progress:(int -> unit) -> t -> unit
