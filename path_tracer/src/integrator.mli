type t

val create
  :  width:int
  -> height:int
  -> write_pixel:(x:int -> y:int -> Color.t -> unit)
  -> samples_per_pixel:int
  -> max_bounces:int
  -> camera:Camera.t
  -> intersect:(Ray.t -> Hit.t option)
  -> background:(Ray.t -> Color.t)
  -> diffuse_plus_light:Pdf.t
  -> t

val count_tiles : t -> int
val render : update_progress:(unit -> unit) -> t -> unit
