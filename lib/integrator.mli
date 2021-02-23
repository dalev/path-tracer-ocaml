type t

val create :
     width:int
  -> height:int
  -> write_pixel:(x:int -> y:int -> Color.t -> unit)
  -> samples_per_pixel:int
  -> max_bounces:int
  -> t

val render : ?update_progress:(float -> unit Lwt.t) -> t -> Scene.t -> unit Lwt.t
