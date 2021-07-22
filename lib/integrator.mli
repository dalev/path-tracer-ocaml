type t

val create :
     width:int
  -> height:int
  -> write_pixel:(x:int -> y:int -> Color.t -> unit)
  -> samples_per_pixel:int
  -> max_bounces:int
  -> trace_path:
       (cx:float -> cy:float -> int -> Low_discrepancy_sequence.Sample.t -> Color.t)
  -> t

val render : ?update_progress:(float -> unit Lwt.t) -> t -> unit Lwt.t
