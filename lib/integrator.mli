type t

val create :
  width:int ->
  height:int ->
  write_pixel:(x:int -> y:int -> r:float -> g:float -> b:float -> unit) ->
  samples_per_pixel:int ->
  max_bounces:int ->
  t

val render : ?update_progress:(float -> unit) -> t -> Scene.t -> unit
