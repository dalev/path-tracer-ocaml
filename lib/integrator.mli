type t

val create :
  width:int ->
  height:int ->
  write_pixel:(x:int -> y:int -> r:float -> g:float -> b:float -> unit) ->
  t

val render : t -> Scene.t -> unit
