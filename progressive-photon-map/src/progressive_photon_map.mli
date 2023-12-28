open Base

module Args : sig
  type t = private
    { width : int
    ; height : int
    ; iterations : int
    ; max_bounces : int
    ; photon_count : int
    ; alpha : float
    ; output : string
    }

  val parse : ?specs:(string * Stdlib.Arg.spec * string) list -> unit -> t
end

module Light : sig
  type t

  val create_point
    :  position:Path_tracer.P3.t
    -> color:Path_tracer.Color.t
    -> power:float
    -> t

  val create_spot
    :  position:Path_tracer.P3.t
    -> direction:Path_tracer.V3.t
    -> color:Path_tracer.Color.t
    -> power:float
    -> t
end

module Make (Scene : sig
  val bbox : Path_tracer.Bbox.t
  val camera : Path_tracer.Camera.t
  val lights : Light.t list
  val intersect : Path_tracer.Ray.t -> Path_tracer.Hit.t option
  val args : Args.t
end) : sig
  val go : output:string -> unit
end
