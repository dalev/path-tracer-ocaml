open Base

module Args : sig
  type t =
    { width : int
    ; height : int
    ; samples_per_pixel : int
    ; output : string
    ; no_progress : bool
    ; max_bounces : int
    }

  val parse : ?specs:(string * Caml.Arg.spec * string) list -> unit -> t
end

val with_elapsed_time : (unit -> 'a) -> Int63.t * 'a

module Make (Scene : sig
  val camera : Path_tracer.Camera.t
  val intersect : Path_tracer.Ray.t -> Path_tracer.Hit.t option
  val background : Path_tracer.Ray.t -> Path_tracer.Color.t
end) : sig
  val run : Args.t -> unit
end
