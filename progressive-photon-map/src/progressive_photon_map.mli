open Base

module Point_light : sig
  type t

  val create
    :  position:Path_tracer.Affine.p3
    -> power:float
    -> color:Path_tracer.Color.t
    -> t
end

module Make (Scene : sig
  val bbox : Path_tracer.Bbox.t
  val camera : Path_tracer.Camera.t
  val point_lights : Point_light.t list
  val intersect : Path_tracer.Ray.t -> Path_tracer.Hit.t option
  val width : int
  val height : int
  val max_bounces : int
  val num_iterations : int
  val photon_count : int
  val alpha : float
end) : sig
  val go : Domainslib.Task.pool -> string -> unit
end
