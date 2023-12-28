open Base

type p3 =
  { x : float
  ; y : float
  ; z : float
  }
[@@deriving sexp_of]

module V3 : sig
  type t =
    { x : float
    ; y : float
    ; z : float
    }

  val pp : Stdlib.Format.formatter -> t -> unit [@@ocaml.toplevel_printer]
  val create : x:float -> y:float -> z:float -> t
  val x : t -> float
  val y : t -> float
  val z : t -> float
  val min_coord : t -> float
  val max_coord : t -> float
  val zero : t
  val one : t
  val unit_x : t
  val unit_y : t
  val unit_z : t
  val axis : Axis.t -> t -> float
  val quadrance : t -> float
  val dot : t -> t -> float
  val cross : t -> t -> t
  val scale : t -> float -> t
  val lerp : float -> t -> t -> t
  val normalize : t -> t
  val of_float : float -> t
  val of_points : src:p3 -> tgt:p3 -> t
  val map : t -> f:(float -> float) -> t
  val map2 : t -> t -> f:(float -> float -> float) -> t

  (** [fma u v w] is like [Infix.(u * v + w)], but it uses fused-multiply-add ops. *)
  val fma : t -> t -> t -> t

  module Infix : sig
    val ( + ) : t -> t -> t
    val ( - ) : t -> t -> t
    val ( * ) : t -> t -> t
    val ( / ) : t -> t -> t
    val ( ~- ) : t -> t
  end
end

module P3 : sig
  type t = p3 =
    { x : float
    ; y : float
    ; z : float
    }
  [@@deriving sexp_of]

  val pp : Stdlib.Format.formatter -> t -> unit [@@ocaml.toplevel_printer]
  val create : x:float -> y:float -> z:float -> t
  val axis : Axis.t -> t -> float
  val x : t -> float
  val y : t -> float
  val z : t -> float
  val origin : t
  val translate : t -> V3.t -> t
  val to_v3 : t -> V3.t
  val of_v3 : V3.t -> t
  val scale : t -> float -> t
  val map2 : t -> t -> f:(float -> float -> float) -> t

  module Infix : sig
    val ( + ) : t -> t -> t
    val ( - ) : t -> t -> t
    val ( * ) : t -> t -> t
    val ( / ) : t -> t -> t
    val ( ~- ) : t -> t
  end
end
