open! Base
module FArray = Caml.Float.ArrayLabels

let ( .%{} ) = FArray.get

type t =
  { dim : int
  ; pixel_radius : int
  ; data : FArray.t
  }

let iter t ~f =
  let r = t.pixel_radius in
  let i = ref 0 in
  for dy = -r to r do
    for dx = -r to r do
      let weight = t.data.%{!i} in
      Int.incr i;
      f ~dx ~dy weight
    done
  done
;;

module Binomial = struct
  let rec pow_falling n k = if k = 0 then 1 else n * pow_falling (n - 1) (k - 1)
  let factorial n = pow_falling n n
  let binomial n k = pow_falling n k / factorial k

  let pp f { dim; pixel_radius = _; data } =
    let fmt_row =
      Fmt.append (Fmt.hbox @@ Fmt.brackets @@ Fmt.list ~sep:Fmt.sp Fmt.float) Fmt.cut
    in
    for i = 0 to dim - 1 do
      fmt_row f (FArray.to_list (FArray.sub data ~pos:(i * dim) ~len:dim))
    done
  ;;

  let outer_product v w =
    let order = FArray.length v in
    assert (order = FArray.length w);
    FArray.init (order * order) ~f:(fun j ->
        let r = j / order
        and c = j % order in
        v.%{r} *. w.%{c})
  ;;

  let create ~order ~pixel_radius =
    assert (pixel_radius >= 0);
    let f_width = 1 + (2 * pixel_radius) in
    let ratio = Num.(num_of_int order // num_of_int f_width) in
    let coeffs = Array.init order ~f:(fun k -> binomial (order - 1) k) in
    let w =
      let open Num in
      let one = num_of_int 1 in
      let zero = num_of_int 0 in
      let fractional_part n =
        let open Num in
        mod_num n one
      in
      FArray.init f_width ~f:(fun i ->
          let i' = num_of_int i */ ratio in
          let j' = i' +/ ratio in
          let beg' = floor_num i' in
          let beg = int_of_num beg' in
          let end_ = ceiling_num j' in
          let len = int_of_num end_ - beg in
          let sum = ref zero in
          for k = 0 to len - 1 do
            let weight : Num.num =
              if k = 0
              then one -/ fractional_part i'
              else if k = len - 1
              then one -/ (end_ -/ j')
              else Num.num_of_int 1
            in
            sum := !sum +/ (weight */ num_of_int coeffs.(k + beg))
          done;
          float_of_num !sum)
    in
    let total_weight = FArray.fold_left ~f:( +. ) ~init:0.0 w in
    let w = FArray.map ~f:(fun f -> f /. total_weight) w in
    { dim = f_width; pixel_radius; data = outer_product w w }
  ;;
end