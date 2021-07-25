open Base

type 'a t =
  { base : 'a Array.t
  ; offset : int
  ; length : int
  }

let create base = { base; offset = 0; length = Array.length base }
let length t = t.length
let base_index t index = t.offset + index
let get t index = t.base.(base_index t index)
let to_array_map t ~f = Array.init t.length ~f:(fun i -> f (get t i))
let to_array t = to_array_map t ~f:Fn.id

let fold_from t ~init ~f ~offset =
  let rec loop acc i = if i < t.length then loop (f acc (get t i)) (i + 1) else acc in
  loop init offset
;;

let fold = fold_from ~offset:0

let iter t ~f =
  for i = t.offset to t.offset + t.length - 1 do
    f t.base.(i)
  done
;;

let map_reduce t ~transform ~combine =
  let init = transform (get t 0) in
  fold_from t ~init ~offset:1 ~f:(fun acc item -> combine acc (transform item))
;;

let split_at t i =
  assert (i < t.length);
  let lhs = { t with length = i } in
  let rhs = { t with offset = t.offset + i; length = t.length - i } in
  lhs, rhs
;;

let partition_in_place t ~on_lhs =
  let i = ref 0 in
  let j = ref (length t - 1) in
  while !i < !j do
    while on_lhs (get t !i) do
      Int.incr i
    done;
    while not @@ on_lhs (get t !j) do
      Int.decr j
    done;
    if !i < !j then Array.swap t.base (base_index t !i) (base_index t !j)
  done;
  split_at t !i
;;
