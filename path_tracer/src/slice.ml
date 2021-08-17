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
let unsafe_get t index = Array.unsafe_get t.base (base_index t index)
let to_array_map t ~f = Array.init t.length ~f:(fun i -> f (get t i))
let to_array t = to_array_map t ~f:Fn.id

let fold_from t ~init ~f ~offset =
  let last_index = t.length - 1 in
  assert (base_index t (Int.max offset last_index) < Array.length t.base);
  let acc = ref init in
  for i = offset to t.length - 1 do
    acc := f !acc (unsafe_get t i)
  done;
  !acc
;;

let fold = fold_from ~offset:0

let iter t ~f =
  let last_index = t.offset + t.length - 1 in
  assert (Int.max t.offset last_index < Array.length t.base);
  for i = t.offset to last_index do
    f (Array.unsafe_get t.base i)
  done
;;

let map_reduce t ~transform ~combine =
  let init = transform (get t 0) in
  if t.length > 1
  then fold_from t ~init ~offset:1 ~f:(fun acc item -> combine acc (transform item))
  else init
;;

let reduce_exn t ~f = map_reduce t ~transform:Fn.id ~combine:f

let split_at t i =
  if i >= t.length
  then raise_s [%message "split_at" ~index:(i : int) ~length:(t.length : int)];
  assert (i < t.length);
  let lhs = { t with length = i } in
  let rhs = { t with offset = t.offset + i; length = t.length - i } in
  lhs, rhs
;;

let tail t = snd @@ (split_at [@inlined]) t 1

let partition_in_place t ~on_lhs =
  let i = ref 0 in
  let j = ref (length t - 1) in
  while !i < !j do
    while on_lhs (get t !i) && !i < !j do
      Int.incr i
    done;
    while (not @@ on_lhs (get t !j)) && !j >= 0 do
      Int.decr j
    done;
    if !i < !j then Array.swap t.base (base_index t !i) (base_index t !j)
  done;
  split_at t !i
;;
