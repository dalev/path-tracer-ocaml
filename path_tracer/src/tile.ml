open! Base

type t =
  { row : int
  ; col : int
  ; width : int
  ; height : int
  }

let width t = t.width
let height t = t.height
let area t = width t * height t

let split_width t =
  let half_w = t.width / 2 in
  let lhs = { t with width = half_w } in
  let rhs = { t with col = t.col + half_w; width = t.width - half_w } in
  lhs, rhs
;;

let split_height t =
  let half_h = t.height / 2 in
  let lhs = { t with height = half_h } in
  let rhs = { t with row = t.row + half_h; height = t.height - half_h } in
  lhs, rhs
;;

let split_once t = if t.width > t.height then split_width t else split_height t

let split t ~max_area =
  let rec loop t =
    if area t <= max_area
    then [ t ]
    else (
      let lhs, rhs = split_once t in
      loop lhs @ loop rhs)
  in
  loop t
;;

let create ~width ~height = { row = 0; col = 0; width; height }
let create' ~width ~height ~row ~col = { row; col; width; height }

let extend t ~radius =
  assert (radius > 0);
  { row = t.row - radius
  ; col = t.col - radius
  ; width = t.width + radius
  ; height = t.height + radius
  }
;;

let iter_local t ~f =
  for y = 0 to t.height - 1 do
    for x = 0 to t.width - 1 do
      f ~x ~y
    done
  done
;;

let iter_global t ~f =
  let x0 = t.col in
  let y0 = t.row in
  for y = y0 to y0 + t.height - 1 do
    for x = x0 to x0 + t.width - 1 do
      f ~x ~y
    done
  done
;;

let iter t ~f =
  for local_y = 0 to t.height - 1 do
    let global_y = local_y + t.row in
    for local_x = 0 to t.width - 1 do
      let global_x = local_x + t.col in
      f ~local_x ~local_y ~global_x ~global_y
    done
  done
;;

let fold t ~init ~f =
  let acc = ref init in
  iter t ~f:(fun ~local_x ~local_y ~global_x ~global_y ->
    acc := f ~local_x ~local_y ~global_x ~global_y !acc);
  !acc
;;
