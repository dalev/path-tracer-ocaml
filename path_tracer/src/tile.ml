open! Base

type t =
  { row : int
  ; col : int
  ; width : int
  ; height : int
  }

let area t = t.width * t.height

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

let iter t ~f =
  let x0 = t.col in
  let y0 = t.row in
  for y = y0 to y0 + t.height - 1 do
    for x = x0 to x0 + t.width - 1 do
      f ~x ~y
    done
  done
;;
