open! Base
open Path_tracer

let unit_bbox = Bbox.create ~min:P3.origin ~max:(P3.create ~x:1.0 ~y:1.0 ~z:1.0)

let ray_hit, ray_miss =
  let o = P3.create ~x:(-5.0) ~y:0.5 ~z:0.5 in
  let hit = V3.unit_x in
  let miss = V3.unit_y in
  Ray.create o hit, Ray.create o miss
;;

let test_case name f = Alcotest.test_case name `Quick f

module Int2 = struct
  module T = struct
    type t = int * int [@@deriving compare, hash, sexp_of]
  end

  include T
  include Comparator.Make (T)

  let pp = Fmt.(parens @@ pair ~sep:comma int int)
  let set_pp = Fmt.iter ~sep:Fmt.comma (fun elt_pp set -> Set.iter set ~f:elt_pp) pp

  let check_set msg (s : (t, comparator_witness) Set.t) t =
    Alcotest.(check (testable set_pp Set.equal)) msg s t
  ;;
end

let check_true msg actual = Alcotest.(check bool) msg true actual
let check_false msg actual = Alcotest.(check bool) msg false actual

let tile_tests =
  ( "Tile"
  , let width = 10
    and height = 5
    and max_area = 7 in
    let tile = Tile.create ~width ~height in
    let tiles = Tile.split tile ~max_area in
    [ test_case "area after split" (fun () ->
        List.iter tiles ~f:(fun t ->
          let a = Tile.area t in
          let msg = Fmt.str "%d <= %d" a max_area in
          check_true msg (a <= max_area)))
    ; test_case "iter produces all points" (fun () ->
        List.iter (tile :: tiles) ~f:(fun t ->
          let expect_local =
            let xs = List.init (Tile.width t) ~f:Fn.id in
            let ys = List.init (Tile.height t) ~f:Fn.id in
            Set.of_list (module Int2) @@ List.cartesian_product xs ys
          in
          let expect_global =
            Set.map
              (module Int2)
              expect_local
              ~f:(fun (x, y) -> x + t.Tile.col, y + t.Tile.row)
          in
          let mt = Set.empty (module Int2) in
          let locals, globals =
            Tile.fold
              t
              ~init:(mt, mt)
              ~f:(fun ~local_x ~local_y ~global_x ~global_y (locals, globals) ->
              Set.add locals (local_x, local_y), Set.add globals (global_x, global_y))
          in
          Int2.check_set "locals" expect_local locals;
          Int2.check_set "globals" expect_global globals))
    ] )
;;

let film_tile_tests =
  ( "Film_tile"
  , let width = 7
    and height = 8
    and row = 2
    and col = 3
    and pixel_radius = 1 in
    let t = Tile.create' ~width ~height ~row ~col in
    let k = Filter_kernel.Binomial.create ~order:5 ~pixel_radius in
    [ test_case "global coords" (fun () ->
        let ft = Film_tile.create t ~filter_kernel:k in
        let actual = ref (Set.empty (module Int2)) in
        Film_tile.iter ft ~f:(fun ~global_x ~global_y (_ : Color.t) ->
          actual := Set.add !actual (global_x, global_y));
        let actual = !actual in
        let expect =
          let xs =
            List.init (width + (2 * pixel_radius)) ~f:(fun x -> x + col - pixel_radius)
          and ys =
            List.init (height + (2 * pixel_radius)) ~f:(fun y -> y + row - pixel_radius)
          in
          Set.of_list (module Int2) @@ List.cartesian_product xs ys
        in
        Int2.check_set "coords" expect actual)
    ; test_case "write_pixel locus" (fun () ->
        let ft = Film_tile.create t ~filter_kernel:k in
        Film_tile.write_pixel ft ~x:0 ~y:0 Color.white;
        Film_tile.iter ft ~f:(fun ~global_x ~global_y c ->
          if col - pixel_radius <= global_x
             && global_x <= col + pixel_radius
             && row - pixel_radius <= global_y
             && global_y <= row + pixel_radius
          then begin
            let open Float.O in
            let r, g, b = Color.to_rgb c in
            check_true "r non-zero" (r > 0.0);
            check_true "g non-zero" (g > 0.0);
            check_true "b non-zero" (b > 0.0)
          end
          else begin
            let check_zero msg = Alcotest.(check (float 0.0) msg 0.0) in
            let r, g, b = Color.to_rgb c in
            check_zero "r zero" r;
            check_zero "g zero" g;
            check_zero "b zero" b
          end))
    ] )
;;

let bbox_tests =
  ( "Bbox.is_hit"
  , [ test_case "ray towards / t_max > 5" (fun () ->
        check_true "..." (Bbox.is_hit unit_bbox ray_hit ~t_min:0.0 ~t_max:5.01))
    ; test_case "ray towards / t_max < 5" (fun () ->
        check_false "..." (Bbox.is_hit unit_bbox ray_hit ~t_min:0.0 ~t_max:4.99))
    ; test_case "ray away" (fun () ->
        check_false "..." (Bbox.is_hit unit_bbox ray_miss ~t_min:0.0 ~t_max:1000.0))
    ] )
;;

let shader_space_tests =
  ( "shader_space"
  , [ test_case "unit_square_to_hemisphere is normalized" (fun () ->
        for _i = 0 to 100 do
          let u = Random.float 1.0
          and v = Random.float 1.0 in
          let w = Shader_space.unit_square_to_hemisphere u v in
          Alcotest.(check (float 1e-6) "within 1e-6") 1.0 (V3.quadrance w)
        done)
    ] )
;;

let () =
  Alcotest.run
    "path_tracer"
    [ bbox_tests; tile_tests; film_tile_tests; shader_space_tests ]
;;
