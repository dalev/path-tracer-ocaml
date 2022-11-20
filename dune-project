(lang dune 3.0)

(name path_tracer)

(source
 (github dalev/path-tracer-ocaml))

(authors "Dale Vaillancourt")

(maintainers "dalev@alum.wpi.edu")

(package
 (name path_tracer)
 (synopsis "library for the geometry-agnostic portion")
 (depends
  (time_now
   (>= v0.15.0))
  (core
   (>= v0.15.1))
  (stdio
   (>= v0.15.0))
  (progress
   (>= 0.2.1))
  (ppx_base
   (>= v0.15.1))
  (bimage-unix
   (>= 0.6.0))
  (bimage
   (>= 0.6.0))
  (alcotest
   (and
    :with-test
    (>= 1.6)))
  (base
   (>= 0.15.1))
  (ocaml
   (>= 5.0.0~beta1))))

(package
 (name filter_kernel)
 (synopsis "filter kernel")
 (depends
  (base
   (>= v0.15.1))))

(package
 (name low_discrepancy_sequence)
 (synopsis "a low-discrepancy point sequence in N-dimensions")
 (depends
  (alcotest
   (and
    :with-test
    (>= 1.5)))
  (ocaml
   (>= 5.0.0~beta1))))

(package
 (name ply_format)
 (synopsis "parse .ply files (stanford triangle format)")
 (depends
  (alcotest
   (and
    (>= 1.6.0)
    :with-test))
  (core_kernel
   (>= v0.15.0))
  (ppx_sexp_message
   (>= v0.15.0))
  (ppx_base
   (>= v0.15.0))
  (parsexp
   (>= v0.15.0))
  (bigstringaf
   (>= 0.9.0))
  (base_bigstring
   (>= v0.15.0))
  (base
   (>= v0.15.1))
  (ocaml
   (>= 5.0.0~beta1))))

(generate_opam_files true)
