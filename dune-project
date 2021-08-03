(lang dune 2.9)

(name path_tracer)

(source
 (github dalev/path-tracer-ocaml))

(authors "Dale Vaillancourt")

(package
 (name path_tracer)
 (depends
  (stdio
   (>= v0.14.0))
  (progress
   (>= 0.2.1))
  (ppx_base
   (>= v0.14.0))
  (bimage-io
   (>= 0.4.0))
  (bimage
   (>= 0.4.0))
  (alcotest
   (and
    :with-test
    (>= "1.4")))
  (base
   (>= 0.14.1))
  (ocaml
   (>= 4.12))))

(package
 (name low_discrepancy_sequence)
 (depends
  (alcotest
   (and
    :with-test
    (>= "1.4")))
  (ocaml
   (>= 4.12))))

(generate_opam_files true)
