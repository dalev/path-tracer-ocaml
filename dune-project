(lang dune 2.9)

(name path_tracer)

(package
 (name path_tracer)
 (depends
  (base
   (>= 0.14.1))
  (lwt
   (>= 5.4.1))
  (ocaml
   (>= 4.12))))

(package
 (name low_discrepancy_sequence)
 (depends
  (ocaml
   (>= 4.12))))

(generate_opam_files true)
