A simple Monte-Carlo path tracer as described by [Peter Shirley's Ray Tracing](https://raytracing.github.io/) articles.

It's implemented in [Ocaml](https://ocaml.org) using the new experimental [multicore](https://github.com/ocaml-multicore/ocaml-multicore) branch.

To build, follow the [instructions](https://github.com/ocaml-multicore/multicore-opam#install-multicore-ocaml) for installing the experimental compiler.  Then, on that opam switch, `opam install` these libraries:
- base
- stdio
- bimage
- bimage-io
- domainslib

Then generate the sample image by running, e.g.  `dune exec path_tracer -- -max-threads 2 -samples-per-pixel`.  Pass the `-help` flag to see other options.  If you say `-max-threads 1`, then the renderer will avoid using the `domainslib` abstractions entirely (rather than spinning up a single `Domain` to do all the work).
