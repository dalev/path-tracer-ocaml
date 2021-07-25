A simple quasi-Monte-Carlo path tracer as described by [Peter Shirley's Ray Tracing articles](https://raytracing.github.io/).   For quasi-random sampling, I use the low-discrepancy sequence [described here](http://extremelearning.com.au/unreasonable-effectiveness-of-quasirandom-sequences/) by Martin Roberts.

![Our rendering of Shirley's test scene at 32 samples per pixel](https://github.com/dalev/path-tracer-ocaml/blob/main/shirley-spheres.png?raw=true)

It's implemented in [Ocaml](https://ocaml.org) and a little bit of [Rust](https://rust-lang.org).  The Rust portion only works on x86-64:  it uses AVX intrinsics to intersect a ray with four spheres at a time.

To build, you'll need Ocaml and Rust installed.  Then `opam install` these packages:
- dune
- base
- stdio
- [bimage](https://opam.ocaml.org/packages/bimage)
- [bimage-io](https://opam.ocaml.org/packages/bimage-io) (uses an external package `openimageio`)
- lwt

To build, run `dune build --release`.  Then generate the above image by running `dune exec shirley_spheres -- -samples-per-pixel 32 -max-bounces 4 -width 600`.  Increase `-samples-per-pixel` to render a higher quality image.  Pass the `-help` flag to see other options. 
