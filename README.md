A simple quasi-Monte-Carlo path tracer as described by [Peter Shirley's Ray Tracing articles](https://raytracing.github.io/).   For quasi-random sampling, I use the low-discrepancy sequence [described here](http://extremelearning.com.au/unreasonable-effectiveness-of-quasirandom-sequences/) by Martin Roberts.

![Our rendering of Shirley's test scene at 32 samples per pixel](https://github.com/dalev/path-tracer-ocaml/blob/main/shirley-spheres.png?raw=true)

It's implemented in [Ocaml](https://ocaml.org) and a little bit of [Rust](https://rust-lang.org).  On x86-64, the rust code uses AVX intrinsics to intersect a ray with four spheres at a time.

To build, run `dune build --release`.  To generate the above image, run `dune exec --release shirley_spheres -- --dimension=600,300 --samples-per-pixel=32 --max-ray-bounces=4`.  Increase `--samples-per-pixel` and/or `--max-ray-bounces` to render a higher quality image.  Pass the `--help` flag to see other options.
