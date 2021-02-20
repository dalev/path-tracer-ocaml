A simple quasi-Monte-Carlo path tracer as described by [Peter Shirley's Ray Tracing articles](https://raytracing.github.io/).   For quasi-random sampling, I use the low-discrepancy sequence [described here](http://extremelearning.com.au/unreasonable-effectiveness-of-quasirandom-sequences/) by Martin Roberts.

![Our rendering of Shirley's test scene at 32 samples per pixel](https://github.com/dalev/path-tracer-ocaml/blob/main/shirley-spheres.png?raw=true)

It's implemented in [Ocaml](https://ocaml.org).

To build, `opam install` these libraries:
- base
- stdio
- bimage
- bimage-io
- domainslib

Then generate the sample image by running, e.g.  `dune exec path_tracer -- -samples-per-pixel 4`.  Increase `-samples-per-pixel` to render a higher quality image.  (E.g, the image above was rendered with 32 samples per pixel.)  Pass the `-help` flag to see other options. 
