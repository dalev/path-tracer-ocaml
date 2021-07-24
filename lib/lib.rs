use core::slice;
use ocaml::Raw;
use packed_simd::*;

#[ocaml::func]
pub fn hello_world() -> &'static str {
    "hello, world!"
}

#[derive(Clone, Copy)]
struct V3 {
    x: f64,
    y: f64,
    z: f64,
}

impl std::convert::From<&[f64]> for V3 {
    fn from(s: &[f64]) -> V3 {
        V3 {
            x: s[0],
            y: s[1],
            z: s[2],
        }
    }
}

impl std::ops::Sub for V3 {
    type Output = Self;
    fn sub(self, src: V3) -> Self::Output {
        V3 {
            x: self.x - src.x,
            y: self.y - src.y,
            z: self.z - src.z,
        }
    }
}

impl V3 {
    fn dot(self, other: V3) -> f64 {
        self.x * other.x + self.y * other.y + self.z * other.z
    }

    fn quadrance(self) -> f64 {
        self.dot(self)
    }
}

unsafe fn make_slice<'a, T>(p: *const T) -> &'a [T] {
    let len = ocaml_sys::caml_array_length(p as isize);
    slice::from_raw_parts(p, len)
}

#[no_mangle]
pub extern "C" fn spheres_intersect_native(
    c: Raw,
    t_min: f64,
    t_max: f64,
    ray: Raw,
    t_hit_ref: Raw,
) -> Raw {
    let c = unsafe { slice::from_raw_parts(c.0 as *const *const f64, 4) };
    let ray = unsafe { slice::from_raw_parts(ray.0 as *const *const f64, 2) };
    let xs = unsafe { make_slice(c[0]) };
    let ys = unsafe { make_slice(c[1]) };
    let zs = unsafe { make_slice(c[2]) };
    let rs = unsafe { make_slice(c[3]) };
    let o = V3::from(unsafe { make_slice(ray[0]) });
    let d = V3::from(unsafe { make_slice(ray[1]) });
    let (t_found, found) = spheres_intersect_aux(xs, ys, zs, rs, o, d, t_min, t_max);
    unsafe {
        *(t_hit_ref.0 as *mut f64) = t_found;
    }
    unsafe { ocaml::Raw(ocaml_sys::val_int(found)) }
}

const LEAF_SIZE: usize = 32;

type LeafF64s = [f64; LEAF_SIZE];

fn sub_const(dst: &mut [f64], src: &[f64], rhs: f64) {
    for (d, s) in dst.iter_mut().zip(src) {
        *d = s - rhs
    }
}

fn vmul_add(dst: &mut [f64], src: &[f64], scalar: f64) {
    for (d, f) in dst.iter_mut().zip(src) {
        *d = f.mul_add(scalar, *d)
    }
}

fn sq(x: f64) -> f64 {
    x * x
}

fn spheres_intersect_aux(
    xs: &[f64],
    ys: &[f64],
    zs: &[f64],
    rs: &[f64],
    o: V3,
    d: V3,
    t_min: f64,
    t_max: f64,
) -> (f64, isize) {
    let len = xs.len();
    // assert_eq!(len, LEAF_SIZE);
    let mut r2s: LeafF64s = [0.0; LEAF_SIZE];
    let mut fxs: LeafF64s = [0.0; LEAF_SIZE];
    let mut fys: LeafF64s = [0.0; LEAF_SIZE];
    let mut fzs: LeafF64s = [0.0; LEAF_SIZE];
    for (dst, src) in r2s.iter_mut().zip(rs) {
        *dst = src * src;
    }
    let d_quadrance = d.quadrance();
    sub_const(&mut fxs, &xs, o.x);
    sub_const(&mut fys, &ys, o.y);
    sub_const(&mut fzs, &zs, o.z);
    let mut bps: LeafF64s = [0.0; LEAF_SIZE];
    vmul_add(&mut bps, &fxs[0..len], d.x);
    vmul_add(&mut bps, &fys[0..len], d.y);
    vmul_add(&mut bps, &fzs[0..len], d.z);
    // let discrim = r2 - (d.scale(bp / a) - f).quadrance();
    let mut discrims = r2s;
    for (dst, (bp, (fx, (fy, fz)))) in discrims[0..len]
        .iter_mut()
        .zip(bps.iter().zip(fxs.iter().zip(fys.iter().zip(fzs))))
    {
        let s = bp / d_quadrance; // a == d_quadrance
        let q = sq(d.x * s - fx) + sq(d.y * s - fy) + sq(d.z * s - fz);
        *dst -= q
    }
    let mut t_max = t_max;
    let mut found: isize = -1;
    let mut cs = [0.0; LEAF_SIZE];
    for (c, (&fx, (&fy, (&fz, r2)))) in cs[0..len]
        .iter_mut()
        .zip(fxs.iter().zip(fys.iter().zip(fzs.iter().zip(r2s))))
    {
        *c = sq(fx) + sq(fy) + sq(fz) - r2
    }
    for (i, (&c, (&bp, discrim))) in cs
        .iter()
        .zip(bps.iter().zip(discrims))
        .enumerate()
        .take(len)
    {
        let t_hit = intersect(discrim, d_quadrance, bp, c, t_min, t_max);
        if !f64::is_nan(t_hit) {
            t_max = t_hit;
            found = i as isize;
        }
    }
    (t_max, found)
}

fn intersect(discrim: f64, a: f64, bp: f64, c: f64, t_min: f64, t_max: f64) -> f64 {
    if discrim < 0.0 {
        std::f64::NAN
    } else {
        let sign_bp = bp.signum();
        let q = bp + (sign_bp * (a * discrim).sqrt());
        let t_hit = if c > 0.0 { c / q } else { q / a };
        if t_min <= t_hit && t_hit <= t_max {
            t_hit
        } else {
            std::f64::NAN
        }
    }
}
