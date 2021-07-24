use core::slice;
use ocaml::Raw;

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

impl std::convert::From<ocaml::Array<'_, f64>> for V3 {
    fn from(a: ocaml::Array<f64>) -> V3 {
        unsafe {
            let x = a.get_double_unchecked(0);
            let y = a.get_double_unchecked(1);
            let z = a.get_double_unchecked(2);
            V3 { x, y, z }
        }
    }
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

    fn scale(self, s: f64) -> V3 {
        V3 {
            x: self.x * s,
            y: self.y * s,
            z: self.z * s,
        }
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
    let mut t_max = t_max;
    let mut found: isize = -1;
    for (i, (&x, (&y, (&z, &r)))) in xs.iter().zip(ys.iter().zip(zs.iter().zip(rs))).enumerate() {
        let c = V3 { x, y, z };
        let t_hit = intersect(c, r, o, d, t_min, t_max);
        if !f64::is_nan(t_hit) {
            t_max = t_hit;
            found = i as isize;
        }
    }
    unsafe {
        *(t_hit_ref.0 as *mut f64) = t_max;
    }
    unsafe { ocaml::Raw(ocaml_sys::val_int(found)) }
}

fn intersect(center: V3, radius: f64, o: V3, d: V3, t_min: f64, t_max: f64) -> f64 {
    let r2 = radius * radius;
    let f = center - o;
    let bp = f.dot(d);
    let a = d.quadrance();
    let discrim = r2 - (d.scale(bp / a) - f).quadrance();
    if discrim < 0.0 {
        std::f64::NAN
    } else {
        let sign_bp = bp.signum();
        let q = bp + (sign_bp * (a * discrim).sqrt());
        let c = f.quadrance() - r2;
        let t_hit = if c > 0.0 { c / q } else { q / a };
        if t_min <= t_hit && t_hit <= t_max {
            t_hit
        } else {
            std::f64::NAN
        }
    }
}
