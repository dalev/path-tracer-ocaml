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

#[derive(ocaml::FromValue)]
struct Coords<'a> {
    xs: ocaml::Array<'a, ocaml::Float>,
    ys: ocaml::Array<'a, ocaml::Float>,
    zs: ocaml::Array<'a, ocaml::Float>,
    rs: ocaml::Array<'a, ocaml::Float>,
}

#[derive(ocaml::FromValue)]
struct Ray<'a> {
    orig: ocaml::Array<'a, ocaml::Float>,
    dir: ocaml::Array<'a, ocaml::Float>,
    dir_inv: ocaml::Array<'a, ocaml::Float>,
}

#[ocaml::func]
pub fn spheres_intersect(
    c: Coords,
    t_min: ocaml::Float,
    t_max: ocaml::Float,
    ray: Ray,
) -> Option<(ocaml::Float, ocaml::Int)> {
    let xs = &c.xs;
    let ys = &c.ys;
    let zs = &c.zs;
    let rs = &c.rs;
    // if !xs.is_double_array() {
    //     panic!("xs is double array")
    // }
    // if !ys.is_double_array() {
    //     panic!("ys is double array")
    // }
    // if !zs.is_double_array() {
    //     panic!("zs is double array")
    // }
    // if !rs.is_double_array() {
    //     panic!("rs is double array")
    // }
    // if !ray.orig.is_double_array() {
    //     panic!("ray_orig is double array, len = {}", ray.orig.len())
    // }
    // if !ray.dir.is_double_array() {
    //     panic!("ray_dir is double array")
    // }
    let ray_orig = V3::from(ray.orig);
    let ray_dir = V3::from(ray.dir);
    let mut t_max = t_max;
    let mut found: Option<usize> = None;
    let len = xs.len();
    for i in 0..len {
        let c = unsafe {
            V3 {
                x: xs.get_double_unchecked(i),
                y: ys.get_double_unchecked(i),
                z: zs.get_double_unchecked(i),
            }
        };
        let r = unsafe { rs.get_double_unchecked(i) };
        match intersect(c, r, ray_orig, ray_dir, t_min, t_max) {
            None => (),
            Some(t_hit) => {
                t_max = t_hit;
                found = Some(i)
            }
        }
    }
    match found {
        None => None,
        Some(i) => Some((t_max, i as isize)),
    }
}

fn intersect(center: V3, radius: f64, o: V3, d: V3, t_min: f64, t_max: f64) -> Option<f64> {
    let r2 = radius * radius;
    let f = center - o;
    let bp = f.dot(d);
    let a = d.quadrance();
    let discrim = r2 - (d.scale(bp / a) - f).quadrance();
    if discrim < 0.0 {
        None
    } else {
        let sign_bp = bp.signum();
        let q = bp + (sign_bp * (a * discrim).sqrt());
        let c = f.quadrance() - r2;
        let t_hit = if c > 0.0 { c / q } else { q / a };
        if t_min <= t_hit && t_hit <= t_max {
            Some(t_hit)
        } else {
            None
        }
    }
}
