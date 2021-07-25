use core::slice;
use ocaml::Raw;

const LEAF_SIZE: usize = 16;

#[ocaml::func]
pub fn leaf_size() -> usize {
    LEAF_SIZE
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
    unsafe {
        let c = slice::from_raw_parts(c.0 as *const *const f64, 4);
        let ray = slice::from_raw_parts(ray.0 as *const *const f64, 2);
        let xs = make_slice(c[0]);
        let ys = make_slice(c[1]);
        let zs = make_slice(c[2]);
        let rs = make_slice(c[3]);
        let o = V3::from(make_slice(ray[0]));
        let d = V3::from(make_slice(ray[1]));
        let (t_found, found) = spheres_intersect_aux(xs, ys, zs, rs, o, d, t_min, t_max);
        *(t_hit_ref.0 as *mut f64) = t_found;
        ocaml::Raw(ocaml_sys::val_int(found))
    }
}

mod simd {
    #[cfg(target_arch = "x86_64")]
    use core::arch::x86_64::*;

    #[inline(always)]
    pub unsafe fn dot4(
        vx: __m256d,
        vy: __m256d,
        vz: __m256d,
        wx: __m256d,
        wy: __m256d,
        wz: __m256d,
    ) -> __m256d {
        _mm256_fmadd_pd(vx, wx, _mm256_fmadd_pd(vy, wy, _mm256_mul_pd(vz, wz)))
    }

    #[inline(always)]
    pub unsafe fn quadrance(vx: __m256d, vy: __m256d, vz: __m256d) -> __m256d {
        dot4(vx, vy, vz, vx, vy, vz)
    }
}

#[cfg(target_arch = "x86_64")]
unsafe fn spheres_intersect_aux(
    xs: &[f64],
    ys: &[f64],
    zs: &[f64],
    rs: &[f64],
    o: V3,
    d: V3,
    t_min: f64,
    t_max: f64,
) -> (f64, isize) {
    use core::arch::x86_64::*;
    let mut t_hits = [0.0; LEAF_SIZE];
    let d_quadrance = d.quadrance();
    let a = _mm256_set1_pd(d_quadrance);
    let ox = _mm256_set1_pd(o.x);
    let oy = _mm256_set1_pd(o.y);
    let oz = _mm256_set1_pd(o.z);
    let dx = _mm256_set1_pd(d.x);
    let dy = _mm256_set1_pd(d.y);
    let dz = _mm256_set1_pd(d.z);
    for ((((x, y), z), r), dst_t_hit) in xs
        .chunks_exact(4)
        .zip(ys.chunks_exact(4))
        .zip(zs.chunks_exact(4))
        .zip(rs.chunks_exact(4))
        .zip(t_hits.chunks_exact_mut(4))
    {
        let x = _mm256_loadu_pd(x.as_ptr());
        let y = _mm256_loadu_pd(y.as_ptr());
        let z = _mm256_loadu_pd(z.as_ptr());
        let r = _mm256_loadu_pd(r.as_ptr());
        // f = center - origin
        let fx = _mm256_sub_pd(x, ox);
        let fy = _mm256_sub_pd(y, oy);
        let fz = _mm256_sub_pd(z, oz);
        let r2 = _mm256_mul_pd(r, r);
        let c = _mm256_sub_pd(simd::quadrance(fx, fy, fz), r2);
        // bp = dot(f, d)
        let bp = simd::dot4(fx, fy, fz, dx, dy, dz);
        let bp_over_a = _mm256_div_pd(bp, a);
        // let w = d.scale(bp / a) - f
        let wx = _mm256_fmsub_pd(dx, bp_over_a, fx);
        let wy = _mm256_fmsub_pd(dy, bp_over_a, fy);
        let wz = _mm256_fmsub_pd(dz, bp_over_a, fz);
        let wq = simd::quadrance(wx, wy, wz);
        let dis = _mm256_sub_pd(r2, wq);

        // let q = bp + (sign_bp * (a * discrim).sqrt());
        let q_rhs = _mm256_sqrt_pd(_mm256_mul_pd(a, dis));
        let q = _mm256_blendv_pd(_mm256_add_pd(bp, q_rhs), _mm256_sub_pd(bp, q_rhs), bp);

        let c_div_q = _mm256_div_pd(c, q);
        let q_div_a = _mm256_div_pd(q, a);
        let t_hit = _mm256_blendv_pd(c_div_q, q_div_a, c);
        let outside_range = _mm256_or_pd(
            _mm256_cmp_pd(t_hit, _mm256_set1_pd(t_min), _CMP_LT_OQ),
            _mm256_cmp_pd(t_hit, _mm256_set1_pd(t_max), _CMP_GT_OQ),
        );
        let t_hit = _mm256_blendv_pd(
            t_hit,
            _mm256_set1_pd(f64::NAN),
            _mm256_or_pd(dis, outside_range),
        );
        _mm256_storeu_pd(dst_t_hit.as_mut_ptr(), t_hit);
    }
    let mut t_found = t_max;
    let mut found: isize = -1;
    for (i, &t_hit) in t_hits.iter().enumerate().take(xs.len()) {
        if t_hit <= t_found {
            t_found = t_hit;
            found = i as isize
        }
    }
    (t_found, found)
}
