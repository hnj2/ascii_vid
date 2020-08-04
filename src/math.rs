#![allow(unused)]

#[derive(Copy, Clone, Debug)]
pub struct Vec3(pub f64, pub f64, pub f64);

impl Vec3 {
    pub fn add(&self, o: &Vec3) -> Self { Self(self.0 + o.0, self.1 + o.1, self.2 + o.2) }
    pub fn sub(&self, o: &Vec3) -> Self { Self(self.0 - o.0, self.1 - o.1, self.2 - o.2) }
    pub fn scale(&self, f: f64) -> Self { Self(self.0 * f, self.1 * f, self.2 * f) }
    pub fn dot(&self, o: &Vec3) -> f64 { self.0 * o.0 + self.1 * o.1 + self.2 * o.2 }
    pub fn abs(&self) -> f64 { self.dot(self).sqrt() }
    pub fn norm(&self) -> Self { self.scale(self.abs().recip()) }
    pub fn is_zero(&self) -> bool { self.abs() < 1e-12 }

    pub fn cross(&self, f: &Vec3) -> Self {
        Self(
            self.1 * f.2 - self.2 * f.1,
            self.2 * f.0 - self.0 * f.2,
            self.0 * f.1 - self.1 * f.0,
        )
    }
}

#[test]
fn vec_test() {
    let x = Vec3(1.0,0.0,0.0);
    let y = Vec3(0.0,1.0,0.0);
    let z = Vec3(0.0,0.0,1.0);
    
    fn is_zero(v: f64) -> bool { v < 1e-12 }

    assert!(is_zero(x.abs() - 1.0));
    assert!(is_zero(y.abs() - 1.0));
    assert!(is_zero(z.abs() - 1.0));

    assert!(is_zero(x.add(&y).abs() - 2_f64.sqrt()));
    assert!(is_zero(x.add(&y).sub(&z).abs() - 3_f64.sqrt()));
    assert!(is_zero(x.add(&y).sub(&z).norm().abs() - 1.0));

    assert!(x.sub(&x).is_zero());
    assert!(!x.sub(&y).is_zero());

    assert!(x.cross(&y).sub(&z).is_zero());
}

#[derive(Copy, Clone, Debug)]
pub struct Mat3(pub Vec3, pub Vec3, pub Vec3);

impl Mat3 {
    pub fn rot_x(angle: f64) -> Self {
        let sin = angle.sin();
        let cos = angle.cos();
        Self(
            Vec3(1.0, 0.0, 0.0),
            Vec3(0.0, cos,-sin),
            Vec3(0.0, sin, cos),
        )
    }

    pub fn rot_y(angle: f64) -> Self {
        let sin = angle.sin();
        let cos = angle.cos();
        Self(
            Vec3( cos, 0.0, sin),
            Vec3( 0.0, 1.0, 0.0),
            Vec3(-sin, 0.0, cos),
        )
    }

    pub fn rot_z(angle: f64) -> Self {
        let sin = angle.sin();
        let cos = angle.cos();
        Self(
            Vec3(cos,-sin, 0.0),
            Vec3(sin, cos, 0.0),
            Vec3(0.0, 0.0, 1.0),
        )
    }

    fn cof0(&self) -> Vec3 { let mut r = self.1.cross(&self.2); /*r.1 = -r.1;*/ r }
    fn cof1(&self) -> Vec3 { let mut r = self.2.cross(&self.0); /*r.1 = -r.1;*/ r }
    fn cof2(&self) -> Vec3 { let mut r = self.0.cross(&self.1); /*r.1 = -r.1;*/ r }

    pub fn mul_vec(&self, o: &Vec3) -> Vec3 {
        Vec3(self.0.dot(o), self.1.dot(o), self.2.dot(o))
    }

    pub fn mul_mat(&self, o: &Self) -> Self {
        let t = o.transpose();
        Self(self.mul_vec(&t.0), self.mul_vec(&t.1), self.mul_vec(&t.2)).transpose()
    }

    pub fn det(&self) -> f64 { self.0.dot(&self.cof0()) }

    pub fn transpose(&self) -> Self {
        Self(
            Vec3(self.0.0, self.1.0, self.2.0),
            Vec3(self.0.1, self.1.1, self.2.1),
            Vec3(self.0.2, self.1.2, self.2.2),
        )
    }

    pub fn inv_transpose(&self) -> Option<Self> {
        let c0 = self.cof0();
        let det = self.0.dot(&c0);
        if det.abs() < 1e-12 { return None }
        let f = det.recip();
        Some(
            Self(
                c0.scale(f),
                self.cof1().scale(f),
                self.cof2().scale(f)
            )
        )
    }

    pub fn inv(&self) -> Option<Self> { Some(self.inv_transpose()?.transpose()) }
}

#[test]
fn mat_test() {

    fn is_zero(v: f64) -> bool { v < 1e-12 }

    let x = Vec3(1.0,0.0,0.0);
    let y = Vec3(0.0,1.0,0.0);
    let z = Vec3(0.0,0.0,1.0);
    let i = Mat3(x,y,z);

    let i_inv = i.inv().unwrap();

    println!("{:#?}", i_inv);

    assert!((i.0.sub(&i_inv.0)).is_zero());
    assert!((i.1.sub(&i_inv.1)).is_zero());
    assert!((i.2.sub(&i_inv.2)).is_zero());

    let m = Mat3(Vec3(1.0, 2.0, 3.0),
                 Vec3(4.0, 5.0, 6.0),
                 Vec3(7.0, 8.0, 9.9));
    println!("m:\n{:#?}", m);

    let m_inv = &m.inv().unwrap();
    println!("m_inv:\n{:#?}", m_inv);

    let m_i = m.mul_mat(&m_inv);
    println!("m_i:\n{:#?}", m_i);

    assert!((i.0.sub(&m_i.0)).is_zero());
    assert!((i.1.sub(&m_i.1)).is_zero());
    assert!((i.2.sub(&m_i.2)).is_zero());

}

#[derive(Copy, Clone, Debug)]
pub struct Tri3 {
    pub a: Vec3,
    pub b: Vec3,
    pub c: Vec3,
}

impl Tri3 {
    pub fn mul_mat(&self, mat: &Mat3) -> Self {
        Self {
            a: mat.mul_vec(&self.a),
            b: mat.mul_vec(&self.b),
            c: mat.mul_vec(&self.c),
        }
    }

    pub fn normal(&self) -> Vec3 {
        self.b.sub(&self.a).cross(&self.c.sub(&self.a)).norm()
    }

    pub fn intersection(&self, dir: &Vec3, offset: &Vec3) -> Option<f64> {
        /*
            We want to solve:
            a + x*(b-a) + y*(c-a) = z*dir - offset
            as a linear system this is:
            a + offset = [(a-b), (a-c), dir] * [x, y, z]^t
                `---- mat_a ------´
            The intersection is at the point:
            z * dir - offset = (1-x-y)*a + x*b + y*c

            We can calculate x, y and z with:
            inv(mat_a) * (a + offset) = [x, y, z]^t
            Notice that this means we have:
            x = row_0(inv(mat_a)) * (a + offset)
            y = row_1(inv(mat_a)) * (a + offset)
            z = row_2(inv(mat_a)) * (a + offset)

            The point lies in the triangle if it
            is a convex combination of a, b and c.
            Which is the case iff x, y and x + y lie in [0,1],
            or equivalently iff 0<x, 0<y and x+y<1 hold.

        */
        let mat_a_inv = Mat3(self.a.sub(&self.b), self.a.sub(&self.c), *dir).inv_transpose()?;

        let ao = self.a.add(offset);

        let x = mat_a_inv.0.dot(&ao);
        if x.is_sign_negative() { return None }

        let y = mat_a_inv.1.dot(&ao);
        if y.is_sign_negative() { return None }
        if (x+y) > 1.0 { return None }

        Some(mat_a_inv.2.dot(&ao))
    }
}

#[test]
fn tri_test() {

    fn is_zero(v: f64) -> bool { v < 1e-12 }

    let v0 = Vec3(0.0,0.0,0.0);
    let x = Vec3(1.0,0.0,0.0);
    let y = Vec3(0.0,1.0,0.0);
    let z = Vec3(0.0,0.0,1.0);

    let tri = Tri3 {
        a: Vec3( 1.0, 1.0,1.0),
        b: Vec3(-1.0, 1.0,1.0),
        c: Vec3( 0.0,-1.0,1.0),
    };
    println!("triangle:\n{:#?}", tri);
    println!("normal:\n{:#?}", tri.normal());
    assert!(tri.normal().sub(&z).is_zero());
    println!("line:\n{:#?}", z);
    println!("intersection @ {:#?}", tri.intersection(&z, &v0));

    assert!(is_zero(tri.intersection(&z, &v0).unwrap() - 1.0));
    assert!(is_zero(tri.intersection(&Vec3(0.0,0.0,0.5), &v0).unwrap() - 2.0));
    assert!(is_zero(tri.intersection(&Vec3(0.0,0.25,0.5), &v0).unwrap() - 2.0));
}

