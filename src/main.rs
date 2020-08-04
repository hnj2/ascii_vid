mod math;
use math::*;

fn dimmness_to_ascii(dimmness: &f64) -> char {
    const DIMMER: [char; 15] = [' ','.',',','-',':',';','~','!','=','*','@','$','#','▮','■'];
    assert!(dimmness.is_finite());
    let index = ((DIMMER.len() as f64) * dimmness.max(0.0)) as usize;
    DIMMER[index.min(DIMMER.len() - 1)]
}

fn dimmnesses_to_ascii(dimmnesses: &[f64]) -> String {
    dimmnesses.iter().map(dimmness_to_ascii).collect()
    //String::from_utf8(bytes).unwrap()
}

const HEIGHT: usize = 50;
const WIDTH: usize = 100;
const H_PER_W: f64 = 2.05;

pub struct Frame {
    val: [[f64; WIDTH]; HEIGHT],
    z_inv: [[f64; WIDTH]; HEIGHT],
    light: Vec3,
    f_offset: f64,
}

impl Frame {
    pub fn new(light: Vec3, z_offset: f64, min_radius: f64) -> Self {
        assert!(z_offset > 1e-12);
        assert!(min_radius > 1e-12);
        Self {
            val: [[0f64; WIDTH]; HEIGHT],
            z_inv: [[0f64; WIDTH]; HEIGHT],
            light,
            f_offset: z_offset / min_radius * (WIDTH as f64).min((HEIGHT as f64) * H_PER_W) / 2.0,
        }
    }

    pub fn set_light(&mut self, light: Vec3) {
        self.light = light;
    }

    pub fn set_fov(&mut self, z_offset: f64, min_radius: f64) {
        self.f_offset = z_offset / min_radius * (WIDTH as f64).min((HEIGHT as f64) * H_PER_W) / 2.0;
    }

    fn line(&self, x: usize, y: usize) -> Vec3 {
        const X_OFFSET: f64 = (WIDTH as f64) / 2.0;
        const Y_OFFSET: f64 = (HEIGHT as f64) / 2.0;
        Vec3(
            (x as f64) - X_OFFSET,
            ((y as f64) - Y_OFFSET) * H_PER_W,
            self.f_offset,
        ).norm()
    }

    pub fn add_tri(&mut self, tri: &Tri3, offset: &Vec3) {
        for x in (0..WIDTH) {
            for y in (0..HEIGHT) {
                let line = self.line(x,y);
                if let Some(z) = tri.intersection(&line, offset) {
                    let z_inv = z.recip();
                    if z_inv > self.z_inv[y][x] {
                        self.z_inv[y][x] = z_inv;
                        let mut normal = tri.normal();
                        // Turn normal towards viewer
                        if normal.dot(&line).is_sign_positive() {
                            normal = Vec3(-normal.0, -normal.1, -normal.2);
                        }
                        let lum = self.light.dot(&normal);
                        self.val[y][x] = lum.sqrt().max(15.0f64.recip() + 1e-10);
                    }
                }
            }
        }
    }

    pub fn alloc_space(&self) {
        for _ in (0..HEIGHT) {
            println!();
        }
    }

    pub fn clear(&mut self) {
        self.val = [[0f64; WIDTH]; HEIGHT];
        self.z_inv = [[0f64; WIDTH]; HEIGHT];
    }

    pub fn draw(&self) {
        print!("\r{}[{}A", std::str::from_utf8(&[27u8]).unwrap(), HEIGHT);
        for row in &self.val {
            println!("{}", dimmnesses_to_ascii(row));
        }
    }
}

#[derive(Copy, Clone)]
pub struct Trans {
    pub mat: Mat3,
    pub offset: Vec3,
}

impl Trans {
    pub fn apply_mat(&self, p: &Vec3) -> Vec3 {
        self.mat.mul_vec(&p)
    }

    pub fn chain(&self, o: &Self) -> Self {
        Self {
            mat: self.mat.mul_mat(&o.mat),
            offset: self.offset.add(&self.mat.mul_vec(&o.offset)),
        }
    }
}

pub struct StripIter<'a, T> where T : Iterator<Item = &'a Vec3> {
    points: T,
    trans: Trans,
    a: Vec3,
    b: Vec3,
    c: Vec3,
}

impl<'a, T : Iterator<Item = &'a Vec3>> StripIter<'a, T> {
    pub fn new(mut points: T, trans: Trans) -> Self {
        let a = Vec3(0.0,0.0,0.0);
        let b = trans.apply_mat(points.next().unwrap_or(&a));
        let c = trans.apply_mat(points.next().unwrap_or(&a));
        Self { points, trans, a, b, c }
    }

    pub fn render(&mut self, frame: &mut Frame) {
        let offset = self.trans.offset;
        for tri in self {
            frame.add_tri(&tri, &offset);
        }
    }
}

impl<'a, T : Iterator<Item = &'a Vec3>> Iterator for StripIter<'a, T> {
    type Item = Tri3;

    fn next(&mut self) -> Option<Tri3> {
        let c = self.trans.apply_mat(self.points.next()?);
        self.a = self.b;
        self.b = self.c;
        self.c = c;
        Some(Tri3{ a: self.a, b: self.b, c: self.c })
    }
}

pub struct Tetra {
    pub a: Vec3,
    pub b: Vec3,
    pub c: Vec3,
    pub d: Vec3,
}

impl Tetra {
    pub fn render(&self, mat: &Mat3, offset: &Vec3, frame: &mut Frame) {
        let a = mat.mul_vec(&self.a);
        let b = mat.mul_vec(&self.b);
        let c = mat.mul_vec(&self.c);
        let d = mat.mul_vec(&self.d);

        frame.add_tri(&Tri3{ a: a, b: b, c: c}, offset);
        frame.add_tri(&Tri3{ a: a, b: b, c: d}, offset);
        frame.add_tri(&Tri3{ a: a, b: c, c: d}, offset);
        frame.add_tri(&Tri3{ a: b, b: c, c: d}, offset);
    }
}

const TORUS_DIFFS_1: usize = 10;
const TORUS_DIFFS_2: usize = 20;
const TORUS_INC_1: f64 = std::f64::consts::TAU / (TORUS_DIFFS_1 as f64);
const TORUS_INC_2: f64 = std::f64::consts::TAU / (TORUS_DIFFS_2 as f64);

pub struct Torus {
    strips: [[Vec3; 2*TORUS_DIFFS_1+2]; TORUS_DIFFS_2],
}

impl Torus {

    pub fn render(&self, trans: &Trans, frame: &mut Frame) {
        for s in &self.strips {
            StripIter::new(s.into_iter(), *trans).render(frame);
        }
    }

    pub fn new(r1: f64, r2: f64) -> Self {
        let rot0 = Mat3::rot_y(TORUS_INC_1 / 2.0);
        let rot1 = Mat3::rot_z(TORUS_INC_2);
        let rot2 = Mat3::rot_z(TORUS_INC_2 * 2.0);

        let mut strip = [Vec3(0.0,0.0,0.0); 2*TORUS_DIFFS_1+2];

        // Build first strip
        let mut point = Vec3(r2,0.0,0.0);
        let mut offset = Vec3(r1,0.0,0.0);
        for i in 0..=TORUS_DIFFS_1 {
            strip[2*i] = point.add(&offset);
            point = rot0.mul_vec(&point);
            strip[2*i+1] = rot1.mul_vec(&point.add(&offset));
            point = rot0.mul_vec(&point);
        }

        // Build other strips
        let mut strips = [strip; TORUS_DIFFS_2];
        let mut rot_odd = 0usize;
        for i in 1..TORUS_DIFFS_2 {
            for i in 0..=TORUS_DIFFS_1 {
                strip[2*i + rot_odd] = rot2.mul_vec(&strip[2*i + rot_odd])
            }
            rot_odd = 1 - rot_odd;
            strips[i] = strip;
        }

        Self { strips }
    }
}

use std::thread::sleep;
use std::time::Duration;

fn main() {

    let tetra = Tetra {
        a: Vec3( 1.0,-1.0, 0.0),
        b: Vec3(-1.0,-1.0, 0.0),
        c: Vec3( 0.0, 1.0, 1.0),
        d: Vec3( 0.0, 1.0,-1.0),
    };

    let torus = Torus::new(2.0, 1.0);

    let tori : Vec<Torus> = (0..100).map(|i| Torus::new(((i as f64)/50.0 * std::f64::consts::PI).sin() + 2.0, 1.0)).collect();

    let mut frame = Frame::new(Vec3(0.6,-0.4,1.0), 4.0, 4.05);
    frame.alloc_space();

    let mut angle = 0f64;
    let mut i: usize = 0;
    loop {
        let mat = Mat3::rot_z(angle)
            .mul_mat(&Mat3::rot_y(angle/1.643221))
            .mul_mat(&Mat3::rot_x(angle/6.12354223));
        let dist = 30.0 + 5.0 * (angle*2.254234).sin();
        let offset = Vec3(0.0,0.0,dist);
        frame.set_fov(dist, 4.05);
        frame.set_light(Mat3::rot_y(angle*3.23123).mul_vec(&Vec3(0.6,-0.4,1.0)));
        //tetra.render(&mat, &offset, &mut frame);
        //torus.render(&Trans { mat, offset }, &mut frame);
        tori[i].render(&Trans { mat, offset }, &mut frame);
        frame.draw();
        frame.clear();

        //sleep(Duration::from_millis(50));
        angle += 0.1;
        i += 1;
        if i == 100 { i = 0 }
    }
}
