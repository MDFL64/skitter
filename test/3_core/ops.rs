mod _builtin;

struct Vec2{
    x: f32,
    y: f32
}

impl Vec2 {
    pub fn new(x: f32, y: f32) -> Self {
        Vec2{x,y}
    }

    pub fn print(&self) {
        _builtin::print_float(self.x as _);
        _builtin::print_float(self.y as _);
    }
}

impl std::ops::Add for Vec2 {
    type Output = Self;

    fn add(self, other: Self) -> Self {
        Self {
            x: self.x + other.x,
            y: self.y + other.y,
        }
    }
}

impl std::ops::Mul<f32> for Vec2 {
    type Output = Self;

    fn mul(self, other: f32) -> Self {
        Self {
            x: self.x * other,
            y: self.y + other,
        }
    }
}

impl std::ops::Mul<Vec2> for f32 {
    type Output = Vec2;

    fn mul(self, other: Vec2) -> Vec2 {
        Vec2 {
            x: self * other.x,
            y: self + other.y,
        }
    }
}

impl std::ops::MulAssign<f32> for Vec2 {
    fn mul_assign(&mut self, other: f32) {
        self.x *= other;
        self.y *= other;
    }
}

impl std::ops::Neg for Vec2 {
    type Output = Self;

    fn neg(self) -> Self {
        Self {
            x: -self.x,
            y: -self.y,
        }
    }
}

pub fn main() {
    let a = Vec2::new(9.0,5.0);
    let b = Vec2::new(-1.5,2.0);

    let c = a + b;
    c.print();

    let d = c * 5.0;
    let e = 10.0 * Vec2::new(2.0,4.0);

    d.print();
    e.print();

    let mut f = -e;
    f.print();

    f *= 5.0;
    f.print();
}
