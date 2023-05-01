mod _builtin;

type List = [f32;10];

fn check_ref(n: &[f32]) {
    let mut i = 0;
    while i < 10 {
        _builtin::print_float(n[i] as _);
        i += 1;
    }
}

fn mutate(n: &mut [f32]) {
    let mut i = 0;
    while i < 10 {
        n[i] = (i as f32 + 10.0) * 3.0;
        i += 1;
    }
}

pub fn main() {
    let mut list: List = [
        1.0,5.0,6.0,8.0,9.5,
        10.12,8.2,-0.5,9e10,44.32];
        
    let list: &mut [f32] = &mut list;
    check_ref(list);

    {
        let mut i = 0;
        while i < 10 {
            list[i] = 1.0/0.0;
            i += 1;
        }
    }
    check_ref(list);

    mutate(list);

    check_ref(list);
}
