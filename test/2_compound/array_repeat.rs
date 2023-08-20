mod _builtin;

type List<T> = [T;10];

fn sum(input: List<(f32,f32)>, output: &mut List<f32>) {
    let mut i = 0;
    while i < 10 {
        output[i] = input[i].0 + input[i].1;
        i += 1;
    }
}

fn print(input: List<f32>) {
    let mut i = 0;
    while i < 10 {
        _builtin::print_float(input[i] as _);
        i += 1;
    }
}

pub fn main() {
    let input = [(0.5,3.0);10];
    let mut output = [0.0;10];
    sum(input,&mut output);
    print(output);
}
