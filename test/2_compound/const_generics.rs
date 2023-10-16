mod _builtin;

fn foo<const N: i32>() {
    _builtin::print_int((N * 2) as _);
}

fn bar<const N: i32>() {
    foo::<N>();
}

const fn baz() -> i32 {
    15
}

fn fud<const N: usize>() {
    let array = [5;N];

    let mut sum = 0;

    for n in array {
        sum += n;
    }

    _builtin::print_int(sum as _);
}

fn main() {
    foo::<3>();
    foo::<4>();
    foo::<5>();
    foo::<-10>();

    bar::<123>();

    foo::<{baz() + 3}>();
    bar::<{baz() + 3}>();

    fud::<9>();
    fud::<30>();
}
