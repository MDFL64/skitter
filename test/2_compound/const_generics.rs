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

fn main() {
    foo::<3>();
    foo::<4>();
    foo::<5>();
    foo::<-10>();

    bar::<123>();

    foo::<{baz() + 3}>();
    bar::<{baz() + 3}>();
}
