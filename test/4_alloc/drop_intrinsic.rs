mod _builtin;
mod _log_drop;

use _log_drop::LogDrop;


pub fn main() {
    let x = LogDrop("main 1");
    _builtin::print_int(1);
    let y: &str = unsafe { std::mem::transmute(x) };
    
    let x: LogDrop = unsafe { std::mem::transmute("main 2") };

    std::mem::forget(LogDrop("main 3"));

    let mut z = core::mem::ManuallyDrop::new(LogDrop("main 4"));
    unsafe { core::mem::ManuallyDrop::drop(&mut z) }

    let b = Box::new(LogDrop("main 5"));
}
