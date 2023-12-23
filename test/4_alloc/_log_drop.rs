#[repr(transparent)]
pub struct LogDrop(pub &'static str);

impl Drop for LogDrop {
    fn drop(&mut self) {
        crate::_builtin::print_raw(self.0);
        crate::_builtin::print_raw("\n");
    }
}
