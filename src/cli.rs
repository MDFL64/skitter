use std::ffi::OsString;

use clap::Parser;

/// Simple program to greet a person
#[derive(Parser, Debug, Clone)]
#[command(author, version, about, long_about = None)]
pub struct CliArgs {
    /// The rust file to run.
    pub file_name: OsString,

    /// Used to run tests. Pass a directory as file_name.
    #[clap(long)]
    pub test: bool,

    /// Dump timing information after running.
    #[clap(long)]
    pub profile: bool,

    /// Print debug information.
    #[clap(long, short)]
    pub verbose: bool,

    /// Save IR to a cache file
    #[clap(long)]
    pub save: bool,

    /// Continue the same action in a loop, forever, or until an error is encountered.
    #[clap(long)]
    pub debug_repeat: bool,

    /// Lookup local inherent impls instead of using a fast path.
    #[clap(long)]
    pub debug_local_impls: bool,

    /// Compile dependencies instead of loading cached IR. SLOW.
    #[clap(long)]
    pub debug_no_load: bool,

    /// Log the arguments and return value of every function call.
    #[clap(long)]
    pub debug_trace_calls: bool,
}
