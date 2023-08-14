use std::path::PathBuf;

use clap::Parser;

/// Simple program to greet a person
#[derive(Parser, Debug, Clone)]
#[command(author, version, about, long_about = None)]
pub struct CliArgs {
    /// The rust file to run.
    pub file_name: PathBuf,

    /// Used to run tests. Pass a directory as file_name.
    #[clap(long)]
    pub test: bool,

    /// Dump timing information after running.
    #[clap(long)]
    pub profile: bool,

    /// Print debug information.
    #[clap(long, short)]
    pub verbose: bool,

    /// Load core sources (SLOW, TEMPORARY FEATURE)
    #[clap(long)]
    pub core: bool,

    /// Save IR to a cache file
    #[clap(long)]
    pub save: bool,

    /// Load IR from a cache file instead of compiling from source
    #[clap(long)]
    pub load: bool,

    /// Continue the same action in a loop, forever, or until an error is encountered.
    #[clap(long)]
    pub repeat: bool,
}
