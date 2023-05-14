use clap::Parser;

/// Simple program to greet a person
#[derive(Parser, Debug, Clone)]
#[command(author, version, about, long_about = None)]
pub struct CliArgs {
    /// The rust file to run.
    pub file_name: String,

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
    pub core: bool
}
