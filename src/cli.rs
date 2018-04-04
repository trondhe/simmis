#[derive(clap::Parser, Debug)]
#[command(version, about, long_about = None)]
pub struct Cli {
    pub input: String,
}
