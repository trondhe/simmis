#![allow(dead_code)]
mod cli;

mod math;
mod tf;
mod types;

use clap::Parser;

fn main() {
    let cli = cli::Cli::parse();
    println!("{}", cli.input);
}
