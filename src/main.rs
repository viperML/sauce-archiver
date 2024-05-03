mod saucenao;

use std::path::PathBuf;

use clap::Parser;
use color_eyre::eyre::Result;
use tracing::trace;
use tracing_subscriber::prelude::*;

#[derive(Debug, Parser)]
struct AppConfig {
    #[arg(long, env = "SAUCENAO_APIKEY")]
    saucenao_apikey: String,

    #[arg(long, default_value = "CAG_INPUT")]
    dir_input: PathBuf,
    #[arg(long, default_value = "CAG_SAUCE")]
    dir_sauce: PathBuf,
    #[arg(long, default_value = "CAG_NOSAUCE")]
    dir_nosauce: PathBuf,
}

fn main() -> Result<()> {
    color_eyre::install()?;

    let layer = tracing_subscriber::fmt::layer()
        .compact()
        .with_filter(tracing_subscriber::EnvFilter::from_default_env());
    tracing_subscriber::registry().with(layer).init();

    let args = <AppConfig as Parser>::parse();

    Ok(())
}
