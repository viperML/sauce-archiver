use color_eyre::eyre::Result;
use tracing::trace;
use tracing_subscriber::prelude::*;

fn main() -> Result<()> {
    color_eyre::install()?;

    let layer = tracing_subscriber::fmt::layer()
        .compact()
        .with_filter(tracing_subscriber::EnvFilter::from_default_env());
    tracing_subscriber::registry().with(layer).init();

    trace!("Hello, world!");

    Ok(())
}
