use color_eyre::eyre::Result;
use reqwest::{Client, Url};
use std::path::Path;

use crate::AppConfig;

#[derive(Debug)]
struct Sauce {}


async fn sauce_file<P: AsRef<Path>>(input: P, config: &AppConfig) -> Result<Sauce> {
    let url = Url::parse_with_params(
        "https://saucenao.com/search.php",
        [
            ("output_type", "2"),
            ("numres", "1"),
            ("minsim", "85"),
            ("db", "9"),
            ("api_key", &config.saucenao_apikey),
        ],
    );

    todo!();
}
