#![doc = include_str!("../README.md")]

use core::fmt::Display;

pub mod evaluation_tree;
pub mod lexer;
pub mod parser;

pub mod lexer_v2;

#[macro_use]
extern crate tracing;

#[cfg(test)]
#[allow(unused)]
pub(crate) fn log_init() {
    use tracing::level_filters::LevelFilter;
    use tracing_subscriber::fmt::{self, format::FmtSpan, time::OffsetTime};

    #[cfg(verbose_logging)]
    if fmt::fmt()
        .with_max_level(LevelFilter::TRACE)
        .with_span_events(FmtSpan::ACTIVE)
        .with_line_number(true)
        .with_level(true)
        .with_target(true)
        .with_timer(OffsetTime::local_rfc_3339().expect("Failed to get local timer"))
        .compact()
        .try_init()
        .is_ok()
    {
        info!("Log up!");
    } else {
        eprintln!("Log is up?");
    }
}

#[allow(
    clippy::ref_option,
    reason = "Use case. Don't want to call `.as_ref()` on each site"
)]
pub(crate) fn display_opt<T: Display>(t: &Option<T>) -> &dyn Display {
    if let Some(t) = t.as_ref() {
        t as &dyn Display
    } else {
        &"" as &dyn Display
    }
}
#[cfg(test)]
extern crate alloc;
