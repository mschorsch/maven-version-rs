//! # Maven 2/3 Version Parser

// modules
mod maven2;
mod maven3;

// public uses
pub use maven2::Maven2ArtifactVersion;
pub use maven3::Maven3ArtifactVersion;

use std::fmt;

pub trait ArtifactVersion : fmt::Debug {
    
    fn version(&self) -> &str;
}
