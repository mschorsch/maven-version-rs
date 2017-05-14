//! # Maven 2 Version Parser
//! 
//! This crate is a direct translation of
//! `https://github.com/apache/maven/blob/maven-2.2.x/maven-artifact/src/main/java/org/apache/maven/artifact/versioning/DefaultArtifactVersion.java`
//!
use std::fmt;
use std::convert::From;
use std::cmp::Ordering;
use std::hash;

use ::ArtifactVersion;

#[derive(Debug)]
pub struct Maven2ArtifactVersion<'a> {
    major_version: Option<u32>,
    minor_version: Option<u32>,
    incremental_version: Option<u32>,
    build_number: Option<u32>,
    qualifier: Option<&'a str>,
    unparsed: &'a str,
}

impl<'a> Maven2ArtifactVersion<'a> {

    /// Creates a new instance of Maven2ArtifactVersion.
    pub fn new(maven_version: &'a str) -> Self {
        parse_version(maven_version)
    }

    pub fn major_version(&self) -> u32 {
        self.major_version.unwrap_or(0)
    }
    
    pub fn minor_version(&self) -> u32 {
        self.minor_version.unwrap_or(0)
    }

    pub fn incremental_version(&self) -> u32 {
        self.incremental_version.unwrap_or(0)
    }

    pub fn build_number(&self) -> u32 {
        self.build_number.unwrap_or(0)
    }

    pub fn qualifier(&self) -> Option<&'a str> {
        self.qualifier
    }    
}

impl<'a> ArtifactVersion for Maven2ArtifactVersion<'a> {

    fn version(&self) -> &str {
        self.unparsed
    }
}

// From
//
impl<'a> From<&'a str> for Maven2ArtifactVersion<'a> {

    fn from(maven_version: &'a str) -> Maven2ArtifactVersion<'a> {
        Maven2ArtifactVersion::new(maven_version)
    }
}

// Display
//
impl<'a> fmt::Display for Maven2ArtifactVersion<'a> {

    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, "{}", self.unparsed)
    }
}

#[allow(unused_assignments)]
fn parse_version(maven_version: &str) -> Maven2ArtifactVersion {
    let mut major_version: Option<u32> = None;
    let mut minor_version: Option<u32> = None;
    let mut incremental_version: Option<u32> = None;
    let mut build_number: Option<u32> = None;
    let mut qualifier: Option<&str> = None;

    // 0. Let's go
    let mut part1: Option<&str> = None;
    let mut part2: Option<&str> = None;

    // 1. Split version-qualifier
    let splitted: Vec<&str> = maven_version.splitn(2, '-').collect();
    if splitted.len() == 2 {
        part1 = Some(splitted[0]);
        part2 = Some(splitted[1]);
    } else {
        part1 = Some(maven_version);
    }

    // 2. Parse quailifier
    if let Some(q) = part2 {
        if q.chars().count() == 1 || !q.starts_with('0') {
            if let Ok(bn) = q.parse::<u32>() {
                build_number = Some(bn);
            } else {
                qualifier = part2;
            }
        } else {
            qualifier = part2;
        }
    }

    // 3. Parse Version
    if let Some(part1_str) = part1 {
        if !part1_str.contains('.') && !part1_str.starts_with('0') {
            if let Ok(mv) = part1_str.parse::<u32>() {
                major_version = Some(mv);
            } else {
                qualifier = Some(maven_version);
                build_number = None;
            } 

        } else {
            let mut fallback = false;
            let mut token_iter = part1_str.split('.');

            // major
            if let Some(value) = token_iter.next() {
                if let Ok(i) = parse_integer_token(value) {
                    major_version = Some(i);
                } else {
                    fallback = true;
                }   
            } else {
                fallback = true;
            }

            // minor
            if let Some(value) = token_iter.next() {
                if let Ok(i) = parse_integer_token(value) {
                    minor_version = Some(i);
                } else {
                    fallback = true;
                }   
            }

            // incremental
            if let Some(value) = token_iter.next() {
                if let Ok(i) = parse_integer_token(value) {
                    incremental_version = Some(i);
                } else {
                    fallback = true;
                }   
            }

            // rest            
            if token_iter.next().is_some() {
                fallback = true;
            }

            if part1_str.contains("..") || part1_str.starts_with('.') || part1_str.ends_with('.') {
                fallback = true;
            }

            if fallback {
                // qualifier is the whole version, including "-"
                qualifier = Some(maven_version);
                major_version = None;
                minor_version = None;
                incremental_version = None;
                build_number = None;                
            }
        }
    }

    Maven2ArtifactVersion {
        major_version: major_version,
        minor_version: minor_version,
        incremental_version: incremental_version,
        build_number: build_number,
        qualifier: qualifier,
        unparsed: maven_version,
    }
}

fn parse_integer_token(token: &str) -> Result<u32, String> {
    if token.chars().count() > 1 && token.starts_with('0') {
        Err(format!("Number part has a leading 0: '{}'", token))
    } else {
        token.parse::<u32>().map_err(|_| "Number is invalid".to_string())
    }
}

// PartialEq
//
impl<'a> PartialEq for Maven2ArtifactVersion<'a> {

    fn eq(&self, other: &Maven2ArtifactVersion<'a>) -> bool {
        self.partial_cmp(other) == Some(Ordering::Equal)
    }
}

// Eq
//
impl<'a> Eq for Maven2ArtifactVersion<'a> {}

// PartialOrd
//
impl<'a> PartialOrd for Maven2ArtifactVersion<'a> {

    fn partial_cmp(&self, other: &Maven2ArtifactVersion<'a>) -> Option<Ordering> {
        Some(self.cmp(other))
    }
}

// Ord
//
impl<'a> Ord for Maven2ArtifactVersion<'a> {

    fn cmp(&self, other: &Maven2ArtifactVersion<'a>) -> Ordering {
        let mut result: Ordering = self.major_version().cmp(&other.major_version());

        if result == Ordering::Equal {
            result = self.minor_version().cmp(&other.minor_version());
        }

        if result == Ordering::Equal {
            result = self.incremental_version().cmp(&other.incremental_version());
        }

        if result == Ordering::Equal {
            if let Some(qualifier) = self.qualifier() {
                if let Some(other_qualifier) = other.qualifier() {
                    let qualifier_count = qualifier.chars().count();
                    let other_qualifier_count = other_qualifier.chars().count();

                    if qualifier_count > other_qualifier_count && qualifier.starts_with(other_qualifier) {
                        // here, the longer one that otherwise match is considered older
                        result = Ordering::Less;
                    } else if qualifier_count < other_qualifier_count && other_qualifier.starts_with(qualifier){
                        // here, the longer one that otherwise match is considered older
                        result = Ordering::Greater;
                    } else {
                        result = qualifier.cmp(other_qualifier);
                    }

                } else {
                    // otherVersion has no qualifier but we do - that's newer
                    result = Ordering::Less;                    
                }                
            } else if other.qualifier().is_some() {
                // otherVersion has a qualifier but we don't, we're newer
                result = Ordering::Greater;
            } else {
                result = self.build_number().cmp(&other.build_number());
            }
        }

        result
    }
}

// Hash
//
impl<'a> hash::Hash for Maven2ArtifactVersion<'a> {

    fn hash<H: hash::Hasher>(&self, state: &mut H) {
        self.major_version().hash(state);
        self.minor_version().hash(state);
        self.incremental_version().hash(state);
        self.build_number().hash(state);

        if let Some(qualifier) = self.qualifier() {
            qualifier.hash(state);
        }
    }
}

#[cfg(test)]
mod tests {

    // https://github.com/apache/maven/blob/maven-2.2.x/maven-artifact/src/test/java/org/apache/maven/artifact/versioning/DefaultArtifactVersionTest.java

    use super::*;

    #[test]
    fn test_version_parsing() {
        check_version_parsing( "1", 1, 0, 0, 0, None );
        check_version_parsing( "1.2", 1, 2, 0, 0, None );
        check_version_parsing( "1.2.3", 1, 2, 3, 0, None );
        check_version_parsing( "1.2.3-1", 1, 2, 3, 1, None );
        check_version_parsing( "1.2.3-alpha-1", 1, 2, 3, 0, Some("alpha-1") );
        check_version_parsing( "1.2-alpha-1", 1, 2, 0, 0, Some("alpha-1") );
        check_version_parsing( "1.2-alpha-1-20050205.060708-1", 1, 2, 0, 0, Some("alpha-1-20050205.060708-1") );
        check_version_parsing( "RELEASE", 0, 0, 0, 0, Some("RELEASE") );
        check_version_parsing( "2.0-1", 2, 0, 0, 1, None );

        // 0 at the beginning of a number has a special handling
        check_version_parsing( "02", 0, 0, 0, 0, Some("02") );
        check_version_parsing( "0.09", 0, 0, 0, 0, Some("0.09") );
        check_version_parsing( "0.2.09", 0, 0, 0, 0, Some("0.2.09") );
        check_version_parsing( "2.0-01", 2, 0, 0, 0, Some("01") );

        // version schemes not really supported: fully transformed as qualifier
        check_version_parsing( "1.0.1b", 0, 0, 0, 0, Some("1.0.1b") );
        check_version_parsing( "1.0M2", 0, 0, 0, 0, Some("1.0M2") );
        check_version_parsing( "1.0RC2", 0, 0, 0, 0, Some("1.0RC2") );
        check_version_parsing( "1.7.3.0", 0, 0, 0, 0, Some("1.7.3.0") );
        check_version_parsing( "1.7.3.0-1", 0, 0, 0, 0, Some("1.7.3.0-1") );
        check_version_parsing( "PATCH-1193602", 0, 0, 0, 0, Some("PATCH-1193602") );
        check_version_parsing( "5.0.0alpha-2006020117", 0, 0, 0, 0, Some("5.0.0alpha-2006020117") );
        check_version_parsing( "1.0.0.-SNAPSHOT", 0, 0, 0, 0, Some("1.0.0.-SNAPSHOT") );
        check_version_parsing( "1..0-SNAPSHOT", 0, 0, 0, 0, Some("1..0-SNAPSHOT") );
        check_version_parsing( "1.0.-SNAPSHOT", 0, 0, 0, 0, Some("1.0.-SNAPSHOT") );
        check_version_parsing( ".1.0-SNAPSHOT", 0, 0, 0, 0, Some(".1.0-SNAPSHOT") );

        check_version_parsing( "1.2.3.200705301630", 0, 0, 0, 0, Some("1.2.3.200705301630") );
        check_version_parsing( "1.2.3-200705301630", 1, 2, 3, 0, Some("200705301630") );
    }

    fn check_version_parsing(maven_version: &str, major: u32, minor: u32, incremental: u32, build_number: u32, qualifier: Option<&str>) {
        let actual = Maven2ArtifactVersion::from(maven_version);
        let parsed = format!("'{}' parsed as ('{:?}', '{:?}', '{:?}', '{:?}', '{:?}'), ",
                         maven_version, actual.major_version, actual.minor_version,
                         actual.incremental_version, actual.build_number,
                         actual.qualifier);

        assert_eq!(major, actual.major_version(), "{} check major version", parsed );
        assert_eq!(minor, actual.minor_version(), "{} check minor version", parsed );
        assert_eq!(incremental, actual.incremental_version(), "{} check incremental version", parsed );
        assert_eq!(build_number, actual.build_number(), "{} check build number version", parsed );
        assert_eq!(qualifier, actual.qualifier(), "{} check qualifier version", parsed );
    }

    #[test]
    fn test_version_comparing() {
        assert_version_equal( "1", "1" );
        assert_version_older( "1", "2" );
        assert_version_older( "1.5", "2" );
        assert_version_older( "1", "2.5" );
        assert_version_equal( "1", "1.0" );
        assert_version_equal( "1", "1.0.0" );
        assert_version_older( "1.0", "1.1" );
        assert_version_older( "1.1", "1.2" );
        assert_version_older( "1.0.0", "1.1" );
        assert_version_older( "1.1", "1.2.0" );
        assert_version_older( "1.2", "1.10" );

        assert_version_older( "1.0-alpha-1", "1.0" );
        assert_version_older( "1.0-alpha-1", "1.0-alpha-2" );
        assert_version_older( "1.0-alpha-1", "1.0-beta-1" );

        assert_version_older( "1.0-SNAPSHOT", "1.0-beta-1" );
        assert_version_older( "1.0-SNAPSHOT", "1.0" );
        assert_version_older( "1.0-alpha-1-SNAPSHOT", "1.0-alpha-1" );

        assert_version_older( "1.0", "1.0-1" );
        assert_version_older( "1.0-1", "1.0-2" );
        assert_version_equal( "2.0-0", "2.0" );
        assert_version_older( "2.0", "2.0-1" );
        assert_version_older( "2.0.0", "2.0-1" );
        assert_version_older( "2.0-1", "2.0.1" );

        assert_version_older( "2.0.1-klm", "2.0.1-lmn" );
        assert_version_older( "2.0.1-xyz", "2.0.1" );

        assert_version_older( "2.0.1", "2.0.1-123" );
        assert_version_older( "2.0.1-xyz", "2.0.1-123" );
    }

    #[test]
    fn test_version_snapshot_comparing() {
        assert_version_equal( "1-SNAPSHOT", "1-SNAPSHOT" );
        assert_version_older( "1-SNAPSHOT", "2-SNAPSHOT" );
        assert_version_older( "1.5-SNAPSHOT", "2-SNAPSHOT" );
        assert_version_older( "1-SNAPSHOT", "2.5-SNAPSHOT" );
        assert_version_equal( "1-SNAPSHOT", "1.0-SNAPSHOT" );
        assert_version_equal( "1-SNAPSHOT", "1.0.0-SNAPSHOT" );
        assert_version_older( "1.0-SNAPSHOT", "1.1-SNAPSHOT" );
        assert_version_older( "1.1-SNAPSHOT", "1.2-SNAPSHOT" );
        assert_version_older( "1.0.0-SNAPSHOT", "1.1-SNAPSHOT" );
        assert_version_older( "1.1-SNAPSHOT", "1.2.0-SNAPSHOT" );
        assert_version_older( "1.0-alpha-1-SNAPSHOT", "1.0-alpha-2-SNAPSHOT" );
        assert_version_older( "1.0-alpha-1-SNAPSHOT", "1.0-beta-1-SNAPSHOT" );
        assert_version_older( "1.0-SNAPSHOT-SNAPSHOT", "1.0-beta-1-SNAPSHOT" );
        assert_version_older( "1.0-SNAPSHOT-SNAPSHOT", "1.0-SNAPSHOT" );
        assert_version_older( "1.0-alpha-1-SNAPSHOT-SNAPSHOT", "1.0-alpha-1-SNAPSHOT" );
        assert_version_older( "2.0-1-SNAPSHOT", "2.0.1-SNAPSHOT" );
        assert_version_older( "2.0.1-klm-SNAPSHOT", "2.0.1-lmn-SNAPSHOT" );
    }    

    #[test]
    fn test_snapshot_releases() {
         assert_version_older( "1.0-RC1", "1.0-SNAPSHOT" );
    }

    fn assert_version_older(left: &str, right: &str) {
        assert!(new_artifact_version( left ).cmp( &new_artifact_version( right ) ) == Ordering::Less, "{} should be older than {}", left, right);
        assert!(new_artifact_version( right ).cmp( &new_artifact_version( left ) ) == Ordering::Greater, "{} should be newer than {}", right, left);
    }

    fn assert_version_equal(left: &str, right: &str) {
        assert!(new_artifact_version( left ).cmp( &new_artifact_version( right ) ) == Ordering::Equal, "{} should be equal to {}", left, right);
        assert!(new_artifact_version( right ).cmp( &new_artifact_version( left ) ) == Ordering::Equal, "{} should be equal to {}", right, left);
    }

    fn new_artifact_version<'a>(version: &'a str) -> Maven2ArtifactVersion<'a> {
        Maven2ArtifactVersion::from(version)
    }
}