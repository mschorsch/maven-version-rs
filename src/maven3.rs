//! # Maven 3 Version Parser
//! 
//! This crate is a direct translation of
//! `https://github.com/apache/maven/blob/master/maven-artifact/src/main/java/org/apache/maven/artifact/versioning/ComparableVersion.java`
//!

// see additional https://cwiki.apache.org/confluence/display/MAVENOLD/Versioning

use std::convert::{AsRef, From};
use std::fmt;
use std::hash;
use std::cmp::Ordering;
use std::cmp;

use ::ArtifactVersion;

const RELEASE_VERSION_INDEX: &'static str = "5"; // QUALIFIERS index of ""

// ---------------------------------------
// Maven3ArtifactVersion
// ---------------------------------------

#[derive(Debug)]
pub struct Maven3ArtifactVersion<'a> {
    version: &'a str,
    canonical: String,
    items: Vec<Item>,
}

/// Maven3 Version Artifact
impl<'a> Maven3ArtifactVersion<'a> {

    /// Creates a new instance of Maven3ArtifactVersion.
    ///
    /// # Example
    ///
    /// ```
    /// use maven_version::Maven3ArtifactVersion; 
    ///
    /// let v1 = Maven3ArtifactVersion::new("2.0.1");
    /// let v2 = Maven3ArtifactVersion::new("2.0.1-xyz");
    /// assert!(v1 < v2);
    /// ```
    pub fn new(version: &'a str) -> Self {
        let items = parse_items(version);
        let canonical = make_canonical(&items);
        Maven3ArtifactVersion{version, canonical, items}
    }

    pub fn canonical(&self) -> &str {
        &self.canonical
    }
}

impl<'a> ArtifactVersion for Maven3ArtifactVersion<'a> {

    fn version(&self) -> &str {
        self.version
    }
}

// From
//
impl<'a> From<&'a str> for Maven3ArtifactVersion<'a> {

    fn from(version: &'a str) -> Maven3ArtifactVersion<'a> {
        Maven3ArtifactVersion::new(version)
    }
}

// Eq
//
impl<'a> Eq for Maven3ArtifactVersion<'a> {}

impl<'a> PartialEq for Maven3ArtifactVersion<'a> {

    fn eq(&self, other: &Maven3ArtifactVersion) -> bool {
        self.canonical == other.canonical
    }
}

// Ord
//
impl<'a> Ord for Maven3ArtifactVersion<'a> {

    fn cmp(&self, other: &Maven3ArtifactVersion) -> Ordering {
        compare_all_items(&self.items, &other.items)
    }
}

fn compare_all_items(first: &[Item], second: &[Item]) -> Ordering {
    use self::Item::*;

    let first_length = first.len();
    let second_length = second.len();

    for index in 0..cmp::max(first_length, second_length) { 
        let left: Option<&Item> = first.get(index);
        let right: Option<&Item> = second.get(index);

        let result = match left {
            Some(&Integer(i)) => compare_integer_with(i, right),
            Some(&Str(ref s)) => compare_str_with(s, right),
            Some(&Minus) => {
                match right {
                    Some(&Integer(_)) => Ordering::Less,  // 1-1 < 1.0.x
                    Some(&Str(_)) => Ordering::Greater, // 1-1 > 1-sp
                    Some(&Minus) => compare_all_items(&first[(index + 1)..], &second[(index + 1)..]),
                    None => compare_all_items(&first[(index + 1)..], &second[index..]),
                }
            },
            None => {
                let result_ordering = match right {
                    Some(_) => compare_all_items(&second[index..], &first[index..]),
                    None => Ordering::Equal,
                };

                match result_ordering {
                    Ordering::Greater => Ordering::Less,
                    Ordering::Less => Ordering::Greater,
                    _ => Ordering::Equal,
                } 
            },
        };

        if result != Ordering::Equal {
            return result;
        }
    }

    Ordering::Equal
}

fn compare_integer_with(value: u32, item: Option<&Item>) -> Ordering {
    use self::Item::*;

    match item {
        Some(&Integer(ref i)) => value.cmp(i),
        Some(&Str(_)) | Some(&Minus) => Ordering::Greater, // 1.1 > 1-sp | 1.1 > 1-1
        None => {
            // 1.0 == 1, 1.1 > 1
            if value == 0 {
                Ordering::Equal
            } else {
                Ordering::Greater
            }
        },
    }
}

fn compare_str_with(value: &str, item: Option<&Item>) -> Ordering {
    use self::Item::*;

    match item {
        Some(&Integer(_)) | Some(&Minus) => Ordering::Less, // 1.any < 1.1 ? | 1.any < 1-1
        Some(&Str(ref s)) => comparable_str_qualifier(value).cmp(&comparable_str_qualifier(s)),
        None => {
            // 1-rc < 1, 1-ga > 1
            let cmp_qualifier: &str = &comparable_str_qualifier(value);
            cmp_qualifier.cmp(RELEASE_VERSION_INDEX)
        },
    }
}

impl<'a> PartialOrd for Maven3ArtifactVersion<'a> {

    fn partial_cmp(&self, other: &Maven3ArtifactVersion) -> Option<Ordering> {
        Some(self.cmp(other))
    }
}

// Hash
//
impl<'a> hash::Hash for Maven3ArtifactVersion<'a> {
    
    fn hash<H: hash::Hasher>(&self, state: &mut H) {
        self.canonical.hash(state);
    }
}

// Display
//
impl<'a> fmt::Display for Maven3ArtifactVersion<'a> {
    
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, "{}", self.version)
    }
}

fn make_canonical(items: &[Item]) -> String {
    let mut buffer = String::new();

    let mut prev_val = false;
    for item in items {
        match *item {
            Item::Integer(i) => {
                if prev_val {
                    buffer.push('.');
                }                    
                prev_val = true;
                buffer.push_str(&format!("{}", i));
            },
            Item::Str(ref s) => {
                if prev_val {
                    buffer.push('.');
                }                    
                prev_val = true;
                buffer.push_str(s);
            },
            Item::Minus => {
                prev_val = false;
                buffer.push('-');
            },
        };
    }

    buffer
}

// ---------------------------------------
// Item
// ---------------------------------------

#[derive(Debug)]
enum Item {
    Integer(u32),
    Str(String),
    Minus, // list equivalent
}

impl Item {

    fn is_minus(&self) -> bool {
        match *self {
            Item::Minus => true,
            _ => false,
        }
    }    
}

impl fmt::Display for Item {
    
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match *self {
            Item::Integer(i) => write!(f, "{}", i),
            Item::Str(ref s) => write!(f, "{}", s),
            Item::Minus => write!(f, "-"),
        }
    }
}

fn comparable_str_qualifier(qualifier: &str) -> String {
    match qualifier {
        "alpha" => "0".to_owned(),
        "beta" => "1".to_owned(),
        "milestone" => "2".to_owned(),
        "rc" => "3".to_owned(),
        "snapshot" => "4".to_owned(),
        "" => "5".to_owned(),
        "sp" => "6".to_owned(),
        _ => format!("7-{}", qualifier), // length of all special qualifiers + qualifier
    }
}

fn parse_items(version: &str) -> Vec<Item> {
    let version = version.to_lowercase();
    let mut items: Vec<Item> = Vec::new();
    
    let mut is_digit = false;
    let mut start_index = 0;

    let chars: Vec<char> = version.chars().collect();
    for (index, c) in chars.iter().enumerate() {
        if *c == '.' {
            if index == start_index {
                items.push(Item::Integer(0));
            } else {
                let substring: String = chars[start_index..index].into_iter().collect();
                items.push(parse_item(is_digit, &substring));
            }
            start_index = index + 1;

        } else if *c == '-' {
            if index == start_index {
                items.push(Item::Integer(0));
            } else {
                let substring: String = chars[start_index..index].into_iter().collect();
                items.push(parse_item(is_digit, &substring));
            }

            start_index = index + 1;
            items.push(Item::Minus);

        } else if c.is_digit(10) {
            if !is_digit && index > start_index {
                let substring: String = chars[start_index..index].into_iter().collect();
                items.push(to_string_item(substring, true));

                start_index = index;
                items.push(Item::Minus);
            }
            is_digit = true;

        } else {
            if is_digit && index > start_index {
                let substring: String = chars[start_index..index].into_iter().collect();
                items.push(parse_item(true, substring));

                start_index = index;
                items.push(Item::Minus);
            }
            is_digit = false;
        }
    }

    if chars.len() > start_index {
        let substring: String = chars[start_index..].into_iter().collect();
        items.push(parse_item(is_digit, substring));
    }

    normalize(&mut items);

    items
}

fn parse_item<T: AsRef<str>>(is_digit: bool, buf: T) -> Item {
    if is_digit {
        to_integer_item(buf.as_ref())
    } else {
        to_string_item(buf, false)
    }
}

fn to_integer_item(value: &str) -> Item {
    Item::Integer(value.parse::<u32>().unwrap())
}

fn to_string_item<T: AsRef<str>>(value: T, followed_by_digit: bool) -> Item {
    let mut value: &str = value.as_ref();
    if followed_by_digit && value.chars().count() == 1 {
        value = match value.chars().nth(0) {
            Some('a') => "alpha",
            Some('b') => "beta",
            Some('m') => "milestone",
            _ => value,
        }
    }
    value = match value {
        "ga" | "final" => "",
        "cr" => "rc",
        _ => value,
    };
    Item::Str(value.to_string())
}

// Splits all items at Item::Minus and normalize
fn normalize(items: &mut Vec<Item>) {
    let mut start_index = items.len() - 1;

    for index in (0..items.len()).rev() {
        if items[index].is_minus() {
            normalize_sublist(items, (index + 1), start_index + 1);
            start_index = index;
        }      
    }

    normalize_sublist(items, 0, start_index + 1);
}

fn normalize_sublist(items: &mut Vec<Item>, start_index: usize, end_index: usize) {
    for index in (start_index..end_index).rev() {
        // check for null
        let is_null = match items[index] {
            Item::Minus => (items.len() - index) <= 1, // minus only in list
            Item::Integer(i) => i == 0,
            Item::Str(ref s) => comparable_str_qualifier(s) == RELEASE_VERSION_INDEX,
        };

        if is_null {
            // remove null trailing items: 0, "", empty list
            items.remove(index);
        } else if !items[index].is_minus() {
            break;
        }
    }
}

#[cfg(test)]
mod tests {

    // see https://github.com/apache/maven/blob/master/maven-artifact/src/test/java/org/apache/maven/artifact/versioning/ComparableVersionTest.java

    use super::*;
    use std::hash::{Hash, Hasher};
    use std::collections::hash_map::DefaultHasher;

    const VERSIONS_QUALIFIER: [&'static str; 22] = ["1-alpha2snapshot", "1-alpha2", "1-alpha-123", "1-beta-2", "1-beta123", "1-m2", "1-m11", "1-rc", "1-cr2",
            "1-rc123", "1-SNAPSHOT", "1", "1-sp", "1-sp2", "1-sp123", "1-abc", "1-def", "1-pom-1", "1-1-snapshot", "1-1", "1-2", "1-123" ];

    const VERSIONS_NUMBER: [&'static str; 25] = ["2.0", "2-1", "2.0.a", "2.0.0.a", "2.0.2", "2.0.123", "2.1.0", "2.1-a", "2.1b", "2.1-c", "2.1-1", "2.1.0.1",
            "2.2", "2.123", "11.a2", "11.a11", "11.b2", "11.b11", "11.m2", "11.m11", "11", "11.a", "11b", "11c", "11m"];

    fn new_artifact_version(version: &str) -> Maven3ArtifactVersion {
        let artifact_version = Maven3ArtifactVersion::new(version);

        {
            let canonical = artifact_version.canonical();

            let parsed_artifact_version = Maven3ArtifactVersion::new(canonical);
            let parsed_canonical = parsed_artifact_version.canonical();

            //println!("canonical( {} ) = {}", version, canonical);
            assert_eq!(canonical, parsed_canonical, "canonical( {} ) = {} -> canonical: {}", version, canonical, parsed_canonical);
        }

        artifact_version
    }

    fn calc_hash<T: Hash>(t: &T) -> u64 {
        let mut s = DefaultHasher::new();
        t.hash(&mut s);
        s.finish()
    }

    fn check_versions_equal(v1: &str, v2: &str) {
        let c1 = new_artifact_version(v1);
        let c2 = new_artifact_version(v2);

        assert!(c1.cmp(&c2) == Ordering::Equal, "expected {} == {}", v1, v2);
        assert!(c2.cmp(&c1) == Ordering::Equal, "expected {} == {}", v2, v1);
        assert!(calc_hash(&c1) == calc_hash(&c2), "expected same hashcode for {} and {}", v1, v2);
        assert!(c1 == c2, "expected {} == {}", v1, v2);
        assert!(c2 == c1, "expected {} == {}", v2, v1);
    }

    fn check_versions_order(v1: &str, v2: &str) {
        let c1 = new_artifact_version(v1);
        let c2 = new_artifact_version(v2);
        assert!(c1 < c2, "expected {} < {}", v1, v2);
        assert!(c2 > c1, "expected {} > {}", v2, v1);
    }

    fn check_all_versions_order(versions: &[&'static str]) {
        let c: Vec<Maven3ArtifactVersion> = versions.iter().map(|version| Maven3ArtifactVersion::new(version)).collect();
        for i in 1..versions.len() {
            let low = &c[i - 1];

            for j in i..versions.len() {
                let high = &c[j];
                assert!(low < high, "expected {} < {}", low, high);
                assert!(high > low, "expected {} > {}", high, low);
            }
        }
    }

    #[test]
    fn test_versions_qualifier() {
        check_all_versions_order(&VERSIONS_QUALIFIER);
    }

    #[test]
    fn test_versions_number() {
        check_all_versions_order(&VERSIONS_NUMBER);
    }

    #[test]
    fn test_versions_equal() {
        new_artifact_version( "1.0-alpha" );
        check_versions_equal( "1", "1" );
        check_versions_equal( "1", "1.0" );
        check_versions_equal( "1", "1.0.0" );
        check_versions_equal( "1.0", "1.0.0" );
        check_versions_equal( "1", "1-0" );
        check_versions_equal( "1", "1.0-0" );
        check_versions_equal( "1.0", "1.0-0" );
        // no separator between number and character
        check_versions_equal( "1a", "1-a" );
        check_versions_equal( "1a", "1.0-a" );
        check_versions_equal( "1a", "1.0.0-a" );
        check_versions_equal( "1.0a", "1-a" );
        check_versions_equal( "1.0.0a", "1-a" );
        check_versions_equal( "1x", "1-x" );
        check_versions_equal( "1x", "1.0-x" );
        check_versions_equal( "1x", "1.0.0-x" );
        check_versions_equal( "1.0x", "1-x" );
        check_versions_equal( "1.0.0x", "1-x" );

        // aliases
        check_versions_equal( "1ga", "1" );
        check_versions_equal( "1final", "1" );
        check_versions_equal( "1cr", "1rc" );

        // special "aliases" a, b and m for alpha, beta and milestone
        check_versions_equal( "1a1", "1-alpha-1" );
        check_versions_equal( "1b2", "1-beta-2" );
        check_versions_equal( "1m3", "1-milestone-3" );

        // case insensitive
        check_versions_equal( "1X", "1x" );
        check_versions_equal( "1A", "1a" );
        check_versions_equal( "1B", "1b" );
        check_versions_equal( "1M", "1m" );
        check_versions_equal( "1Ga", "1" );
        check_versions_equal( "1GA", "1" );
        check_versions_equal( "1Final", "1" );
        check_versions_equal( "1FinaL", "1" );
        check_versions_equal( "1FINAL", "1" );
        check_versions_equal( "1Cr", "1Rc" );
        check_versions_equal( "1cR", "1rC" );
        check_versions_equal( "1m3", "1Milestone3" );
        check_versions_equal( "1m3", "1MileStone3" );
        check_versions_equal( "1m3", "1MILESTONE3" );        
    }
    
    #[test]
    fn test_version_comparing() {
        check_versions_order( "1", "2" );
        check_versions_order( "1.5", "2" );
        check_versions_order( "1", "2.5" );
        check_versions_order( "1.0", "1.1" );
        check_versions_order( "1.1", "1.2" );
        check_versions_order( "1.0.0", "1.1" );
        check_versions_order( "1.0.1", "1.1" );
        check_versions_order( "1.1", "1.2.0" );

        check_versions_order( "1.0-alpha-1", "1.0" );
        check_versions_order( "1.0-alpha-1", "1.0-alpha-2" );
        check_versions_order( "1.0-alpha-1", "1.0-beta-1" );

        check_versions_order( "1.0-beta-1", "1.0-SNAPSHOT" );
        check_versions_order( "1.0-SNAPSHOT", "1.0" );
        check_versions_order( "1.0-alpha-1-SNAPSHOT", "1.0-alpha-1" );

        check_versions_order( "1.0", "1.0-1" );
        check_versions_order( "1.0-1", "1.0-2" );
        check_versions_order( "1.0.0", "1.0-1" );

        check_versions_order( "2.0-1", "2.0.1" );
        check_versions_order( "2.0.1-klm", "2.0.1-lmn" );
        check_versions_order( "2.0.1", "2.0.1-xyz" );

        check_versions_order( "2.0.1", "2.0.1-123" );
        check_versions_order( "2.0.1-xyz", "2.0.1-123" );
    }

    #[test]
    fn test_mng5568() {
        let a = "6.1.0";
        let b = "6.1.0rc3";
        let c = "6.1H.5-beta"; // this is the unusual version string, with 'H' in the middle

        check_versions_order( b, a ); // classical
        check_versions_order( b, c ); // now b < c, but before MNG-5568, we had b > c
        check_versions_order( a, c );
    }
}