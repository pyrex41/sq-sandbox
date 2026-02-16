/// Sandbox IDs: [a-zA-Z0-9_-]+
pub fn valid_id(s: &str) -> bool {
    !s.is_empty() && s.chars().all(|c| c.is_ascii_alphanumeric() || c == '_' || c == '-')
}

/// Snapshot labels and module names: [a-zA-Z0-9_.-]+
pub fn valid_label(s: &str) -> bool {
    !s.is_empty() && s.chars().all(|c| c.is_ascii_alphanumeric() || c == '_' || c == '.' || c == '-')
}

/// Module names follow the same rules as labels.
pub fn valid_module(s: &str) -> bool {
    valid_label(s)
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_valid_id() {
        assert!(valid_id("dev"));
        assert!(valid_id("my-sandbox"));
        assert!(valid_id("test_123"));
        assert!(valid_id("ABC-xyz_09"));
        assert!(!valid_id(""));
        assert!(!valid_id("../../etc"));
        assert!(!valid_id("a b"));
        assert!(!valid_id("a/b"));
        assert!(!valid_id("a.b")); // dots not allowed in IDs
    }

    #[test]
    fn test_valid_label() {
        assert!(valid_label("snapshot-1"));
        assert!(valid_label("v2.0"));
        assert!(valid_label("000-base-alpine"));
        assert!(valid_label("my_module.v3"));
        assert!(!valid_label(""));
        assert!(!valid_label("../etc"));
        assert!(!valid_label("a b"));
        assert!(!valid_label("a/b"));
    }

    #[test]
    fn test_valid_module() {
        assert!(valid_module("000-base-alpine"));
        assert!(valid_module("100-python312"));
        assert!(!valid_module(""));
        assert!(!valid_module("mod/evil"));
    }

    #[test]
    fn test_id_rejects_path_traversal() {
        assert!(!valid_id(".."));
        assert!(!valid_id("../../etc"));
        assert!(!valid_id("foo/bar"));
        assert!(!valid_id("foo\\bar"));
    }

    #[test]
    fn test_label_rejects_path_traversal() {
        assert!(!valid_label("../etc"));
        assert!(!valid_label("foo/bar"));
        assert!(!valid_label("foo\\bar"));
    }

    #[test]
    fn test_id_rejects_special_chars() {
        assert!(!valid_id("hello world"));
        assert!(!valid_id("id@host"));
        assert!(!valid_id("a;b"));
        assert!(!valid_id("$(cmd)"));
        assert!(!valid_id("a\nb"));
        assert!(!valid_id("a\0b"));
    }

    #[test]
    fn test_label_allows_dots() {
        assert!(valid_label("v1.0"));
        assert!(valid_label("snapshot.2024"));
        assert!(valid_label("a.b.c"));
    }

    #[test]
    fn test_id_does_not_allow_dots() {
        assert!(!valid_id("v1.0"));
        assert!(!valid_id("a.b"));
    }

    #[test]
    fn test_single_char_inputs() {
        assert!(valid_id("a"));
        assert!(valid_id("0"));
        assert!(valid_id("_"));
        assert!(valid_id("-"));
        assert!(valid_label("a"));
        assert!(valid_label("."));
    }

    #[test]
    fn test_module_delegates_to_label() {
        // valid_module should accept exactly the same inputs as valid_label
        let cases = vec![
            ("000-base-alpine", true),
            ("v1.2.3", true),
            ("my_module.v3", true),
            ("", false),
            ("mod/evil", false),
            ("a b", false),
        ];
        for (input, expected) in cases {
            assert_eq!(
                valid_module(input),
                expected,
                "valid_module({:?}) should be {}",
                input,
                expected
            );
            assert_eq!(
                valid_label(input),
                expected,
                "valid_label({:?}) should be {}",
                input,
                expected
            );
        }
    }
}
