pub mod builder;

use std::path::{Path, PathBuf};

use tracing::debug;

/// Check if a module exists locally.
pub fn module_exists(modules_dir: &Path, name: &str) -> bool {
    modules_dir.join(format!("{}.squashfs", name)).is_file()
}

/// List all locally available modules (by scanning *.squashfs files).
pub fn list_modules(modules_dir: &Path) -> Vec<String> {
    let mut modules = Vec::new();
    let entries = match std::fs::read_dir(modules_dir) {
        Ok(entries) => entries,
        Err(_) => return modules,
    };
    for entry in entries.flatten() {
        let path = entry.path();
        if path.extension().and_then(|e| e.to_str()) == Some("squashfs") {
            if let Some(stem) = path.file_stem().and_then(|s| s.to_str()) {
                modules.push(stem.to_string());
            }
        }
    }
    modules.sort();
    debug!("found {} modules in {}", modules.len(), modules_dir.display());
    modules
}

/// Path to a module's squashfs file.
pub fn module_path(modules_dir: &Path, name: &str) -> PathBuf {
    modules_dir.join(format!("{}.squashfs", name))
}

/// Path to a module's version file.
pub fn version_path(modules_dir: &Path, name: &str) -> PathBuf {
    modules_dir.join(format!("{}.version", name))
}

/// Read the version number from a module's .version file.
/// Returns 0 if the file doesn't exist or can't be parsed.
pub fn read_version(modules_dir: &Path, name: &str) -> u32 {
    let path = version_path(modules_dir, name);
    match std::fs::read_to_string(&path) {
        Ok(content) => content.trim().parse().unwrap_or(0),
        Err(_) => 0,
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use std::fs;

    #[test]
    fn test_module_exists() {
        let dir = tempfile::tempdir().unwrap();
        assert!(!module_exists(dir.path(), "foo"));

        fs::write(dir.path().join("foo.squashfs"), b"fake").unwrap();
        assert!(module_exists(dir.path(), "foo"));
    }

    #[test]
    fn test_list_modules() {
        let dir = tempfile::tempdir().unwrap();
        fs::write(dir.path().join("000-base-alpine.squashfs"), b"a").unwrap();
        fs::write(dir.path().join("100-python312.squashfs"), b"b").unwrap();
        fs::write(dir.path().join("not-a-module.txt"), b"c").unwrap();

        let mods = list_modules(dir.path());
        assert_eq!(mods, vec!["000-base-alpine", "100-python312"]);
    }

    #[test]
    fn test_read_version() {
        let dir = tempfile::tempdir().unwrap();

        // No version file → 0
        assert_eq!(read_version(dir.path(), "foo"), 0);

        // Valid version file
        fs::write(dir.path().join("foo.version"), "2\n").unwrap();
        assert_eq!(read_version(dir.path(), "foo"), 2);

        // Invalid content → 0
        fs::write(dir.path().join("bar.version"), "notanumber").unwrap();
        assert_eq!(read_version(dir.path(), "bar"), 0);
    }

    #[test]
    fn test_module_path() {
        let dir = Path::new("/data/modules");
        assert_eq!(
            module_path(dir, "000-base-alpine"),
            PathBuf::from("/data/modules/000-base-alpine.squashfs")
        );
    }
}
