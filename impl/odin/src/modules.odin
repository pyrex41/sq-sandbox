package squashd

import "core:fmt"
import "core:os"
import "core:strings"

module_exists :: proc(config: ^Config, name: string) -> bool {
	if !valid_module(name) {
		return false
	}
	path := fmt.tprintf("%s/%s.squashfs", modules_dir(config), name)
	return os.exists(path)
}

list_modules :: proc(config: ^Config, allocator := context.allocator) -> []string {
	mods_dir := modules_dir(config)
	dh, err := os.open(mods_dir)
	if err != nil {
		return nil
	}
	defer os.close(dh)

	entries, read_err := os.read_dir(dh, -1)
	if read_err != nil {
		return nil
	}
	defer delete(entries)

	result := make([dynamic]string, allocator = allocator)
	for entry in entries {
		if entry.is_dir {
			continue
		}
		name := _basename(entry.fullpath)
		if !strings.has_suffix(name, ".squashfs") {
			continue
		}
		mod := name[:len(name) - len(".squashfs")]
		if valid_module(mod) {
			append(&result, mod)
		}
	}
	return result[:]
}
