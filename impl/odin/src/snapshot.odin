package squashd

import "core:fmt"
import "core:os"
import "core:time"

generate_snapshot_label :: proc() -> string {
	now := time.now()
	y, mon, d := time.date(now)
	h, m, s := time.clock(now)
	return fmt.tprintf("%04d%02d%02d-%02d%02d%02d", y, int(mon), d, h, m, s)
}

snapshot_file_path :: proc(sandbox_dir: string, label: string) -> string {
	return fmt.tprintf("%s/snapshots/%s.squashfs", sandbox_dir, label)
}

snapshot_create :: proc(sandbox: ^Sandbox, label: string) -> (size: u64, ok: bool) {
	upper_data := fmt.tprintf("%s/upper/data", sandbox.dir)
	snap_dir := fmt.tprintf("%s/snapshots", sandbox.dir)
	if !os.exists(snap_dir) {
		_ensure_dir_recursive(snap_dir)
	}
	snap_path := snapshot_file_path(sandbox.dir, label)
	if os.exists(snap_path) {
		return 0, false
	}
	if !run_cmd("mksquashfs", upper_data, snap_path, "-comp", "zstd", "-b", "256K", "-noappend", "-quiet") {
		return 0, false
	}
	info, info_err := os.stat(snap_path)
	if info_err != nil {
		return 0, true
	}
	return u64(info.size), true
}
