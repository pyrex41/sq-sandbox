package squashd

import "core:strings"

_is_id_char :: proc(ch: rune) -> bool {
	return (ch >= 'a' && ch <= 'z') ||
		(ch >= 'A' && ch <= 'Z') ||
		(ch >= '0' && ch <= '9') ||
		ch == '-' ||
		ch == '_'
}

_is_label_char :: proc(ch: rune) -> bool {
	return _is_id_char(ch) || ch == '.'
}

valid_id :: proc(s: string) -> bool {
	if len(s) == 0 {
		return false
	}
	if strings.contains(s, "..") || strings.contains(s, "/") {
		return false
	}
	for ch in s {
		if !_is_id_char(ch) {
			return false
		}
	}
	return true
}

valid_label :: proc(s: string) -> bool {
	if len(s) == 0 {
		return false
	}
	if strings.contains(s, "..") || strings.contains(s, "/") {
		return false
	}
	for ch in s {
		if !_is_label_char(ch) {
			return false
		}
	}
	return true
}

valid_module :: proc(s: string) -> bool {
	return valid_label(s)
}
