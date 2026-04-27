package controlplane

import (
	_ "embed"
	"os"
	"path/filepath"
)

//go:embed shen_backpressure.shen
var DefaultShenBackpressurePolicy []byte

func EnsureShenPolicy(path string) error {
	if path == "" {
		return nil
	}
	if _, err := os.Stat(path); err == nil {
		return nil
	}
	if err := os.MkdirAll(filepath.Dir(path), 0755); err != nil {
		return err
	}
	return os.WriteFile(path, DefaultShenBackpressurePolicy, 0644)
}
