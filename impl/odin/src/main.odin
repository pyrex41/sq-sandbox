package squashd

import "core:fmt"
import "core:os"

main :: proc() {
	config := config_from_env(context.allocator)

	init_err := init_run(&config)
	if init_err != .None {
		fmt.eprintfln("[main] init failed: %v", init_err)
		os.exit(1)
	}

	// Start sq-sync sidecar (if available and not already running)
	if !os.exists(bus_sock_path(&config)) && _command_exists("sq-sync") {
		_ = run_cmd("sq-sync", "--daemon")
		fmt.eprintln("[main] sq-sync sidecar started")
	}

	manager := manager_init(&config, context.allocator)
	defer manager_destroy(&manager)
	manager_recover_from_disk(&manager)

	_ = reaper_start(&manager)
	api_init(&config, &manager)

	fmt.printfln("[main] squashd (odin) ready on :%d", config.port)
	serve(config.port, api_routes(), &config, &manager)
}
