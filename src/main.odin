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

	manager := manager_init(&config, context.allocator)
	defer manager_destroy(&manager)
	manager_recover_from_disk(&manager)

	_ = reaper_start(&manager)
	api_init(&config, &manager)

	fmt.printfln("[main] squashd (odin) ready on :%d", config.port)
	serve(config.port, api_routes(), &config, &manager)
}
