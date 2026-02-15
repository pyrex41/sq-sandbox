mod api;
mod config;
mod init;
mod modules;
mod proxy;
mod reaper;
mod s3;
mod sandbox;
mod validate;

use std::sync::Arc;

use tokio::net::TcpListener;
use tracing::{info, warn};

use config::Config;
use s3::{S3Config, S3Store};

#[tokio::main]
async fn main() {
    // 1. Init tracing
    tracing_subscriber::fmt()
        .with_env_filter(
            tracing_subscriber::EnvFilter::try_from_default_env()
                .unwrap_or_else(|_| "sq_sandbox=info,tower_http=info".parse().unwrap()),
        )
        .init();

    // 2. Parse config
    let config = Arc::new(Config::from_env());

    // 3. Set up S3 store (if configured)
    let s3 = if config.s3_enabled() {
        let s3_config = S3Config {
            bucket: config.s3_bucket.clone().unwrap(),
            endpoint: config.s3_endpoint.clone(),
            region: config.s3_region.clone(),
            prefix: config.s3_prefix.clone(),
        };
        Some(Arc::new(S3Store::new(&s3_config).await))
    } else {
        None
    };

    // 4. Create sandbox manager
    let sandbox_manager = Arc::new(sandbox::manager::SandboxManager::new(config.max_sandboxes));

    // 5. Run init/recovery and register recovered sandboxes
    let init_result = init::run(&config, s3.as_deref(), Some(&sandbox_manager))
        .await
        .expect("init failed");

    // 6. Start secret proxy on :8888 (if secrets.json exists)
    if config.secrets_path().is_file() {
        match proxy::ProxyState::new(&config.data_dir) {
            Ok(state) => {
                let state = Arc::new(state);
                match proxy::start(state).await {
                    Ok(addr) => info!("secret proxy started on {}", addr),
                    Err(e) => warn!("secret proxy failed to start: {}", e),
                }
            }
            Err(e) => warn!("secret proxy init failed: {}", e),
        }
    }

    // 7. Start Tailscale (if authkey set, shell out to setup-tailscale)
    if config.tailscale_authkey.is_some() {
        info!("starting tailscale setup");
        tokio::spawn(async {
            match tokio::process::Command::new("setup-tailscale")
                .status()
                .await
            {
                Ok(status) if status.success() => info!("tailscale setup complete"),
                Ok(status) => warn!("setup-tailscale exited with {}", status),
                Err(e) => warn!("setup-tailscale failed: {}", e),
            }
        });
    }

    // 8. Start reaper background task
    reaper::spawn(
        Arc::clone(&sandbox_manager),
        Arc::clone(&config),
        s3.clone(),
    );

    // 9. Build API router and start server
    let port = config.port;
    let app = api::router(Arc::clone(&config), Arc::clone(&sandbox_manager), s3.clone());

    let listener = TcpListener::bind(("0.0.0.0", port))
        .await
        .expect("failed to bind API listener");

    // 10. Log ready message with module/sandbox counts
    info!(
        "squash v4 ready — modules: {}, sandboxes: {}, port: {}",
        init_result.module_count, init_result.sandbox_count, port
    );

    axum::serve(listener, app).await.expect("server error");
}
