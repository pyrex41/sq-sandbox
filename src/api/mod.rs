mod handlers;
mod middleware;
mod models;

use std::sync::Arc;

use axum::middleware as axum_mw;
use axum::routing::{get, post};
use axum::Router;
use tower_http::services::ServeDir;

use crate::config::Config;
use crate::s3::S3Store;
use crate::sandbox::manager::SandboxManager;

/// Shared application state available to all handlers.
#[derive(Clone)]
pub struct AppState {
    pub config: Arc<Config>,
    pub sandbox_manager: Arc<SandboxManager>,
    pub s3: Option<Arc<S3Store>>,
}

/// Build the complete API router with auth and content-type middleware.
pub fn router(
    config: Arc<Config>,
    sandbox_manager: Arc<SandboxManager>,
    s3: Option<Arc<S3Store>>,
) -> Router {
    let state = AppState {
        config,
        sandbox_manager,
        s3,
    };

    // Routes that require authentication (when SQUASH_AUTH_TOKEN is set)
    let api_routes = Router::new()
        .route("/cgi-bin/api/sandboxes", get(handlers::list_sandboxes).post(handlers::create_sandbox))
        .route(
            "/cgi-bin/api/sandboxes/{id}",
            get(handlers::get_sandbox).delete(handlers::destroy_sandbox),
        )
        .route("/cgi-bin/api/sandboxes/{id}/exec", post(handlers::exec_in_sandbox))
        .route("/cgi-bin/api/sandboxes/{id}/activate", post(handlers::activate_module))
        .route("/cgi-bin/api/sandboxes/{id}/snapshot", post(handlers::snapshot_sandbox))
        .route("/cgi-bin/api/sandboxes/{id}/restore", post(handlers::restore_sandbox))
        .route("/cgi-bin/api/sandboxes/{id}/logs", get(handlers::get_logs))
        .route("/cgi-bin/api/modules", get(handlers::list_modules))
        .layer(axum_mw::from_fn_with_state(
            state.clone(),
            middleware::require_auth,
        ));

    // Health endpoint — no auth required
    let public_routes = Router::new().route("/cgi-bin/health", get(handlers::health));

    public_routes
        .merge(api_routes)
        // Content-Type enforcement applies to all POST requests
        .layer(axum_mw::from_fn(middleware::require_json_content_type))
        // Serve static files from /app/static as fallback for unmatched routes
        .fallback_service(ServeDir::new("/app/static"))
        .with_state(state)
}
