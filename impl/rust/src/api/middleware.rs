use axum::body::Body;
use axum::extract::State;
use axum::http::{header, Method, Request, StatusCode};
use axum::middleware::Next;
use axum::response::{IntoResponse, Response};
use axum::Json;

use super::models::ErrorResponse;
use super::AppState;

/// Authentication middleware.
///
/// If `Config::auth_token` is `Some`, require `Authorization: Bearer {token}` on every request.
/// Returns 401 if the header is missing or doesn't match.
pub async fn require_auth(
    State(state): State<AppState>,
    request: Request<Body>,
    next: Next,
) -> Response {
    if let Some(ref expected) = state.config.auth_token {
        let auth_header = request
            .headers()
            .get(header::AUTHORIZATION)
            .and_then(|v| v.to_str().ok());

        match auth_header {
            Some(value) if value.strip_prefix("Bearer ").is_some_and(|t| t == expected) => {}
            _ => {
                return (
                    StatusCode::UNAUTHORIZED,
                    Json(ErrorResponse {
                        error: "unauthorized".to_string(),
                    }),
                )
                    .into_response();
            }
        }
    }

    next.run(request).await
}

/// Content-Type enforcement middleware for POST requests.
///
/// Returns 415 Unsupported Media Type if a POST request doesn't have
/// `Content-Type: application/json`.
pub async fn require_json_content_type(request: Request<Body>, next: Next) -> Response {
    if request.method() == Method::POST {
        let has_json_ct = request
            .headers()
            .get(header::CONTENT_TYPE)
            .and_then(|v| v.to_str().ok())
            .is_some_and(|ct| ct.starts_with("application/json"));

        if !has_json_ct {
            return (
                StatusCode::UNSUPPORTED_MEDIA_TYPE,
                Json(ErrorResponse {
                    error: "Content-Type must be application/json".to_string(),
                }),
            )
                .into_response();
        }
    }

    next.run(request).await
}

#[cfg(test)]
mod tests {
    use super::*;
    use axum::body::Body;
    use axum::http::{Request, StatusCode};
    use axum::middleware as axum_mw;
    use axum::routing::post;
    use axum::Router;
    use std::sync::Arc;
    use tower::ServiceExt;

    use crate::config::Config;
    use crate::sandbox::manager::SandboxManager;

    fn test_state(token: Option<&str>) -> AppState {
        let config = Arc::new(Config {
            auth_token: token.map(String::from),
            ..Config::default()
        });
        AppState {
            config,
            sandbox_manager: Arc::new(SandboxManager::new(0)),
            s3: None,
        }
    }

    // -- Auth middleware tests --

    #[tokio::test]
    async fn auth_passes_when_no_token_configured() {
        let state = test_state(None);
        let app = Router::new()
            .route("/test", post(|| async { "ok" }))
            .layer(axum_mw::from_fn_with_state(state.clone(), require_auth))
            .with_state(state);

        let resp = app
            .oneshot(
                Request::builder()
                    .method("POST")
                    .uri("/test")
                    .body(Body::empty())
                    .unwrap(),
            )
            .await
            .unwrap();

        assert_eq!(resp.status(), StatusCode::OK);
    }

    #[tokio::test]
    async fn auth_rejects_missing_header() {
        let state = test_state(Some("secret-123"));
        let app = Router::new()
            .route("/test", post(|| async { "ok" }))
            .layer(axum_mw::from_fn_with_state(state.clone(), require_auth))
            .with_state(state);

        let resp = app
            .oneshot(
                Request::builder()
                    .method("POST")
                    .uri("/test")
                    .body(Body::empty())
                    .unwrap(),
            )
            .await
            .unwrap();

        assert_eq!(resp.status(), StatusCode::UNAUTHORIZED);
    }

    #[tokio::test]
    async fn auth_rejects_wrong_token() {
        let state = test_state(Some("secret-123"));
        let app = Router::new()
            .route("/test", post(|| async { "ok" }))
            .layer(axum_mw::from_fn_with_state(state.clone(), require_auth))
            .with_state(state);

        let resp = app
            .oneshot(
                Request::builder()
                    .method("POST")
                    .uri("/test")
                    .header("Authorization", "Bearer wrong-token")
                    .body(Body::empty())
                    .unwrap(),
            )
            .await
            .unwrap();

        assert_eq!(resp.status(), StatusCode::UNAUTHORIZED);
    }

    #[tokio::test]
    async fn auth_accepts_correct_token() {
        let state = test_state(Some("secret-123"));
        let app = Router::new()
            .route("/test", post(|| async { "ok" }))
            .layer(axum_mw::from_fn_with_state(state.clone(), require_auth))
            .with_state(state);

        let resp = app
            .oneshot(
                Request::builder()
                    .method("POST")
                    .uri("/test")
                    .header("Authorization", "Bearer secret-123")
                    .body(Body::empty())
                    .unwrap(),
            )
            .await
            .unwrap();

        assert_eq!(resp.status(), StatusCode::OK);
    }

    // -- Content-Type middleware tests --

    #[tokio::test]
    async fn ct_allows_get_without_content_type() {
        let app = Router::new()
            .route("/test", axum::routing::get(|| async { "ok" }))
            .layer(axum_mw::from_fn(require_json_content_type));

        let resp = app
            .oneshot(
                Request::builder()
                    .method("GET")
                    .uri("/test")
                    .body(Body::empty())
                    .unwrap(),
            )
            .await
            .unwrap();

        assert_eq!(resp.status(), StatusCode::OK);
    }

    #[tokio::test]
    async fn ct_rejects_post_without_content_type() {
        let app = Router::new()
            .route("/test", post(|| async { "ok" }))
            .layer(axum_mw::from_fn(require_json_content_type));

        let resp = app
            .oneshot(
                Request::builder()
                    .method("POST")
                    .uri("/test")
                    .body(Body::empty())
                    .unwrap(),
            )
            .await
            .unwrap();

        assert_eq!(resp.status(), StatusCode::UNSUPPORTED_MEDIA_TYPE);
    }

    #[tokio::test]
    async fn ct_accepts_post_with_json_content_type() {
        let app = Router::new()
            .route("/test", post(|| async { "ok" }))
            .layer(axum_mw::from_fn(require_json_content_type));

        let resp = app
            .oneshot(
                Request::builder()
                    .method("POST")
                    .uri("/test")
                    .header("Content-Type", "application/json")
                    .body(Body::empty())
                    .unwrap(),
            )
            .await
            .unwrap();

        assert_eq!(resp.status(), StatusCode::OK);
    }

    #[tokio::test]
    async fn ct_accepts_post_with_json_charset() {
        let app = Router::new()
            .route("/test", post(|| async { "ok" }))
            .layer(axum_mw::from_fn(require_json_content_type));

        let resp = app
            .oneshot(
                Request::builder()
                    .method("POST")
                    .uri("/test")
                    .header("Content-Type", "application/json; charset=utf-8")
                    .body(Body::empty())
                    .unwrap(),
            )
            .await
            .unwrap();

        assert_eq!(resp.status(), StatusCode::OK);
    }
}
