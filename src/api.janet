# API — HTTP handlers for /cgi-bin/...
#
# Improvements over original:
#   - Content-Length aware body parsing (reads exact bytes)
#   - Bearer token auth middleware on /cgi-bin/api/ paths
#   - Structured route dispatch with sandbox ID extraction
#   - Proper HTTP status text mapping

(import config :prefix "config/")
(import validate :prefix "validate/")
(import manager :prefix "manager/")
(import modules :prefix "modules/")
(import json)

# ── Response helpers ──────────────────────────────────────────────────

(defn json-response [body &opt status]
  (default status 200)
  @{:status status
    :headers @{"Content-Type" "application/json"}
    :body (if (string? body) body (json/encode body))})

(defn error-response [msg &opt status]
  (default status 400)
  (json-response @{:error msg} status))

(defn no-content-response []
  @{:status 204 :headers @{} :body ""})

(def status-texts
  {200 "OK" 201 "Created" 204 "No Content"
   400 "Bad Request" 401 "Unauthorized" 404 "Not Found"
   409 "Conflict" 413 "Payload Too Large"
   500 "Internal Server Error" 503 "Service Unavailable"})

# ── HTTP request parsing ──────────────────────────────────────────────

(defn- read-request
  "Read and parse an HTTP request from stream. Returns table or nil."
  [stream]
  (def buf @"")
  (def max-header-size 8192)

  # Read until we find end of headers (\r\n\r\n)
  (while (nil? (string/find "\r\n\r\n" (string buf)))
    (when (> (length buf) max-header-size) (break))
    (def chunk (try (ev/read stream 4096) ([e] nil)))
    (when (or (nil? chunk) (= (length chunk) 0)) (break))
    (buffer/push buf chunk))

  (def sep (string/find "\r\n\r\n" (string buf)))
  (when (nil? sep) (break nil))

  (def header-str (string/slice buf 0 sep))
  (def body-start (+ sep 4))

  # Parse request line
  (def lines (string/split "\r\n" header-str))
  (when (< (length lines) 1) (break nil))
  (def parts (string/split " " (get lines 0)))
  (when (< (length parts) 2) (break nil))
  (def method (get parts 0))
  (def path (get parts 1))

  # Parse headers into lowercase-keyed table
  (def headers @{})
  (for i 1 (length lines)
    (def line (get lines i))
    (def colon (string/find ":" line))
    (when colon
      (put headers
        (string/ascii-lower (string/trim (string/slice line 0 colon)))
        (string/trim (string/slice line (+ colon 1))))))

  # Read body based on Content-Length
  (def content-length (or (scan-number (get headers "content-length" "0")) 0))
  (def body-buf (buffer/slice buf body-start))
  (when (< (length body-buf) content-length)
    (def remaining (- content-length (length body-buf)))
    (when (> remaining 0)
      (def rest-buf @"")
      (while (< (length rest-buf) remaining)
        (def chunk (try (ev/read stream (min 4096 (- remaining (length rest-buf)))) ([e] nil)))
        (when (or (nil? chunk) (= (length chunk) 0)) (break))
        (buffer/push rest-buf chunk))
      (buffer/push body-buf rest-buf)))

  @{:method method :path path :headers headers :body (string body-buf)})

(defn- parse-json-body
  "Parse JSON from request body. Returns table or nil."
  [req]
  (def body (get req :body ""))
  (when (> (length body) 0)
    (try (json/decode body) ([e] nil))))

# ── Auth middleware ───────────────────────────────────────────────────

(defn- check-auth
  "Check Bearer token auth. Returns true if authorized."
  [req config]
  (def token (get config :auth-token))
  (when (nil? token) (break true))
  (def path (get req :path ""))
  (when (not (string/has-prefix? "/cgi-bin/api/" path)) (break true))
  (def auth (get (get req :headers) "authorization" ""))
  (= auth (string "Bearer " token)))

# ── Route parsing ────────────────────────────────────────────────────

(defn- parse-sandbox-route
  "Parse /cgi-bin/api/sandboxes/:id/:action. Returns [id action] or nil."
  [path]
  (def prefix "/cgi-bin/api/sandboxes/")
  (when (string/has-prefix? prefix path)
    (def rest (string/slice path (length prefix)))
    (when (> (length rest) 0)
      (def slash (string/find "/" rest))
      (if slash
        [(string/slice rest 0 slash) (string/slice rest (+ slash 1))]
        [rest nil]))))

# ── Route handlers ───────────────────────────────────────────────────

(defn- handle-health [config manager]
  (try
    (json-response @{
      :status "ok"
      :backend "chroot"
      :sandboxes (manager/manager-sandbox-count manager)
      :modules (length (modules/list-modules config))
      :base_ready (modules/base-module-exists? config)
    } 200)
    ([e] (json-response @{:status "ok"} 200))))

(defn- handle-list-sandboxes [manager]
  (json-response
    (array/slice (map manager/sandbox-to-info (manager/manager-list manager)))
    200))

(defn- handle-create-sandbox [req config manager]
  (def data (parse-json-body req))
  (when (not data) (break (error-response "invalid json")))
  (def id (get data "id"))
  (when (not (validate/valid-id? id))
    (break (error-response "invalid id")))
  (var layers (get data "layers" "000-base-alpine"))
  (when (string? layers)
    (set layers (string/split "," (string/trim layers))))
  (def opts @{
    :owner (get data "owner" "anon")
    :task (get data "task" "")
    :layers layers
    :cpu (get data "cpu" 2)
    :memory-mb (get data "memory_mb" 1024)
    :max-lifetime-s (get data "max_lifetime_s" 0)
    :allow-net (get data "allow_net")
  })
  (def sandbox (manager/manager-create manager id opts))
  (json-response (manager/sandbox-to-info sandbox) 201))

(defn- handle-get-sandbox [id manager]
  (def s (manager/manager-get manager id))
  (when (not s) (error "not found"))
  (json-response (manager/sandbox-to-info s) 200))

(defn- handle-delete-sandbox [id manager]
  (manager/manager-destroy manager id)
  (no-content-response))

(defn- handle-exec [id req manager]
  (def data (parse-json-body req))
  (def cmd (get data "cmd"))
  (when (not cmd) (break (error-response "cmd required")))
  (def result (manager/manager-exec manager id cmd @{
    :workdir (get data "workdir" "/")
    :timeout (get data "timeout" 300)
  }))
  (json-response @{
    :exit_code (get result :exit_code)
    :stdout (get result :stdout)
    :stderr (get result :stderr)
    :started (get result :started)
    :finished (get result :finished)
    :duration_ms (* (- (get result :finished) (get result :started)) 1000)
    :seq (get result :seq)
  } 200))

(defn- handle-activate [id req manager]
  (def data (parse-json-body req))
  (def mod (get data "module"))
  (when (not mod) (break (error-response "module required")))
  (manager/manager-activate-module manager id mod)
  (json-response (manager/sandbox-to-info (manager/manager-get manager id)) 200))

(defn- handle-snapshot [id req manager]
  (def data (or (parse-json-body req) @{}))
  (def [lbl size] (manager/manager-snapshot manager id (get data "label")))
  (json-response @{:snapshot lbl :size size} 200))

(defn- handle-restore [id req manager]
  (def data (parse-json-body req))
  (def label (get data "label"))
  (when (not label) (break (error-response "label required")))
  (manager/manager-restore manager id label)
  (json-response (manager/sandbox-to-info (manager/manager-get manager id)) 200))

(defn- handle-logs [id manager]
  (json-response (manager/manager-exec-logs manager id) 200))

# ── Request dispatch ─────────────────────────────────────────────────

(defn- dispatch
  "Route a parsed request to the appropriate handler."
  [req config manager]
  (def method (get req :method))
  (def path (get req :path))

  # Strip query string
  (def qmark (string/find "?" path))
  (def clean-path (if qmark (string/slice path 0 qmark) path))

  (cond
    (= clean-path "/cgi-bin/health")
    (handle-health config manager)

    (and (= method "GET") (= clean-path "/cgi-bin/api/modules"))
    (json-response @{:modules (modules/list-modules config)} 200)

    (and (= method "GET") (= clean-path "/cgi-bin/api/sandboxes"))
    (handle-list-sandboxes manager)

    (and (= method "POST") (= clean-path "/cgi-bin/api/sandboxes"))
    (handle-create-sandbox req config manager)

    # Sandbox sub-routes: /cgi-bin/api/sandboxes/:id[/:action]
    (parse-sandbox-route clean-path)
    (let [[id action] (parse-sandbox-route clean-path)]
      (cond
        (and (nil? action) (= method "GET"))
        (handle-get-sandbox id manager)

        (and (nil? action) (= method "DELETE"))
        (handle-delete-sandbox id manager)

        (and (= action "exec") (= method "POST"))
        (handle-exec id req manager)

        (and (= action "activate") (= method "POST"))
        (handle-activate id req manager)

        (and (= action "snapshot") (= method "POST"))
        (handle-snapshot id req manager)

        (and (= action "restore") (= method "POST"))
        (handle-restore id req manager)

        (and (= action "logs") (= method "GET"))
        (handle-logs id manager)

        (error-response "not found" 404)))

    # Default
    (error-response "not found" 404)))

# ── HTTP response writer ─────────────────────────────────────────────

(defn- write-response
  "Write an HTTP response to the stream."
  [stream response]
  (def status (get response :status 200))
  (def body (get response :body ""))
  (def body-str (if (string? body) body (json/encode body)))
  (def status-text (get status-texts status "OK"))
  (def resp-headers (get response :headers @{}))

  (def header-buf @"")
  (buffer/push header-buf (string/format "HTTP/1.1 %d %s\r\n" status status-text))
  (each [k v] (pairs resp-headers)
    (buffer/push header-buf (string/format "%s: %s\r\n" k v)))
  (buffer/push header-buf (string/format "Content-Length: %d\r\n" (length body-str)))
  (buffer/push header-buf "Connection: close\r\n")
  (buffer/push header-buf "\r\n")
  (buffer/push header-buf body-str)

  (net/write stream (string header-buf)))

# ── Public: build handler ────────────────────────────────────────────

(defn build-handler
  "Return handler function for net/server."
  [config manager]
  (fn [stream]
    (defer (:close stream)
      (try
        (do
          (def req (read-request stream))
          (when (nil? req)
            (write-response stream (error-response "bad request" 400))
            (break))

          # Auth check
          (when (not (check-auth req config))
            (write-response stream (error-response "unauthorized" 401))
            (break))

          # Dispatch and handle errors from handlers
          (def response
            (try (dispatch req config manager)
              ([e] (error-response (string e) 500))))

          (write-response stream response))
        ([e]
          (try (write-response stream (error-response "internal error" 500))
            ([e2] nil)))))))
