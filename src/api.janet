# API — HTTP handlers for /cgi-bin/...
# Uses net/server (built-in) for HTTP. JSON via json package.

(import config :prefix "config/")
(import validate :prefix "validate/")
(import manager :prefix "manager/")
(import json)

(defn json-response [body status]
  @{
    :status (or status 200)
    :headers @{"Content-Type" "application/json"}
    :body (if (string? body) body (json/encode body))
  })

(defn error-response [msg status]
  (json-response @{:error msg} (or status 400)))

(defn build-handler [config manager]
  "Return handler function for net/server."
  (fn [stream]
    (def req (string (net/read stream 65536)))
    (when (nil? req) (return nil))
    # Parse request line: "GET /path HTTP/1.1"
    (def lines (string/split "\r\n" req))
    (def req-line (get lines 0))
    (def parts (string/split " " req-line))
    (def method (get parts 0))
    (def path (get parts 1))

    # Auth check for /cgi-bin/api/
    (when (string/has-prefix? path "/cgi-bin/api/")
      (def auth-token (get config :auth-token))
      (when auth-token
        # TODO: parse Authorization header
        nil))

    # Route
    (def response
      (cond
        (= path "/cgi-bin/health")
        (json-response @{:status "ok" :version "4"} 200)

        (and (= method "GET") (= path "/cgi-bin/api/modules"))
        (do
          (def mods (array))
          # TODO: list modules from config
          (json-response @{:modules mods} 200))

        (and (= method "GET") (= path "/cgi-bin/api/sandboxes"))
        (do
          (def sandboxes (manager/manager-list manager))
          (def list (array (map (fn [s] @{
            :id (get s :id)
            :owner (get s :owner)
            :task (get s :task)
            :state (get s :state)
            :created (get s :created)
            :last-active (get s :last-active)
          }) sandboxes)))
          (json-response list 200))

        (and (= method "POST") (= path "/cgi-bin/api/sandboxes"))
        (try
          (do
            (def body (last (string/split "\r\n\r\n" req)))
            (def data (json/decode body))
            (def id (get data "id"))
            (when (not (validate/valid-id? id))
              (error-response "invalid id" 400))
            (def opts @{
              :owner (get data "owner" "anon")
              :task (get data "task" "")
              :layers (get data "layers" "000-base-alpine")
              :cpu (get data "cpu" 2)
              :memory-mb (get data "memory_mb" 1024)
              :max-lifetime-s (get data "max_lifetime_s" 0)
            })
            (def sandbox (manager/manager-create manager id opts))
            (json-response @{
              :id (get sandbox :id)
              :owner (get sandbox :owner)
              :task (get sandbox :task)
              :state (get sandbox :state)
              :created (get sandbox :created)
            } 201))
          ([e] (error-response (string e) 400)))

        :else
        (error-response "not found" 404)))

    # Send response
    (def status (get response :status))
    (def body (get response :body))
    (net/write stream (string/format "HTTP/1.1 %d OK\r\nContent-Type: application/json\r\nContent-Length: %d\r\n\r\n%s"
                                    status (length body) body))))
