;;;; config.lisp — settings and credentials.
;;;;
;;;; Port of core/config (settings store) and core/auth/credentials.zig.
;;;; Credential sources, in order: VERCEL_OIDC_TOKEN, AI_GATEWAY_API_KEY,
;;;; then the stored key at ~/.fx/api-key.json (the Zig version uses the
;;;; OS secret store; this port keeps a plain file with 0600 permissions).

(in-package #:fx.config)

(defparameter *gateway-base-url*
  (or (getenv "FX_GATEWAY_BASE_URL") "https://ai-gateway.vercel.sh")
  "Same default as gateway/client.zig's default_gateway_base_url.")

(defparameter *default-model* "anthropic/claude-sonnet-4.5")

(defparameter missing-credential-message
  "Fx needs access to Vercel AI Gateway. Run fx setup to use an API key, or set AI_GATEWAY_API_KEY.")

(defun settings-path ()
  (merge-pathnames "settings.json" (fx-dir)))

(defun load-settings ()
  (let ((path (settings-path)))
    (if (probe-file path)
        (handler-case (fx.json:decode (read-file-string path))
          (error () (fx.json:make-jobject)))
        (fx.json:make-jobject))))

(defun save-settings (settings)
  (write-file-string (settings-path) (fx.json:encode-to-string settings)))

(defun resolve-model (&optional cli-model)
  (or cli-model
      (getenv "FX_MODEL")
      (fx.json:jget (load-settings) "model")
      *default-model*))

(defun stored-key-path ()
  (merge-pathnames "api-key.json" (fx-dir)))

(defun load-stored-key ()
  (let ((path (stored-key-path)))
    (when (probe-file path)
      (handler-case
          (fx.json:jget (fx.json:decode (read-file-string path)) "api_key")
        (error () nil)))))

(defun resolve-api-key ()
  "Return (values key source) or NIL when no credential is available."
  (let ((oidc (getenv "VERCEL_OIDC_TOKEN"))
        (env-key (getenv "AI_GATEWAY_API_KEY"))
        (stored (load-stored-key)))
    (cond
      (oidc (values oidc :vercel-oidc-token))
      (env-key (values env-key :ai-gateway-api-key))
      (stored (values stored :stored-key))
      (t nil))))
