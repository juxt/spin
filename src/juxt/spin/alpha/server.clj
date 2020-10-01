;; Copyright © 2020, JUXT LTD.

(ns juxt.spin.alpha.server)

(defprotocol ServerOptions
  :extend-via-metadata true
  (server-header [_]
    "Return the value for server header, or nil to avoid setting it.")
  (server-options [_]))

;; TODO: Deprecate and remove in favor of ReactiveStreamable
(defprotocol RequestBody
  (request-body-as-bytes [_ request cb]
    "Calls the cb function with the bytes of the request body as a
    byte-stream. A Ring-compatible server should call the cb function with
    the :body entry of the given request.")

  (request-body-as-multipart-bytes [_ resource response request respond raise]
    "Request a multipart"))

(defprotocol ReactiveStreamable
  (subscribe-to-request-body [_ request subscriber]
    "Subscribe to a request's body, with back-pressured non-blocked IO. The
    subscriber must satisfy the juxt.flow.protocols/Subscriber protocol.")
  (receive-multipart-body [_ response request respond raise]
    "Return a publisher that publishes file-upload events."))
