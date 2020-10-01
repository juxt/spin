;; Copyright © 2020, JUXT LTD.

(ns juxt.spin.alpha.resource)

;; TODO: OpenAPI in Apex support should be written in terms of these
;; interfaces.

;; TODO: Define exactly the map that locate-resource must return - define this
;; in a spec.
;; juxt.http/payload bytes, optional (if nil, indicates no representation found for this resource)

(defprotocol ResourceLocator
  :extend-via-metadata true
  (locate-resource
    [_ uri request]
    "Return the resource identified with the given URI. Return nil if no
    resource found. The resource should be returned as a map containing
    a :juxt.http/uri entry of type String. Validator
    fields (e.g. :juxt.http/last-modified-since) should be also returned since
    any pre-conditions will be checked against this returned value."))

(defprotocol Representation
  :extend-via-metadata true
  (representation
    [_ resource request]
    "For a given resource, return the representation. If a collection containing
    multiple values are returned, a 300 will result. The pattern of negotiation
    is up to the provider (proactive, reactive, transparent, etc.)."))

(defprotocol GET
  (get-or-head [_ server resource response request respond raise]
    "Invoke the GET or HEAD method on the resource. You should add headers to
    the given response to describe the payload (content-length,
    content-range). If the :request-method of the given request is :get then
    also add the payload itself in the :body of the response. Finally call the
    respond callback with the response. Warning: Be careful only to modify the
    given response argument, which may already contain a status and some
    headers."))

(defprotocol MultipleRepresentations
  :extend-via-metadata true
  (send-300-response
    [_ representations request respond raise]
    "Satisfy this protocol if you want to support reactive
    negotation."))

(defprotocol POST
  (post [_ server resource response request respond raise]
    "Invoke the POST method on the resource. The POST method requests that the
    target resource process the representation enclosed in the request according
    to the resource's own specific semantics. Implementations are expected to
    signal a response with the respond callback."))

(defprotocol PUT
  (put [_ server resource response request respond raise]
    "Invoke the PUT method on the resource. The PUT method requests that the
    state of the target resource be created or replaced with the state defined
    by the representation enclosed in the request message payload. If the
    content type of the request message payload is not reconcilable with the resource, a 415
    response should be sent. Implementations are expected EITHER to signal a
    response with the respond callback (and MUST set the status and other
    headers explicitly according to the semantics defined in RFC 7231 Section
    4.3.4)."))

(defprotocol DELETE
  (delete [_ server resource response request respond raise]
    "Invoke the DELETE method on the resource. Implementations are expected to
    signal a response with the respond callback."))

(defprotocol OPTIONS
  :extend-via-metadata true
  (options [_ server resource response request respond raise]
    "Invoke the OPTIONS method on the resource. Implementations are expected to
    signal a response with the respond callback."))
