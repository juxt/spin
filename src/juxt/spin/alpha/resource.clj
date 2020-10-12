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

(defprotocol AllowedMethods
  :extend-via-metadata true
  (allowed-methods [_ server resource request]
    "Return a set of keywords indicating the allowed methods on this
    resource. If not satisfied, defaults to #{:get}"))

(defprotocol GET
  :extend-via-metadata true
  (get-or-head [_ server resource response request respond raise]
    "Invoke the GET or HEAD method on the resource."))

(defprotocol POST
  :extend-via-metadata true
  (post [_ server resource response request respond raise]
    "Invoke the POST method on the resource. The POST method requests that the
    target resource process the representation enclosed in the request according
    to the resource's own specific semantics. Implementations are expected to
    signal a response with the respond callback."))

(defprotocol PUT
  :extend-via-metadata true
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
  :extend-via-metadata true
  (delete [_ server resource response request respond raise]
    "Invoke the DELETE method on the resource. Implementations are expected to
    signal a response with the respond callback."))

(defprotocol OPTIONS
  :extend-via-metadata true
  (options [_ server resource response request respond raise]
    "Invoke the OPTIONS method on the resource. Implementations are expected to
    signal a response with the respond callback."))

;; All resources:

;; if never content-negotiation, don't satisfy protocol ContentNegotiation

;; if always reactive content-negotiation, implement ContentVariants but don't implement
;; ContentNegotation, (default is to just pass back the variants argument)

;; if always proactive content-negotiation, then implement ContentVariants AND
;; ContentNegotiation

;; Per-resource strategy:

;; if no content-negotiation, return [resource] from available-variants, return
;; [variant] in select-variants

;; if proactive content-negotiation, return variants collection from available-variants
;; (nil/empty means 404), then pick one in select-variants - returning nil/empty
;; means 406

;; if reactive content-negotiation, return variants collection, return
;; collection (containing multiple items) from select-variants, which will
;; result in 300


(defprotocol ContentVariants
  :extend-via-metadata true
  (available-variants [_ server resource response]
    "Return a collection of available variants for a response. Returning empty
    or nil will result in a 404 response. The response may indicate which
    variants are available (for example, variants may differ for 4xx/5xx
    errors."))

(defprotocol ContentProactiveNegotiation
  :extend-via-metadata true
  (select-representations [_ server request variants]
    "Return the representation (or representations) from the variants to return
  in the response. Return a collection. Returning empty or nil will result in a
  406 response. Returning multiple representations may result in a 300
  response."))

(defprotocol ContentResponse
  :extend-via-metadata true
  (respond-with-content [_ server resource representations response request respond raise]
    "The representations may be empty, in which case decide what type of content, if any,
    to send. You should add headers to the given response to describe the
    content (content-length, content-range) and representation
    validators (last-modified-date, etag) as applicable. Then (unless
    the :request-method is :head) also add the payload itself in the :body of
    the response. If there are multiple representations, they can be used as
    links in the content, either as a menu (300) or as part of an initial
    response (200). Finally call the respond callback with the
    response. Warning: Be careful only to modify the given response argument,
    which may already contain a status and some headers."))
