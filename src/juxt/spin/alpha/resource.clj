;; Copyright © 2020, JUXT LTD.

(ns juxt.spin.alpha.resource
  (:require
   [juxt.spin.alpha.util :as util]))

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
    fields (e.g. :juxt.http/last-modified-since, :juxt.http/entity-tag) should
    be also returned here OR by the available-variants method below."))

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
    errors). Variants SHOULD contain validator
    fields (:juxt.http/last-modified, :juxt.http/entity-tag)."))

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
    content (content-length, content-range). Then (unless
    the :request-method is :head) also add the payload itself in the :body of
    the response. If there are multiple representations, either send an initial
    representation with a menu of links, or just a menu of links to the
    representations. Finally call the respond callback with the
    response. Warning: Be careful only to modify the given response argument,
    which may already contain a status and some headers."))

(defprotocol ErrorResponse
  (respond-with-error [_ server resource representation response request respond raise]
    "Use the respond function to respond with an error. The status code is in
    the :status entry of the response argument."))


;; Forwarding proxy
(defrecord ResourceProviderProxy [app-impl default-impl]
  ResourceLocator
  (locate-resource [_ uri request]
    (if (satisfies? ResourceLocator app-impl)
      (locate-resource app-impl uri request)
      (locate-resource default-impl uri request)))

  AllowedMethods
  (allowed-methods [_ server resource request]
    (if (satisfies? AllowedMethods app-impl)
      (allowed-methods app-impl server resource request)
      (allowed-methods default-impl server resource request)))

  GET
  (get-or-head [_ server resource response request respond raise]
    (if (satisfies? GET app-impl)
      (get-or-head app-impl server resource response request respond raise)
      (get-or-head default-impl server resource response request respond raise)))

  POST
  (post [_ server resource response request respond raise]
    (if (satisfies? POST app-impl)
      (post app-impl server resource response request respond raise)
      (post default-impl server resource response request respond raise)))

  PUT
  (put [_ server resource response request respond raise]
    (if (satisfies? PUT app-impl)
      (put app-impl server resource response request respond raise)
      (put default-impl server resource response request respond raise)))

  DELETE
  (delete [_ server resource response request respond raise]
    (if (satisfies? DELETE app-impl)
      (delete app-impl server resource response request respond raise)
      (delete default-impl server resource response request respond raise)))

  OPTIONS
  (options [_ server resource response request respond raise]
    (if (satisfies? OPTIONS app-impl)
      (options app-impl server resource response request respond raise)
      (options default-impl server resource response request respond raise)))

  ContentVariants
  (available-variants [_ server resource response]
    (if (satisfies? ContentVariants app-impl)
      (available-variants app-impl server resource response)
      (available-variants default-impl server resource response)))

  ContentProactiveNegotiation
  (select-representations [_ server request variants]
    (if (satisfies? ContentProactiveNegotiation app-impl)
      (select-representations app-impl server request variants)
      (select-representations default-impl server request variants)))

  ContentResponse
  (respond-with-content [_ server resource representations response request respond raise]
    (if (satisfies? ContentResponse app-impl)
      (respond-with-content app-impl server resource representations response request respond raise)
      (respond-with-content default-impl server resource representations response request respond raise)))

  ErrorResponse
  (respond-with-error [_ server resource representation response request respond raise]
    (if (satisfies? ErrorResponse app-impl)
      (respond-with-error app-impl server resource representation response request respond raise)
      (respond-with-error default-impl server resource representation response request respond raise))))


;; Defaults
(defrecord BackingResourceProvider []
  ResourceLocator
  ;; If ResourceLocator is not satisfied, let's assume the resource exists
  ;; and set it to an empty map. That's the most sensible default. It is
  ;; unlikely, outside of testing and simple demos, that a
  ;; resource-provider will not satisfy http/ResourceLocator
  (locate-resource [_ uri request] {})

  AllowedMethods
  (allowed-methods [_ server resource request]
    (or
     (:juxt.http/allowed-methods resource)
     #{:get :head}))

  GET
  (get-or-head [_ server resource response request respond raise]
    (respond response))

  POST
  (post [_ server resource response request respond raise]
    (throw (ex-info "Default not implemented!" {})))

  PUT
  (put [_ server resource response request respond raise]
    (throw (ex-info "Default not implemented!" {})))

  DELETE
  (delete [_ server resource response request respond raise]
    (throw (ex-info "Default not implemented!" {})))

  OPTIONS
  (options [_ server resource response request respond raise]
    (throw (ex-info "Default not implemented!" {})))

  ContentVariants
  (available-variants [_ server resource response]
    (if (< (:status response) 400)
      ;; We will use the resource as the (only) representation.
      [resource]
      ;; Types we're happy to produce on error (4xx/5xx)
      [{:juxt.http/content-type "text/plain;charset=utf8"}
       {:juxt.http/content-type "text/html;charset=utf8"}]))

  ContentProactiveNegotiation
  (select-representations [_ server request variants]
    variants)

  ContentResponse
  (respond-with-content [_ server resource representations response request respond raise]
    ;; No content to consider, just respond with the response.

    (cond
      (and
       (< (:status response) 400)
       (> (count representations) 1))
      ;; We could produce an automatic 300 here
      (throw (ex-info "TODO: Create a text/plain or text/html menu of links to these representations?" {}))

      (zero? (count representations))
      (throw (ex-info "TODO: 0 representations" {:resource resource :response response}))

      (or
       (= (count representations) 1)
       ;; On error, we just take the first representation
       (>= (:status response) 400))
      (let [representation (first representations)]
        (respond
         (cond-> response
           (:juxt.http/content-type representation)
           (assoc-in
            [:headers "content-type"]
            (:juxt.http/content-type representation))

           (:juxt.http/content-language representation)
           (assoc-in
            [:headers "content-language"]
            (:juxt.http/content-language representation))

           (:juxt.http/content-encoding representation)
           (assoc-in
            [:headers "content-encoding"]
            (:juxt.http/content-encoding representation))

           (:juxt.http/content-length representation)
           (assoc-in
            [:headers "content-length"]
            (str (:juxt.http/content-length representation)))

           (:juxt.http/last-modified representation)
           (assoc-in [:headers "last-modified"] (util/format-http-date (:juxt.http/last-modified representation)))
           (:juxt.http/entity-tag representation)
           (assoc-in [:headers "etag"] (:juxt.http/entity-tag representation)))))

      :else (raise (ex-info "Unexpected case" {:response response
                                               :representations representations}))))


  ErrorResponse
  (respond-with-error [_ server resource representation response request respond raise]
    ;; TODO: Make this a little more informative!
    (respond (assoc response :body "spin: ERROR"))))
