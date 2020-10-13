;; Copyright © 2020, JUXT LTD.

(ns juxt.spin.alpha.methods
  (:require
   [juxt.spin.alpha.util :as util]
   [clojure.tools.logging :as log]
   [juxt.spin.alpha.server :refer [request-body-as-bytes]]
   [juxt.spin.alpha.resource
    :as resource]
   [juxt.reap.alpha.decoders :refer [content-type]]
   [clojure.string :as str]
   [juxt.spin.alpha.resource :as r])
  (:import
   (java.nio.charset Charset)
   (java.util Date)))

;; This function only exists while we wait for reap to get encoding for
;; media-type/content-type - then it will be removed.
(def ^:private encode-content-type
  (memoize
   (fn [m]
     (assert (:juxt.http/type m))
     (let [params (:juxt.http/parameters m)]
       (format "%s/%s%s"
               (:juxt.http/type m)
               (:juxt.http/subtype m)
               (str/join (for [{:keys [juxt.http/parameter-name juxt.http/parameter-value]} params]
                           (format ";%s=%s" parameter-name parameter-value))))))))

;; This function only exists while we wait for reap to get encoding for
;; media-type/content-type - then it will be removed.
(defn ^:private encode-vary [varying]
  (str/join "," (map :juxt.http/field-name varying)))

(defn respond-with-content-maybe [resource-provider server-provider resource representations response request respond raise]
  (if (satisfies? resource/ContentResponse resource-provider)
    (resource/respond-with-content
     resource-provider server-provider resource representations response request respond raise)
    ;; No content to consider, just respond with the response.
    (respond response)))

(defmulti http-method
  (fn [resource-provider server-provider resource response request respond raise]
    (:request-method request)))

(defmethod http-method :default [resource-provider server-provider resource response request respond raise]
  (respond-with-content-maybe
     resource-provider server-provider resource
     (conj response [:status 501]) request respond raise))

;; TODO: Most :apex ns keywords should be in :juxt.http ns. Refactor!

(defn vary [variants]
  ;; TODO: Work out how variants vary.
  "accept"
  )

;; The GET method requests transfer of a current selected representation for the
;; target resource.
(defn- GET-or-HEAD [resource-provider server-provider resource response request respond raise]
  (let [
        ;; Ensure body is removed if this is a head, although implementations
        ;; should check and optimise
        respond (fn [response]
                  (respond
                   (cond-> response
                     (= (:request-method request) :head)
                     (dissoc :body))))

        respond
        (fn [response]
          (if (satisfies? resource/ContentVariants resource-provider)
            (let [available-variants
                  (resource/available-variants
                   resource-provider server-provider resource response)]
              (if (seq available-variants)

                (let [representations
                      (if (satisfies? resource/ContentProactiveNegotiation resource-provider)
                        (resource/select-representations
                         resource-provider server-provider request available-variants)
                        available-variants)

                      response
                      (cond-> response
                        (satisfies? resource/ContentProactiveNegotiation resource-provider)
                        (update :headers (fnil conj {}) ["vary" (vary available-variants)]))]

                  (if (seq representations)
                    (respond-with-content-maybe
                     resource-provider
                     server-provider
                     resource
                     representations
                     response
                     request respond raise)

                    ;; 406!
                    (let [response (conj response [:status 406])
                          available-variants
                          (resource/available-variants
                           resource-provider server-provider resource response)]
                      (respond-with-content-maybe
                       resource-provider
                       server-provider
                       resource
                       (when (seq available-variants)
                         (resource/select-representations
                          resource-provider server-provider request available-variants))
                       response
                       request respond raise))))

                ;; 404!
                (let [response (conj response [:status 404])
                      available-variants
                      (resource/available-variants
                       resource-provider server-provider resource response)]
                  (respond-with-content-maybe
                   resource-provider
                   server-provider
                   resource
                   (when (seq available-variants)
                     (resource/select-representations
                      resource-provider server-provider request available-variants))
                   response
                   request respond raise))))

            ;; No ContentNegotiation show we leave it up to the implementation
            ;; to decide what content to send.
            (respond-with-content-maybe
             resource-provider server-provider resource nil response request respond raise)))]

    (log/info (str "resource-provider satisfies resource/GET ? " (satisfies? resource/GET resource-provider)))

    (if (satisfies? resource/GET resource-provider)
      (resource/get-or-head
       resource-provider server-provider resource
       response
       request
       respond
       raise)
      ;; Otherwise just respond
      (respond response))))

#_(if (satisfies? resource/ContentNegotiation resource-provider)
    ;; Content negotiation is an optional feature. The path where we have
    ;; content negotiation can be more convoluted than where the
    ;; resource-provider has elected not to provide this facility.
    (let [available-variants (resource/available-variants resource-provider server-provider resource response)]
      (if-not (seq available-variants)
        ;; A 404
        (if-not (= (:status response) 404)
          ;; Unless this was already a 404 (and then it's pointless asking a
          ;; second time), this new status code might result in available
          ;; variants. Let's check!
          (let [available-variants-404
                (let [available-variants-404
                      (resource/available-variants resource-provider server-provider resource (conj response [:status 404]))]
                  (if (seq available-variants-404)
                    available-variants-404
                    ;; The resource-provider is not providing any variants for a
                    ;; 404. But we still have to communicate a 404 to the
                    ;; user-agent. By default, we should provide a human friendly
                    ;; response.
                    [{:juxt.http/content-type "text/html"
                      :juxt.http/language "en"
                      :juxt.http/encoding "identity"}
                     {:juxt.http/content-type "text/plain"
                      :juxt.http/language "en"
                      :juxt.http/encoding "identity"}]
                    ;; TODO: But we should also provide these nice defaults in the case where ContentNegotiation isn't satisfied!
                    ))]))

        (let [selected-variants (resource/select-variants resource-provider server-provider request available-variants)]
          (if (seq selected-variants)
            (if (= 1 (count selected-variants))
              (let [variant (first selected-variants)
                    ;; TODO: Transplant variant into response headers - e.g. Content-Type, Content-Location

                    ;; TODO: But don't penalise code that doesn't care about
                    ;; content-negotiation with too many mandatory
                    ;; callbacks!
                    response response]

                (resource/get-response
                 resource-provider server-provider resource response request respond raise))
              )
            )
          )
        )
      )

    ;; No content negotiation, make this straight forward
    )

#_(defn- GET-or-HEAD [resource-provider server-provider resource response request respond raise]
    (let [representations
          (if (satisfies? ContentNegotiation resource-provider)
            (representation resource-provider resource request)
            resource)]
      (cond
        (and
         (or
          (nil? representations)
          (and (sequential? representations)
               (zero? (count representations))))
         ;; We don't want to return a 406 if the resource-provider doesn't deal in
         ;; representations!
         (satisfies? Representation resource-provider))
        (respond (merge response {:status 406}))

        (and (sequential? representations)
             (>= (count representations) 2)
             (satisfies? MultipleRepresentations resource-provider))
        (send-300-response resource-provider (filter uri? representations) request respond raise)

        :else
        (if-let [representation
                 (cond-> representations
                   (sequential? representations) first)]


          (let [ ;; Validators
                last-modified (:juxt.http/last-modified representation)
                entity-tag (:juxt.http/entity-tag representation)

                status 200

                ;; "In theory, the date ought to represent the moment just before
                ;; the payload is generated."
                orig-date (new Date)

                headers
                (cond-> {"date" (util/format-http-date orig-date)}
                  last-modified
                  (conj ["last-modified" (util/format-http-date last-modified)])

                  entity-tag
                  (conj ["etag" entity-tag])

                  ;; RFC 7231 3.1. Representation meta-data

                  (:juxt.http/content-type representation)
                  (conj ["content-type" (encode-content-type (:juxt.http/content-type representation))])
                  ;; TODO: Content-Encoding
                  ;; TODO: Content-Language
                  (not= (:juxt.http/uri representation) (:juxt.http/uri resource))
                  (conj ["content-location" (str (:juxt.http/uri representation))])

                  (:juxt.http/varying representation)
                  (conj ["vary" (encode-vary (:juxt.http/varying representation))]))

                response
                (-> response
                    (assoc :status status)
                    (update :headers merge headers))]

            ;; TODO: Check condition (Last-Modified, If-None-Match)

            ;; TODO: Handle errors (by responding with error response, with appropriate re-negotiation)

            (if (satisfies? GET resource-provider)
              (get-or-head resource-provider server-provider representation response request respond raise)
              (respond response)))


          ;; "The 404 (Not Found) status code indicates that the origin server did
          ;; not find a current representation for the target resource or is not
          ;; willing to disclose that one exists." -- Section 6.5.4 RFC 7231
          (respond
           {:status 404
            :headers {"content-type" "text/plain;charset=utf-8"}
            :body "Not Found\n"})
          ))))

;; Section 4.3.1
(defmethod http-method :get [resource-provider server-provider resource response request respond raise]
  (GET-or-HEAD resource-provider server-provider resource (into {:status (if resource 200 404)} response) request respond raise))

;; Section 4.3.2
(defmethod http-method :head [resource-provider server-provider resource response request respond raise]
  (GET-or-HEAD resource-provider server-provider resource (into {:status (if resource 200 404)} response) request respond raise))

;; Section 4.3.3
(defmethod http-method :post [resource-provider server-provider resource response request respond raise]
  (if (satisfies? resource/POST resource-provider)
    (let [prior-state? (some? (:juxt.http/payload resource))
          orig-date (new Date)
          response (-> response
                       (assoc :status (if prior-state? 200 201))
                       (update :headers (fnil conj {}) ["date" (util/format-http-date orig-date)]))]
      (resource/post resource-provider server-provider resource response request respond raise))

    ;; What should be the default behavior for a POST method which isn't implemented in the resource provider?
    (throw (ex-info "TODO" {}))))

;; Section 4.3.4
(defmethod http-method :put [resource-provider server-provider resource response request respond raise]
  (if (satisfies? resource/PUT resource-provider)

    ;; juxt.http/payload ? maybe juxt.spin/state
    (let [prior-state? (some? (:juxt.http/payload resource))
          orig-date (new Date)
          response (-> response
                       (assoc :status (if prior-state? 200 201))
                       (update :headers (fnil conj {}) ["date" (util/format-http-date orig-date)]))]
      (resource/put
       resource-provider server-provider resource response request respond raise)

      #_(request-body-as-bytes
         server-provider
         request
         (fn [bytes]
           (let [representation-in-request
                 {:juxt.http/content-type (get-in request [:headers "content-type"])
                  ;; TODO: content-language and content-encoding
                  :juxt.http/payload bytes}

                 ]

             ;; The provider of 'put' can decide to respond directly, or return the
             ;; put resource to us so that we can form the response.
             (when-let [resource
                        (put resource-provider representation-in-request resource response request respond raise)]

               (respond
                (cond-> response


                  (:juxt.http/last-modified resource)
                  (update :headers (fnil conj {})
                          ["last-modified" (util/format-http-date (:juxt.http/last-modified resource))])

                  (:juxt.http/entity-tag resource)
                  (update :headers (fnil conj {})
                          ["etag" (:juxt.http/entity-tag resource)])

                  true (update :headers (fnil conj {}) ["content-length" "0"])
                  true )))))))

    ;; What should be the default behavior for a PUT method which isn't
    ;; implemented in the resource provider?
    (throw (ex-info "TODO" {}))))

;; Section 4.3.5
(defmethod http-method :delete [resource-provider server-provider resource response request respond raise]
  (if (satisfies? resource/DELETE resource-provider)
    (resource/delete resource-provider server-provider resource response request respond raise)
    ;; What should be the default behavior for a DELETE method which isn't
    ;; implemented in the resource provider?
    (throw (ex-info "TODO" {}))))

;; Section 4.3.7
(defmethod http-method :options [resource-provider server-provider resource response request respond raise]
  (if (satisfies? resource/OPTIONS resource-provider)
    (resource/options resource-provider server-provider resource response request respond raise)
    ;; What should be the default behavior for a OPTIONS method which isn't
    ;; implemented in the resource provider?
    (throw (ex-info "TODO" {}))))
