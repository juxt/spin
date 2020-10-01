;; Copyright © 2020, JUXT LTD.

(ns juxt.spin.alpha.core
  (:require
   [juxt.spin.alpha.util :as util]
   [juxt.spin.alpha.server :refer [request-body-as-bytes]]
   [juxt.spin.alpha.resource
    :refer [locate-resource
            Representation representation
            MultipleRepresentations send-300-response
            GET get-or-head
            PUT put
            POST post
            DELETE delete
            OPTIONS options]]
   [juxt.reap.alpha.decoders :refer [content-type]]
   [clojure.string :as str])
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

(defmulti http-method (fn [resource-provider server-provider resource response request respond raise] (:request-method request)))

(defmethod http-method :default [resource-provider server-provider resource response request respond raise]
  (respond {:status 501}))

;; TODO: Most :apex ns keywords should be in :juxt.http ns. Refactor!

(defn- GET-or-HEAD [resource-provider server-provider resource response request respond raise]
  (let [representations
        (if (satisfies? Representation resource-provider)
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
  (GET-or-HEAD resource-provider server-provider resource response request respond raise))

;; Section 4.3.2
(defmethod http-method :head [resource-provider server-provider resource response request respond raise]
  (GET-or-HEAD resource-provider server-provider resource response request respond raise))

;; Section 4.3.3
(defmethod http-method :post [resource-provider server-provider resource response request respond raise]
  (if (satisfies? POST resource-provider)
    (let [prior-state? (some? (:juxt.http/payload resource))
          orig-date (new Date)
          response (-> response
                       (assoc :status (if prior-state? 200 201))
                       (update :headers (fnil conj {}) ["date" (util/format-http-date orig-date)]))]
      (post resource-provider server-provider resource response request respond raise))
    (respond {:status 405})))

;; Section 4.3.4
(defmethod http-method :put [resource-provider server-provider resource response request respond raise]
  (if (satisfies? PUT resource-provider)

    ;; juxt.http/payload ? maybe juxt.spin/state
    (let [prior-state? (some? (:juxt.http/payload resource))
          orig-date (new Date)
          response (-> response
                       (assoc :status (if prior-state? 200 201))
                       (update :headers (fnil conj {}) ["date" (util/format-http-date orig-date)]))]
      (put resource-provider server-provider resource response request respond raise)

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
    (respond {:status 405})))

;; Section 4.3.5
(defmethod http-method :delete [resource-provider server-provider resource response request respond raise]
  (if (satisfies? DELETE resource-provider)
    (delete resource-provider server-provider resource response request respond raise)
    (respond {:status 405})))

;; Section 4.3.7
(defmethod http-method :options [resource-provider server-provider resource response request respond raise]
  (if (satisfies? OPTIONS resource-provider)
    (options resource-provider server-provider resource response request respond raise)
    (respond {:status 405})))
