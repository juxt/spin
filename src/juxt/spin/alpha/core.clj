;; Copyright © 2020, JUXT LTD.

(ns juxt.spin.alpha.core
  (:require
   [juxt.spin.alpha.util :as util]
   [juxt.spin.alpha.resource
    :refer [locate-resource
            ContentNegotiation best-representation
            MultipleRepresentations send-300-response
            LastModified last-modified
            EntityTag entity-tag
            GET get-or-head
            PUT put
            POST post
            DELETE delete
            OPTIONS options]]
   [clojure.string :as str]))

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

(defn lookup-resource
  "Return the map corresponding to the resource identified by the given URI. Add
  the URI to the map as the :juxt.http/uri entry. Return nil if not found."
  [provider ^java.net.URI uri]
  (when-let [resource (locate-resource provider uri)]
    (conj resource [:juxt.http/uri uri])))

(defmulti http-method (fn [resource-provider server-provider resource response request respond raise] (:request-method request)))

(defmethod http-method :default [resource-provider server-provider resource response request respond raise]
  (respond {:status 501}))

;; TODO: Most :apex ns keywords should be in :juxt.http ns. Refactor!

;;(defn uri? [i] (instance? java.net.URI i))

(defn- GET-or-HEAD [resource-provider server-provider resource response request respond raise]
  (if resource
    (let [{:juxt.http/keys [variants varying]}
          (if (and (satisfies? ContentNegotiation resource-provider) (:juxt.http/variants resource))
            (best-representation
             resource-provider
             resource request)
            {:juxt.http/variants [resource]})
          representations variants]

      (cond
        (or
         (nil? representations)
         (and (sequential? representations)
              (zero? (count representations))))
        (respond (merge response {:status 406}))

        (and (sequential? representations)
             (>= (count representations) 2)
             (satisfies? MultipleRepresentations resource-provider))
        (send-300-response resource-provider (filter uri? representations) request respond raise)

        :else
        (let [representation
              (cond-> representations
                (sequential? representations) first)

              last-modified
              (when (satisfies? LastModified resource-provider)
                (last-modified resource-provider representation))

              entity-tag
              (when (satisfies? EntityTag resource-provider)
                (entity-tag resource-provider representation))

              status 200

              ;; "In theory, the date ought to represent the moment just before
              ;; the payload is generated."
              orig-date
              (new java.util.Date)

              headers
              (cond-> {"date" (util/format-http-date orig-date)}
                last-modified
                (conj ["last-modified" (util/format-http-date last-modified)])

                entity-tag
                (conj ["etag" entity-tag])

                ;; RFC 7231 3.1. Representation meta-data

                representation
                (conj ["content-type" (encode-content-type (:juxt.http/content-type representation))])
                ;; TODO: Content-Encoding
                ;; TODO: Content-Language
                (not= (:juxt.http/uri representation) (:juxt.http/uri resource))
                (conj ["content-location" (str (:juxt.http/uri representation))])

                ;; RFC 7231 3.3. Payload Semantics

                (:juxt.http/content-length representation)
                (conj ["content-length" (str (:juxt.http/content-length representation))])

                varying
                (conj ["vary" (encode-vary varying)]))

              response
              (-> response
                  (assoc :status status)
                  (update :headers merge headers))]

          ;; TODO: Check condition (Last-Modified, If-None-Match)

          ;; TODO: Handle errors (by responding with error response, with appropriate re-negotiation)

          (cond
            (satisfies? GET resource-provider)
            (get-or-head resource-provider server-provider representation response request respond raise)
            :else (respond response)))))

    ;; resource is nil
    (respond
     (-> response
         (assoc :status 404 :body "Not Found\n")
         (update :headers (fnil conj {}) ["content-type" "text/plain;charset=utf-8"])))))

;; Section 4.3.1
(defmethod http-method :get [resource-provider server-provider resource response request respond raise]
  (GET-or-HEAD resource-provider server-provider resource response request respond raise))

;; Section 4.3.2
(defmethod http-method :head [resource-provider server-provider resource response request respond raise]
  (GET-or-HEAD resource-provider server-provider resource response request respond raise))

;; Section 4.3.3
(defmethod http-method :post [resource-provider server-provider resource response request respond raise]
  (post resource-provider server-provider resource response request respond raise))

;; Section 4.3.4
(defmethod http-method :put [resource-provider server-provider resource response request respond raise]
  (if (satisfies? PUT resource-provider)
    (put resource-provider server-provider resource response request respond raise)
    (raise (ex-info "Resource provider does not implement PUT" {}))))

;; Section 4.3.5
(defmethod http-method :delete [resource-provider server-provider resource response request respond raise]
  (delete resource-provider server-provider resource response request respond raise))

;; Section 4.3.7
(defmethod http-method :options [resource-provider server-provider resource response request respond raise]
  (cond
    (satisfies? OPTIONS resource-provider)
    (options resource-provider server-provider resource response request respond raise)
    ;; TODO: Investigate
    :else (respond {:status 200})))
