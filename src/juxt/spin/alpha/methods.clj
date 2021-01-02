;; Copyright © 2020, JUXT LTD.

(ns juxt.spin.alpha.methods
  (:require
   [clojure.string :as str]
   [juxt.spin.alpha :as spin]
   [juxt.spin.alpha.representation
    :refer [payload]]
   [juxt.reap.alpha.encoders :refer [format-http-date]]))

(defn GET [request resource
           date selected-representation selected-representation-metadata
           current-representations vary
           opts]

  (when (empty? current-representations)
    (throw
     (ex-info
      "Not Found"
      {::spin/response
       {:status 404
        :body "Not Found\r\n"}})))

  (when-not selected-representation
    (throw
     (ex-info
      "Not Acceptable"
      { ;; TODO: Must add list of available representations
       ::spin/response
       {:status 406
        :body "Not Acceptable\r\n"}})))

  (spin/evaluate-preconditions! request resource selected-representation-metadata)

  ;; "The Range header field is evaluated after evaluating the precondition
  ;; header fields defined in [RFC7232], and only if the result in absence
  ;; of the Range header field would be a 200 (OK) response.  In other
  ;; words, Range is ignored when a conditional GET would result in a 304
  ;; (Not Modified) response.

  (let [ranges-specifier (spin/request-range request resource selected-representation-metadata)

        {status :status
         payload-headers :headers
         body :body}
        (payload selected-representation
                 selected-representation-metadata
                 date
                 ranges-specifier
                 opts)]

    (cond-> {:status (or status 200)
             :headers
             (cond-> {}
               date (assoc "date" (format-http-date date))

               (::spin/accept-ranges resource)
               (assoc "accept-ranges" (str/join ", " (::spin/accept-ranges resource)))

               (seq vary) (assoc "vary" (str/join ", " vary))

               selected-representation-metadata (merge selected-representation-metadata)

               (= (get selected-representation-metadata "content-location") (:uri request))
               (dissoc "content-location")

               payload-headers (merge payload-headers))}

      ;; Don't add body for a HEAD method
      (= (:request-method request) :get) (assoc :body body))))
