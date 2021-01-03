;; Copyright © 2020, JUXT LTD.

(ns demo.handler
  (:require
   [demo.app :as app]
   [clojure.java.io :as io]
   [juxt.spin.alpha.representation
    :refer [representation-metadata]]
   [juxt.spin.alpha.methods
    :refer [GET extract-representation-from-request]]
   [juxt.pick.alpha.ring :refer [pick]]
   [juxt.reap.alpha.encoders :refer [format-http-date]]
   [juxt.spin.alpha :as spin]))

(defn POST [request resource date {::spin/keys [db-atom]}]

  (assert (= (:uri request) (::spin/path resource)))

  ;; Revisit - we should not encode knowledge of the URI structure here.
  (case (:uri request)
    ;; TODO: Should look at some hint in the resource as to functionality,
    ;; perhaps via a defmethod
    "/comments"
    (do
      (when-not (get-in request [:headers "content-length"])
        (throw
         (ex-info
          "No Content-Length header found"
          {::spin/response
           {:status 411
            :body "Length Required\r\n"}})))

      (when-not (:body request)
        (throw
         (ex-info
          "No body in request"
          {::spin/response
           {:status 400
            :body "Bad Request\r\n"}})))

      ;; TODO: Check input type, can throw a 415 error response if necessary
      ;; (But use reap)
      ;; (if (= (get-in request [:headers "content-type"]) "text/plain"))

      (let [out (java.io.ByteArrayOutputStream.)
            _ (with-open [in (:body request)]
                (io/copy in out))
            new-db (swap!
                    db-atom
                    (fn [db]
                      ;; TODO: Any preconditions?
                      (app/add-comment db (String. (.toByteArray out)))))]
        (cond->
            (spin/created (:last-location new-db))
          date (assoc "date" (format-http-date date))
          ;; TODO: Add optional payload, with payload headers
          )))))

(defn PUT [request resource selected-representation-metadata date {:demo.app/keys [db-atom]}]
  (let [new-representation (extract-representation-from-request
                            request resource selected-representation-metadata date)
        new-resource
        (-> resource
            (assoc ::spin/representations [new-representation])
            (dissoc ::spin/path))]

    (swap!
     db-atom
     (fn [db]
       (spin/evaluate-preconditions! request resource selected-representation-metadata)
       (assoc-in
        db [:resources (:uri request)] new-resource)))

    ;; TODO: Must read 6.3.2 and 7.2 to properly understand 201, especially: "The
    ;; 201 response payload typically describes and links to the resource(s)
    ;; created."

    ;; "If the target resource does not have a current representation and the PUT
    ;; successfully creates one, then the origin server MUST inform the user agent
    ;; by sending a 201 (Created) response."

    (cond-> {:status 200}
      date (assoc "date" (format-http-date date)))))

(defn DELETE [request resource selected-representation-metadata date {:demo.app/keys [db-atom]}]
  (swap! db-atom #(do
                    (spin/evaluate-preconditions! request resource selected-representation-metadata)
                    (update % :resources dissoc (::spin/path resource))))
  (cond-> {:status 200}
    date (assoc "date" (format-http-date date))
    true (assoc :body "Deleted\r\n")))

(defn OPTIONS [_ resource _ _]
  ;; TODO: Implement *
  (spin/options (::spin/methods resource)))

(defn make-handler [*database]
  (fn [request]
    (let [db @*database]
      (try
        ;; Check method implemented
        (spin/check-method-not-implemented! request)

        ;; Locate the resource
        (let [resource (app/locate-resource db (:uri request))]

          ;; Check method allowed
          (spin/check-method-not-allowed! request resource)

          (let [ ;; Fix the date, this will be used as the message origination
                 ;; date.
                date (new java.util.Date)

                ;; Get the 'current' representations of the resource.
                current-representations (app/current-representations db resource)

                opts {:demo.app/db-atom *database}

                ;; Negotiate the best representation, determining the vary
                ;; header.
                {representation-metadata :juxt.pick.alpha/representation
                 vary :juxt.pick.alpha/vary}
                (when (seq current-representations)
                  ;; Call into pick which does that actual content-negotation
                  ;; for us.
                  (pick
                   request
                   (for [r current-representations]
                     (-> r
                         (representation-metadata date opts)
                         (assoc ::attached-representation r)))
                   {:juxt.pick.alpha/vary? true}))

                selected-representation (::attached-representation representation-metadata)

                representation-metadata
                (as-> representation-metadata %
                  (dissoc % ::attached-representation)
                  ;; Remove the extraneous keyword entries added by pick.
                  (filter (comp string? first) %)
                  (into {} %))]

            ;; Process the request method
            (case (:request-method request)
              (:get :head)
              (GET request resource
                   date selected-representation representation-metadata
                   current-representations vary
                   {:demo.app/db db})

              :post
              (POST request resource date opts)

              :put
              (PUT request resource representation-metadata date opts)

              :delete
              (DELETE request resource representation-metadata date opts)

              :options
              (OPTIONS request resource date opts))))

        (catch clojure.lang.ExceptionInfo e
;;          (tap> e)
          (let [exdata (ex-data e)]
            (or
             (::spin/response exdata)
             {:status 500 :body "Internal Error\r\n"})))))))
