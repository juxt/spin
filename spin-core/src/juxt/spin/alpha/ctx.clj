;; Copyright © 2020, JUXT LTD.

(ns juxt.spin.alpha.ctx
  (:require
   [clojure.spec.alpha :as s]
   [juxt.spin.alpha :as spin]))

(defn sync-adapt [h]
  (fn this
    ([req]
     (let [p (promise)]
       (this
        req
        (fn [response]
          (deliver p response))
        (fn [error]
          (deliver p error)))
       (let [res (deref p 1000 ::timeout)]
         (cond
           (= res ::timeout)
           (throw
            (ex-info
             "Timeout occured waiting for handler"
             {:handler h
              :request req}))
           (instance? Throwable res)
           (throw res)
           :else
           res))))
    ([req respond raise]
     (h req respond raise))))

(defmulti http-method
  (fn [{:juxt.spin.alpha/keys [request]}]
    (:request-method request))
  :default ::default)

(defmethod http-method ::default [{:juxt.spin.alpha/keys [respond]}]
  (respond {:status 501}))

(defn check-allowed-method [{:juxt.spin.alpha/keys [respond]}]
  (respond {:status 200 :body "TODO"}))

(defn not-found [{:juxt.spin.alpha/keys [respond]}]
  (respond {:status 404}))

(defn locate-resource
  [{:juxt.spin.alpha/keys [locate-resource resource raise] :as ctx}]
  (let [resource (or
                  resource
                  (when locate-resource
                    (try
                      (locate-resource ctx)
                      (catch Exception e
                        (raise (ex-info "Failed to locate-resource" {:ctx ctx} e))))))
        ctx (conj ctx [:juxt.spin.alpha/resource resource])]
    (if resource
      (check-allowed-method ctx)
      (not-found ctx))))

(s/fdef locate-resource
  :args (s/cat :ctx (s/keys :req []
                            :opt [::spin/locate-resource
                                  ::spin/resource]))
  :ret ::spin/resource
  )

(defn handler [ctx]
  (-> (fn [request respond raise]
        #_(s/explain ::spin/ctx {::spin/request request
                               ::spin/respond respond
                               ::spin/raise raise})
        (locate-resource
         (conj ctx {::spin/request request
                    ::spin/respond respond
                    ::spin/raise raise})))
      sync-adapt))
