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
          (deliver p response)
          ;; It's crucial this returns nil, as these are semantics of the async
          ;; version
          nil)
        (fn [error]
          (deliver p error)
          nil))
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
    ([req respond! raise!]
     (h req respond! raise!))))

(defmulti http-method
  (fn [{::spin/keys [request]}] (:ring.request/method request))
  :default ::default)

(defmethod http-method :get [{::spin/keys [respond! resource get-or-head!] :as ctx}]
  (let [status (if (empty? resource) 404 200)]
    (if get-or-head!
      (get-or-head! (into {::spin/status status} ctx))
      (respond! {:status status}))))

(defmethod http-method ::default [{::spin/keys [respond!]}]
  (respond! {:status 501}))

(defn locate-resource
  [{::spin/keys [locate-resource resource raise] :as ctx}]
  (when-let
      ;; If the resource is nil, this indicates the locate-resource callback has
      ;; responded itself.
      [resource
       (cond
         resource resource
         locate-resource (try
                           (locate-resource ctx)
                           (catch Exception e
                             (raise (ex-info "Failed to locate-resource" {:ctx ctx} e))))
         :else {})]

      (println "resource is" (pr-str resource))
      (let [ctx (conj ctx [::spin/resource resource])]
        (http-method ctx))))

(s/fdef locate-resource
  :args (s/cat :ctx (s/keys :req []
                            :opt [::spin/locate-resource
                                  ::spin/resource]))
  ;; A nil means the locate-resource chooses to respond itself
  :ret (s/nilable ::spin/resource))

(defn handler [ctx]
  (-> (fn [request respond! raise!]
        (locate-resource
         (conj ctx {::spin/request request
                    ::spin/respond! respond!
                    ::spin/raise! raise!})))
      sync-adapt))
