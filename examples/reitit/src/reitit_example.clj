;; Copyright © 2020, JUXT LTD.

(ns reitit-example
  (:require
   [reitit.core :as r]
   [juxt.spin.alpha :as spin]))

(def router
  (r/router
   [["/api/ping" :ping]
    ["/api/orders/:id" :order]]))

(comment
  (r/match-by-path router "/api/ping"))

(defn locate-resource [request]
  (let [target (r/match-by-path router (:url request))
        nm (get-in target [:data :name])]

    (cond-> {::spin/reitit-result target}
      nm (conj
          (case nm

            :ping {::spin/methods #{:get}
                   :type nm}

            :order {::spin/methods #{:get :post :put :delete}
                    :type nm
                    :order-id (get-in target [:path-params :id])}))

      (nil? nm) ((fnil conj {})
                 {::spin/methods #{:get}
                  ::spin/representations []}))))

(comment
  (locate-resource {:url "/api/orders/H10" }))
