;; Copyright © 2020, JUXT LTD.

(ns juxt.spin.alpha.test-util
  (:require
   [juxt.spin.alpha.ctx :as ctx]
   [clojure.string :as str]))

(defn response-for
  ([ctx request]
   ((ctx/handler ctx) request))
  ([ctx request keyseq]
   (let [keyseq (cond-> keyseq (seq (filter string? keyseq)) (conj :ring.response/headers))]
     (cond-> (response-for ctx request)
       true
       (select-keys (filter keyword? keyseq))
       (seq (filter string? keyseq))
       (update :ring.response/headers select-keys (filter string? keyseq))))))

(defn header [request header value]
  (-> request
      (assoc-in [:ring.response/headers header] value)))

(defn request [method reluri]
  (let [[_ path query] (re-matches #"([^\?]*)\??(.*)" reluri)]
    (cond-> {:ring.request/method method}
     (seq path) (conj [:ring.request/path path])
     (seq query) (conj [:ring.request/query query]))))
