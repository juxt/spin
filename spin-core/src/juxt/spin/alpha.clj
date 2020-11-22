;; Copyright © 2020, JUXT LTD.

(ns juxt.spin.alpha
  (:require
   [clojure.spec.alpha :as s]
   [clojure.spec.gen.alpha :as gen]))

(s/def :ring.request/method keyword?)
(s/def :ring.request/path string?)
(s/def :ring.request/headers map?)

(s/def ::request
  (s/keys :req [:ring.request/method
                :ring.request/path]
          :opt [:ring.request/headers]))

(s/def ::ctx (s/keys :opt [::request ::raise]))

(s/def ::resource
  (s/keys :opt []))
