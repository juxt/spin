;; Copyright © 2020, JUXT LTD.

(ns juxt.spin.alpha.util
  (:require
   [juxt.reap.alpha.regex :as re]
   [juxt.reap.alpha.decoders.rfc7231 :as rfc7231-dec]
   [juxt.reap.alpha.encoders.rfc7231 :as rfc7231-enc]
   [juxt.reap.alpha.decoders.rfc7232 :as rfc7232-dec]
   )
  (:import
   (java.util Date)))

;; Reap utilities (encode-date, decode-date)

(def ^:private date-decoder (rfc7231-dec/http-date {}))
(def ^:private date-encoder (rfc7231-enc/http-date {}))

(defn ^Date parse-http-date [s]
  (:juxt.reap.alpha.rfc7231/date (date-decoder (re/input s))))

(defn format-http-date [^Date inst]
  (assert (instance? java.util.Date inst) (format "Type is %s" (type inst)))
  (date-encoder {:juxt.reap.alpha.rfc7231/date inst}))


(def ^:private if-none-match-decoded (rfc7232-dec/if-none-match {}))

(defn parse-if-none-match [v]
  (if-none-match-decoded (re/input v)))
