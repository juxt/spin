;; Copyright © 2020-2021, JUXT LTD.

(ns juxt.spin.alpha.negotiation
  (:require
   [clojure.string :as str]
   [juxt.pick.alpha.ring :refer [pick]]
   [juxt.spin.alpha.representation :refer [representation-metadata]]
   [juxt.spin.alpha :as spin]))

(defn negotiate-representation [request current-representations date opts]
  ;; Negotiate the best representation, determining the vary
  ;; header.
  (let [{selected-representation-metadata :juxt.pick.alpha/representation
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

        selected-representation
        (::attached-representation selected-representation-metadata)

        ;; Check for a 406 Not Acceptable
        _ (when (contains? #{:get :head} (:request-method request))
            (spin/check-not-acceptable! selected-representation))

        selected-representation-metadata
        (as-> selected-representation-metadata %
          (dissoc % ::attached-representation)
          ;; Remove the extraneous keyword entries added by pick.
          (filter (comp string? first) %)
          (into {} %))

        ;; Pin the vary header onto the selected representation's
        ;; metadata
        selected-representation-metadata
        (cond-> selected-representation-metadata
          (and selected-representation-metadata (seq vary))
          (assoc "vary" (str/join ", " vary)))]

    {:selected-representation selected-representation
     :selected-representation-metadata selected-representation-metadata}

    ))
