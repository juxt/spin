;; Copyright © 2020, JUXT LTD.

(ns juxt.spinner.helpers
  (:require
   [juxt.spinner.core :as s]))

(defn GET! [request representation respond! _]
  (assert (contains? #{:get :head} (:request-method request)))

  ;; Now to evaluate conditional requests. Note that according to Section 5,
  ;; RFC 7232, "redirects and failures take precedence over the evaluation
  ;; of preconditions in conditional requests." Therefore, we don't evaluate
  ;; whether the request is conditional until we have determined its status
  ;; code.
  (cond
    (s/not-modified? request representation)
    (respond! {:status 304})

    :else
    representation))
