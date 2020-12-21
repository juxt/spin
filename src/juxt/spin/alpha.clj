;; Copyright © 2020, JUXT LTD.

(ns juxt.spin.alpha
  (:require
   [clojure.string :as str]
   [juxt.reap.alpha.regex :as re]
   [juxt.reap.alpha.decoders.rfc7231 :as rfc7231-dec]
   [juxt.reap.alpha.encoders.rfc7231 :as rfc7231-enc]
   [juxt.reap.alpha.decoders.rfc7232 :as rfc7232-dec]
   [juxt.spin.alpha :as spin])
  (:import
   (java.util Date)))

(def ^:private date-decoder (rfc7231-dec/http-date {}))
(def ^:private date-encoder (rfc7231-enc/http-date {}))

(defn ^Date parse-http-date [s]
  (when s
    (:juxt.reap.alpha.rfc7231/date (date-decoder (re/input s)))))

(defn format-http-date [^Date inst]
  (assert (instance? java.util.Date inst) (format "Type is %s" (type inst)))
  (date-encoder {:juxt.reap.alpha.rfc7231/date inst}))

(def ^:private if-none-match-decoded (rfc7232-dec/if-none-match {}))

(defn parse-if-none-match [v]
  (if-none-match-decoded (re/input v)))

(defn not-modified?
  "Return true if the given representation's validators report that it has not
  been modified with respect to the given request. This allows a 304 response to
  be returned."
  [request representation]
  ;; "… a server MUST ignore the conditional request header fields … when
  ;; received with a request method that does not involve the selection or
  ;; modification of a selected representation, such as CONNECT, OPTIONS, or
  ;; TRACE." -- Section 5, RFC 7232
  (when (not (#{:connect :options :trace} (:request-method request)))
    (let [last-modified (get representation "last-modified")

          ;; TODO: See 3.3 of RFC 7232 - only do this on GET and HEAD!

          if-modified-since
          (when last-modified
            (some-> (get-in request [:headers "if-modified-since"])
                    parse-http-date))

          entity-tag (get representation "etag")

          if-none-match
          (when entity-tag
            (some->>
             (get-in request [:headers "if-none-match"])
             parse-if-none-match
             (map (comp :juxt.reap.alpha.rfc7232/opaque-tag :juxt.reap.alpha.rfc7232/entity-tag))
             set))]
      (cond
        (and (seq if-none-match) entity-tag)
        (when (contains? if-none-match entity-tag)
          {:status 304 :body "Not Modified\r\n"})

        (and if-modified-since last-modified)
        (when-not (.isAfter
                   (.toInstant (parse-http-date last-modified))
                   (.toInstant if-modified-since))
          ;; TODO: Need to distinguish which
          {:status 304 :body "Not Modified\r\n"})))))

(defn unknown-method?
  "When the request method is unknown, return a 501 response."
  ([request]
   (unknown-method? request #{:get :head :put :post :delete :options :trace :connect}))
  ([request methods]
   (when-not
       (contains?
        methods
        (:request-method request))
       {:status 501 :body "Not Implemented\r\n"})))

(defn ok []
  {:status 200})

(defn not-found?
  "When representation is nil, return a 404 response."
  [representation]
  (when-not representation
    {:status 404 :body "Not Found\r\n"}))

(defn allow-header
  "Return the Allow response header value, given a set of method keywords."
  [methods]
  (->>
   (if (and
        methods
        (seq (disj methods :get :head :options)))
     (concat methods [:options])
     [:get :options])
   ;; if GET is included, so is HEAD
   (mapcat (fn [method] (if (= :get method) [:get :head] [method])))
   distinct
   (map (comp str/upper-case name))
   (str/join ", ")))

(defn method-not-allowed?
  [request methods]
  (let [method (:request-method request)]
    (when-not (contains? methods method)
      {:status 405
       :headers {"allow" (allow-header methods)}
       :body "Method Not Allowed\r\n"})))

(defn head? [request]
  (= (:request-method request) :head))

(defn bad-request []
  {:status 400
   :body "Bad Request\r\n"})

(defn created
  "Convenience function for returning a 201 repsonse with a Location header."
  [location]
  {:status 201
   :headers {"location" location}
   :body "Created\r\n"})

(defn authenticate-challenge
  "Convenience function for returning a 201 repsonse with a Location header."
  [challenge]
  {:status 401
   :headers {"www-authenticate" challenge}
   :body "Unauthorized\r\n"})

(defn options
  [methods]
  {:status 200
   :headers
   {"allow" (allow-header methods)
    ;; TODO: Shouldn't this be a situation (a missing body) detected by
    ;; middleware, which can set the content-length header accordingly?
    "content-length" "0"}})

(defn wrap-add-date
  "Compute and add a Date header to the response."
  [h]
  (fn
    [request]
    (let [inst (java.util.Date.)
          response (h request)
          status (get response :status 200)]
      (cond-> response
        ;; While Section 7.1.1.2 of RFC 7232 states: "An origin server
        ;; MAY send a Date header field if the response is in the 1xx
        ;; (Informational) or 5xx (Server Error) class of status
        ;; codes.", we choose not to, as it cannot be used for
        ;; cacheing.
        (and (>= status 200) (< status 500))
        (assoc-in
         [:headers "date"]
         (format-http-date inst))))))
