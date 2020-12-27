;; Copyright © 2020, JUXT LTD.

(ns juxt.spin.alpha
  (:require
   [clojure.string :as str]
   [juxt.spin.alpha :as spin]))

(defn not-implemented?
  "When the request method is not implemented, return a 501 response."
  ([request]
   (not-implemented? request #{:get :head :put :post :delete :options :trace :connect}))
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
   methods
   seq
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

#_(defn wrap-add-date
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
