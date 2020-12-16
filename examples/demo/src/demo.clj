;; Copyright © 2020, JUXT LTD.

(ns demo
  (:require
   [clojure.java.io :as io]
   [clojure.string :as str]
   [hiccup.page :as hp]
   [juxt.pick.alpha.ring :refer [pick]]
   [juxt.spin.alpha :as spin]
   [ring.adapter.jetty :as jetty]
   [ring.core.protocols :refer [StreamableResponseBody]])
  (:import
   (java.util Date)
   (java.time Instant)))

(def resources
  [{::spin/path "/" ::spin/methods #{:get}}
   {::spin/path "/index.html" ::spin/methods #{:get}}
   {::spin/path "/en/index.html" ::spin/methods #{:get}}
   {::spin/path "/de/index.html" ::spin/methods #{:get}}
   {::spin/path "/es/index.html" ::spin/methods #{:get}}
   {::spin/path "/comments.html" ::spin/methods #{:get}}
   {::spin/path "/comments.txt" ::spin/methods #{:get}}
   {::spin/path "/comments" ::spin/methods #{:post :get}}])

(def resources-by-location
  (into {} (map (juxt ::spin/path identity) resources)))

(defrecord ByteArrayRepresentation [bytes]
  StreamableResponseBody
  (write-body-to-stream [body response output-stream]
    (.write output-stream bytes)))

;; Some comments on a web page
(def *database
  (atom
   {:next-comment-id 2
    :comments
    [^{"content-location" "/comments/1"
       "last-modified" (-> "2020-12-14T23:00:00Z"
                           Instant/parse
                           Date/from
                           spin/format-http-date)}
     {:comment "Here is the first comment"}]}))

(def static-representations
  (letfn [(index-page [title]
            (str
             (hp/html5
              [:head
               [:title title]]
              [:body
               [:h1 title]
               [:a {:href "/comments"} "Comments"]])
             "\r\n\r\n"))]
    {"/" (hp/html5 [:head [:meta {"http-equiv" "Refresh" "content" "0; URL=/index.html"}]])
     "/en/index.html" (index-page "Welcome to the spin demo!")
     "/de/index.html" (index-page "Willkommen zur Spin-Demo!")
     "/es/index.html" (index-page "¡Bienvenida a la demo de spin!")}))

(defn index-html-representation-metadata [content-location content-language]
  (let [content-type "text/html;charset=utf-8"]
    {"content-type" content-type
     "content-language" content-language
     "content-location" content-location
     "content-length" (str (count (get static-representations content-location)))

     "etag"
     (format
      "\"%s\"" ; etags MUST be wrapped in DQUOTEs
      (hash ; Clojure's hash function will do, but we could use another
       {:content (get static-representations content-location)
        :content-type content-type
        :content-language content-language
        :content-encoding ""}))

     "last-modified"
     (-> "2020-12-25T09:00:00Z"
         java.time.Instant/parse
         java.util.Date/from
         spin/format-http-date)}))

(def representation-metadata
  (let [index-en (index-html-representation-metadata "/en/index.html" "en-US")
        index-de (index-html-representation-metadata "/de/index.html" "de")
        index-es (index-html-representation-metadata "/es/index.html" "es")

        comments-html
        {"content-type" "text/html;charset=utf-8"
         "content-location" "/comments.html"}

        comments-txt
        {"content-type" "text/plain;charset=utf-8"
         "content-location" "/comments.txt"}]

    {"/" [{"content-type" "text/html;charset=utf-8"
           "content-location" "/"
           "content-length" (str (count (get static-representations "/")))

           "etag"
           (format
            "\"%s\"" ; etags MUST be wrapped in DQUOTEs
            (hash    ; Clojure's hash function will do, but we could use another
             {:content (get static-representations "/")
              :content-type "text/html;charset=utf-8"}))

           "last-modified"
           (-> "2020-12-01T09:00:00Z"
               java.time.Instant/parse
               java.util.Date/from
               spin/format-http-date)}]

     "/index.html" [index-en index-de index-es]
     "/en/index.html" [index-en]
     "/de/index.html" [index-de]
     "/es/index.html" [index-es]
     "/comments" [comments-html comments-txt]
     "/comments.html" [comments-html]
     "/comments.txt" [comments-txt]}))

(defn locate-resource [path]
  (get resources-by-location path))

(defn available-representations [resource]
  (get representation-metadata (::spin/path resource) []))

(defn response-body [representation]
  (let [content-location (get representation "content-location")]
    (or
     (get static-representations content-location)
     (case content-location
       "/comments.html"
       (str
        (hp/html5
         [:head
          [:title "Comments"]]
         [:body
          [:h1 "Comments"]
          [:ol
           (for [{:keys [comment]} (:comments @*database)]
             [:li comment])]])
        "\r\n")
       "/comments.txt"
       (map (comp #(str % "\r\n") :comment) (:comments @*database))))))

(defn post! [request resource]
  (case (::spin/path resource)
    "/comments"
    (do
      (when-not (get-in request [:headers "content-length"])
        (throw
         (ex-info
          "No Content-Length header found"
          {::spin/response
           {:status 411
            :body "Length Required\r\n"}})))

      (when-not (:body request)
        (throw
         (ex-info
          "No body in request"
          {::spin/response
           {:status 400
            :body "Bad Request\r\n"}})))

      ;; TODO: Check input type, can throw a 415 error response if necessary
      ;; (But use reap)
      ;; (if (= (get-in request [:headers "content-type"]) "text/plain"))

      (let [out (java.io.ByteArrayOutputStream.)]
        (with-open [in (:body request)]
          (io/copy in out))
        (let [comment (String. (.toByteArray out))]
          (swap!
           *database
           (fn [{:keys [next-comment-id comments] :as db}]
             (-> db
                 (update
                  :comments
                  conj
                  (with-meta {:comment comment}
                    {"content-location" (format "/comments/%d" next-comment-id)
                     "last-modified" (spin/format-http-date (new Date))}))
                 (update :next-comment-id inc)))))

        {:status 200 :body "Thanks!\r\n"}))))

(defn handler [request]
  (try
    ;; Check method is known
    (if-let [response (spin/unknown-method? request)]
      response

      ;; Locate the resource
      (let [resource (locate-resource (:uri request))]


        ;; Check method allowed
        (if-let [response (when resource (spin/method-not-allowed? request (::spin/methods resource)))]
          response

          ;; Select the representation (only if GET or HEAD)
          (let [{:keys [representation vary]}
                (when (#{:get :head} (:request-method request))
                  (let [available (when resource (available-representations resource))]
                    (when (empty? available)
                      (throw
                       (ex-info
                        "Not Found"
                        {::spin/response
                         {:status 404
                          :body "Not Found\r\n"}})))

                    (let [to-pick
                          ;; There's some work to do on our representation
                          ;; format to adapt it to pick's expectations.
                          (fn [{:strs [content-type content-encoding content-language] :as representation}]
                            (cond-> representation
                              content-type (assoc :juxt.pick.alpha/content-type content-type)
                              content-encoding (assoc :juxt.pick.alpha/content-encoding content-encoding)
                              content-language (assoc :juxt.pick.alpha/content-language content-language)))

                          negotiation-result
                          (pick
                           request
                           (map to-pick (available-representations resource))
                           {:juxt.pick.alpha/vary? true})]

                      (if-let [representation (:juxt.pick.alpha/representation negotiation-result)]
                        {:representation representation
                         :vary (:juxt.pick.alpha/vary negotiation-result)}
                        (throw
                         (ex-info
                          "Not Acceptable"
                          { ;; TODO: Must add list of available representations
                           ::spin/response
                           {:status 406
                            :body "Not Acceptable\r\n"}}))))))

                response
                (cond-> {}
                  (seq vary)
                  (assoc "vary" (str/join ", " vary)))]

            ;; Conditional requests
            (if-let [not-modified-response
                     (spin/not-modified? request representation)]
              not-modified-response

              ;; Process the request method
              (case (:request-method request)
                (:get :head)
                ;; GET (or HEAD)
                (cond-> {:status 200
                         :headers
                         (cond-> (conj
                                  response
                                  (select-keys
                                   representation
                                   ;; representation metadata
                                   ["content-type" "content-encoding" "content-language"
                                    ;; validators
                                    "last-modified" "etag"
                                    ;; payload header fields too
                                    "content-length" "content-range"]))

                           ;; content-location is only set if different from the effective uri
                           (not= (get representation "content-location") (:uri request))
                           (assoc "content-location" (get representation "content-location")))}

                  (= (:request-method request) :get)
                  (assoc :body (response-body representation)))

                :post
                (post! request resource)))))))

    (catch clojure.lang.ExceptionInfo e
      (let [exdata (ex-data e)]
        (or
         (::spin/response exdata)
         {:status 500 :body "Internal Error\r\n"})))))

(defn run [opts]
  (prn opts)
  (jetty/run-jetty
   handler
   {:port (Integer/parseInt (get opts "--port" "8080")) :join? true}))
