;; Copyright © 2020, JUXT LTD.

(ns demo
  (:require
   [clojure.java.io :as io]
   [clojure.string :as str]
   [clojure.set :as set]
   [hiccup.page :as hp]
   [juxt.pick.alpha.ring :refer [pick]]
   [juxt.spin.alpha :as spin]
   [ring.adapter.jetty :as jetty]
   [ring.core.protocols :refer [StreamableResponseBody]])
  (:import
   (java.util Date)
   (java.time Instant)))

;; Resources - these could be registered in a database.

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

;; Representations - these are streams of bytes representing the current, or
;; intended, state of a resource.

(defrecord ByteArrayRepresentation [bytes]
  StreamableResponseBody
  (write-body-to-stream [body response output-stream]
    (.write output-stream bytes)))

(defn make-byte-array-representation [bytes content-location content-type]
  (with-meta
    (->ByteArrayRepresentation bytes)
    {"content-location" content-location
     "content-type" content-type
     "content-length" (str (count bytes))
     "etag"
     (format
      "\"%s\""      ; etags MUST be wrapped in DQUOTEs
      (hash         ; Clojure's hash function will do, but we could use another
       {:content bytes
        :content-type content-type}))}))

(defn conj-meta [o metadata]
  (with-meta o
    (conj (meta o) metadata)))

(defn make-comment [location s]
  (->
   (make-byte-array-representation
    (.getBytes s)
    location
    "text/plain;charset=utf-8")
   (conj-meta {"content-language" "en-US"
               "last-modified" (-> "2020-12-14T23:00:00Z"
                                   Instant/parse
                                   Date/from
                                   spin/format-http-date)})))

;; Some comments on a web page
(def *database
  (atom
   {:next-comment-id 2
    :comments [(make-comment "/comments/1" "Here is the first comment")]}))

(defn index-page-representation [title content-location]
  (make-byte-array-representation
   (.getBytes
    (str
     (hp/html5
      [:head
       [:title title]]
      [:body
       [:h1 title]
       [:a {:href "/comments"} "Comments"]])
     "\r\n\r\n"))
   content-location
   "text/html;charset=utf-8"))

(def representations
  [(->
    (make-byte-array-representation
     (.getBytes
      (hp/html5 [:head [:meta {"http-equiv" "Refresh" "content" "0; URL=/index.html"}]]))
     "/"
     "text/html;charset=utf-8")
    (conj-meta
     {"last-modified"
      (-> "2020-12-01T09:00:00Z"
          java.time.Instant/parse
          java.util.Date/from
          spin/format-http-date)}))

   (->
    (index-page-representation
     "Welcome to the spin demo!"
     "/en/index.html")
    (conj-meta
     {"content-language" "en-US"
      "last-modified"
      (-> "2020-12-25T09:00:00Z"
          java.time.Instant/parse
          java.util.Date/from
          spin/format-http-date)}))

   (->
    (index-page-representation
     "Willkommen zur Spin-Demo!"
     "/de/index.html")
    (conj-meta
     {"content-language" "de"
      "last-modified"
      (-> "2020-12-25T09:00:00Z"
          java.time.Instant/parse
          java.util.Date/from
          spin/format-http-date)}))

   (->
    (index-page-representation
     "¡Bienvenida a la demo de spin!"
     "/es/index.html")
    (conj-meta
     {"content-language" "es"
      "last-modified"
      (-> "2020-12-25T09:00:00Z"
          java.time.Instant/parse
          java.util.Date/from
          spin/format-http-date)}))

   #_(with-meta
     (reify
       StreamableResponseBody
       (write-body-to-stream [body response output-stream]
         (.write
          output-stream
          (.getBytes
           (hp/html5
            [:head
             [:title "Comments"]]
            [:body
             [:h1 "Comments"]
             [:ol
              (for [{:keys [comment]} (:comments @*database)]
                [:li comment])]])))))
     {"content-location" "/comments.html"
      "content-type" "text/html;charset=utf-8"})

   #_(with-meta
     (reify
       StreamableResponseBody
       (write-body-to-stream [body response output-stream]
         (.write
          output-stream
          (.getBytes
           (apply str (map (comp #(str % "\r\n") :comment) (:comments @*database)))))))
     {"content-location" "/comments.txt"
      "content-type" "text/plain;charset=utf-8"})])

(def negotiated-representations
  {"/index.html" ["/en/index.html" "/de/index.html" "/es/index.html"]
   "/comments" ["/comments.html" "/comments.txt"]})

(def representations-by-path
  (let [index (into {} (map (juxt #(get (meta %) "content-location") identity) representations))
        connegs (into {} (for [[k v] negotiated-representations]
                           [k (map index v)]))]
    (merge index connegs)))

;; ------

(defn current-representations
  "Return the current representations for a resource."
  [resource]
  (representations-by-path (::spin/path resource)))

(defn locate-resource [path]
  (get resources-by-location path))

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
                  (let [current (when resource (current-representations resource))]
                    (when (empty? current)
                      (throw
                       (ex-info
                        "Not Found"
                        {::spin/response
                         {:status 404
                          :body "Not Found\r\n"}})))

                    (let [to-pick
                          ;; There's some work to do on our representation
                          ;; format to adapt it to pick's expectations.
                          (fn [representation]
                            (let [md (meta representation)]
                              (->
                               (set/rename-keys
                                md
                                {"content-type" :juxt.pick.alpha/content-type
                                 "content-encoding" :juxt.pick.alpha/content-encoding
                                 "content-language" :juxt.pick.alpha/content-language})
                               (assoc ::representation representation))))

                          negotiation-result
                          (pick
                           request
                           (map to-pick current)
                           {:juxt.pick.alpha/vary? true})]

                      (if-let [representation (::representation (:juxt.pick.alpha/representation negotiation-result))]
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
                                   (meta representation)
                                   ;; representation metadata
                                   ["content-type" "content-encoding" "content-language"
                                    ;; validators
                                    "last-modified" "etag"
                                    ;; payload header fields too
                                    "content-length" "content-range"]))

                           ;; content-location is only set if different from the effective uri
                           (not= (get (meta representation) "content-location") (:uri request))
                           (assoc "content-location" (get (meta representation) "content-location")))}

                  (= (:request-method request) :get)
                  (assoc :body representation))

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
