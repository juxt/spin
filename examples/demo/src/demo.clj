;; Copyright © 2020-2021, JUXT LTD.

(ns demo
  (:require
   [clojure.java.io :as io]
   [clojure.string :as str]
   [hiccup.page :as hp]
   [juxt.pick.alpha.ring :refer [pick]]
   [juxt.reap.alpha.encoders :refer [format-http-date]]
   [juxt.spin.alpha.ranges :as ranges]
   [juxt.spin.alpha.representation :refer [representation-metadata
                                           make-char-sequence-representation
                                           IRepresentation
                                           payload]]
   [juxt.spin.alpha :as spin]
   [ring.adapter.jetty :as jetty]
   [ring.core.protocols :refer [StreamableResponseBody]]))

(defn make-comment [comment]
  (make-char-sequence-representation
   comment
   {"content-type" "text/plain;charset=utf-8"
    "content-language" "en-US"
    "last-modified" (format-http-date (new java.util.Date))}))

(defn index-page-representation [title representation-metadata]
  (make-char-sequence-representation
   (str
    (hp/html5
     [:head
      [:title title]]
     [:body
      [:h1 title]
      [:a {:href "/comments.html"} "Comments"]])
    "\r\n\r\n")
   (merge
    {"content-type" "text/html;charset=utf-8"}
    representation-metadata)))

(defn get-comments [db]
  (->>
   (for [[path res] (:resources db)
         :let [n (second (re-matches #"/comments/(\d+)" path))]
         :when n]
     {:location path
      :representation (first (::spin/representations res))
      :ordinal n})
   (sort-by :ordinal)))

(defn add-comment [{:keys [next-comment-id] :as db} comment]
  (let [location (format "/comments/%d" next-comment-id)]
    (-> db
        ;; Create a resource
        (update
         :resources
         assoc
         location
         {::spin/methods #{:get :put :delete}
          ::spin/representations [(make-comment comment)]})
        (update :next-comment-id inc)
        (assoc :last-location location))))

(defn locate-resource
  "Locate a resource. We have lots of options here. We can attempt to find a
  'static' resource from a database or file-system, or create a resource
  dynamically if the path warrants it. This is very much an application
  developer's decision and cannot be performed by a library."
  [db path]
  (or
   (some->
    (get-in db [:resources path])
    (assoc ::spin/path path))
   (when (re-matches #"/articles/[a-z][a-z0-9-]*.adoc" path)
     ;; Return a resource that represents the missing article. This is the
     ;; resource that will be added to the database
     {::spin/path path
      ::spin/methods #{:get :head :put :options}
      ::spin/representations []
      ::spin/max-content-length 1024
      ::spin/acceptable
      {"accept" "text/asciidoc,text/plain"
       "accept-charset" "utf-8"}})))

(defn current-representations [db resource]
  (mapcat
   (fn [rep]
     (if (string? rep)
       (current-representations db (get (:resources db) rep))
       [rep]))
   (::spin/representations resource)))

(def *database
  (atom
   { ;; Resources - this contain methods, authorization details, and current representations.
    :resources
    {"/"
     {::spin/methods #{:get :head :options}
      ::spin/representations
      [(make-char-sequence-representation
        (str
         (hp/html5 [:head [:meta {"http-equiv" "Refresh" "content" "0; URL=/index.html"}]])
         "\r\n\r\n")
        {"content-type" "text/html;charset=utf-8"
         "last-modified" (-> "2020-12-01T09:00:00Z"
                             java.time.Instant/parse
                             java.util.Date/from
                             format-http-date)})]}

     "/index.html"
     {::spin/methods #{:get :head :options}
      ::spin/representations
      ["/en/index.html"
       "/de/index.html"
       "/es/index.html"]}

     "/en/index.html"
     {::spin/methods #{:get :head :options}
      ::spin/representations
      [(index-page-representation
        "Welcome to the spin demo!"
        {"content-language" "en-US"
         "content-location" "/en/index.html"
         "last-modified" (-> "2020-12-25T09:00:00Z"
                             java.time.Instant/parse
                             java.util.Date/from
                             format-http-date)})]}

     "/de/index.html"
     {::spin/methods #{:get :head :options}
      ::spin/representations
      [(index-page-representation
        "Willkommen zur Spin-Demo!"
        {"content-language" "de"
         "content-location" "/de/index.html"
         "last-modified"
         (-> "2020-12-25T09:00:00Z"
             java.time.Instant/parse
             java.util.Date/from
             format-http-date)})]}

     "/es/index.html"
     {::spin/methods #{:get :head :options}
      ::spin/representations
      [(index-page-representation
        "¡Bienvenida a la demo de spin!"
        {"content-language" "es"
         "content-location" "/es/index.html"
         "last-modified"
         (-> "2020-12-25T09:00:00Z"
             java.time.Instant/parse
             java.util.Date/from
             format-http-date)})]}

     "/comments.html"
     {::spin/methods #{:get :head :options}
      ::spin/representations
      [(reify
         IRepresentation
         (representation-metadata [_ date _]
           {"content-type" "text/html;charset=utf-8"
            "content-location" "/comments.html"})
         (payload [_ _ date ranges-specifier {::keys [db]}]
           (let [bytes (.getBytes
                        (str
                         (hp/html5
                          [:head
                           [:title "Comments"]]
                          [:body
                           [:h1 "Comments"]
                           [:ol
                            (for [{:keys [location representation]} (get-comments db)]
                              [:li
                               (:char-sequence representation)
                               "&nbsp;"
                               [:small
                                [:a {:href location}
                                 "view"]]])]])
                         "\r\n\r\n"))]
             {:headers {"content-length" (str (count bytes))}
              :body (reify StreamableResponseBody
                      (write-body-to-stream [_ response output-stream]
                        (.write output-stream bytes)))})))]}

     "/comments.txt"
     {::spin/methods #{:get :head :options}
      ::spin/representations
      [(reify
         IRepresentation
         (representation-metadata [_ date opts]
           {"content-type" "text/plain;charset=utf-8"
            "content-location" "/comments.txt"})
         (payload [_ _ date ranges-specifier {::keys [db]}]
           (assert db)
           (let [bytes (.getBytes
                        (str/join
                         (for [{:keys [representation]} (get-comments db)]
                           (str (:char-sequence representation) "\r\n"))))]
             {:headers {"content-length" (str (count bytes))}
              :body (reify StreamableResponseBody
                      (write-body-to-stream [_ response output-stream]
                        (.write output-stream bytes)))})))]
      ::spin/accept-ranges ["bytes" "comments"]}

     "/comments"
     {::spin/methods #{:get :head :post :options}
      ::spin/representations
      ["/comments.html" "/comments.txt"]}

     "/bytes.txt"
     {::spin/methods #{:get :head :options}
      ::spin/representations
      [(let [limit (* 8 100)]
         (reify
           IRepresentation
           (representation-metadata [_ date opts]
             {"content-type" "text/plain;charset=US-ASCII"
              "etag" "\"abc\""
              "last-modified" (format-http-date #inst "2020-12-31T16:00:00Z")})
           (payload [_
                     representation-metadata
                     date
                     {:juxt.reap.alpha.rfc7233/keys [units byte-range-set]
                      :as ranges-specifier}
                     {::keys [db]}]
             (let [bytes (.getBytes
                          (str/join (map #(format "%08d" %) (clojure.core/range 0 limit 8)))
                          "US-ASCII")]
               (if ranges-specifier
                 (case units
                   "bytes" (ranges/byte-ranges-payload
                            bytes ranges-specifier representation-metadata)
                   "lines" (throw
                            (ex-info
                             "Unsupported range units (TODO)"
                             {:demo.handler/response
                              {:status 500 :body (format "TODO: Support range units of %s\r\n" units)}})))

                 {:headers {"content-length" (str (count bytes))}
                  :body bytes})))))]

      ;; to "indicate that it supports range requests for the target resource."
      ;; -- Section 2.3, RFC 7233
      ::spin/accept-ranges ["bytes" "lines"]}}

    :next-comment-id 1}))

(swap!
 *database
 (fn [db]
   (-> db
       (add-comment "Here is the first comment")
       (add-comment "Here is another comment")
       (add-comment "HTTP is a great protocol, so well thought out")
       (add-comment "REST is an architectural style, it's rare to find it implemented properly")
       (add-comment "Another comment")
       (add-comment "We need to be able to delete these comments!")
       (add-comment "And add to them via a POST")
       (add-comment "How about a form?"))))

(defn GET
  "The GET method."
  [request resource
   date selected-representation selected-representation-metadata
   current-representations
   opts]

  ;; Check for a 404 Not Found
  (spin/check-not-found! current-representations)

  ;; Check for a 406 Not Acceptable
  (spin/check-not-acceptable! selected-representation)

  ;; Check for a 304 Not Modified
  (spin/evaluate-preconditions! request resource selected-representation-metadata)

  ;; "The Range header field is evaluated after evaluating the precondition
  ;; header fields defined in [RFC7232], and only if the result in absence
  ;; of the Range header field would be a 200 (OK) response.  In other
  ;; words, Range is ignored when a conditional GET would result in a 304
  ;; (Not Modified) response.

  (let [ranges-specifier (spin/request-range request resource selected-representation-metadata)

        ;; Here we determine the status (optional), payload headers and body of
        ;; the representation.
        {status :status
         payload-headers :headers
         body :body}
        (payload selected-representation
                 selected-representation-metadata
                 date
                 ranges-specifier
                 opts)]

    (cond-> {:status (or status 200)
             :headers
             (cond-> {}
               date (assoc "date" (format-http-date date))

               (::spin/accept-ranges resource)
               (assoc "accept-ranges" (str/join ", " (::spin/accept-ranges resource)))

               selected-representation-metadata (merge selected-representation-metadata)

               (= (get selected-representation-metadata "content-location") (:uri request))
               (dissoc "content-location")

               payload-headers (merge payload-headers))}

      ;; Don't add body for a HEAD method
      (= (:request-method request) :get) (assoc :body body))))

(defn POST [request resource date {::spin/keys [db-atom]}]

  (assert (= (:uri request) (::spin/path resource)))

  ;; Revisit - we should not encode knowledge of the URI structure here.
  (case (:uri request)
    ;; TODO: Should look at some hint in the resource as to functionality,
    ;; perhaps via a defmethod
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

      (let [out (java.io.ByteArrayOutputStream.)
            _ (with-open [in (:body request)]
                (io/copy in out))
            new-db (swap!
                    db-atom
                    (fn [db]
                      ;; TODO: Any preconditions?
                      (add-comment db (String. (.toByteArray out)))))]
        (cond->
            (spin/created (:last-location new-db))
            date (assoc "date" (format-http-date date))
            ;; TODO: Add optional payload, with payload headers
            )))))

(defn PUT [request resource selected-representation-metadata date {::keys [db-atom]}]
  (let [new-representation (spin/receive-representation request resource date)
        new-resource
        (-> resource
            (assoc ::spin/representations [new-representation])
            (dissoc ::spin/path))]

    (swap!
     db-atom
     (fn [db]
       (spin/evaluate-preconditions! request resource selected-representation-metadata)
       (assoc-in
        db [:resources (:uri request)] new-resource)))

    ;; TODO: Must read 6.3.2 and 7.2 to properly understand 201, especially: "The
    ;; 201 response payload typically describes and links to the resource(s)
    ;; created."

    ;; "If the target resource does not have a current representation and the PUT
    ;; successfully creates one, then the origin server MUST inform the user agent
    ;; by sending a 201 (Created) response."

    (cond-> {:status 200}
      date (assoc "date" (format-http-date date)))))

(defn DELETE [request resource selected-representation-metadata date {::keys [db-atom]}]
  (swap! db-atom #(do
                    (spin/evaluate-preconditions! request resource selected-representation-metadata)
                    (update % :resources dissoc (::spin/path resource))))
  (cond-> {:status 200}
    date (assoc "date" (format-http-date date))
    true (assoc :body "Deleted\r\n")))

(defn OPTIONS [_ resource _ _]
  ;; TODO: Implement *
  (spin/options (::spin/methods resource)))

(defn make-handler [*database]
  (fn [request]
    (let [db @*database]
      (try
        ;; Check method implemented
        (spin/check-method-not-implemented! request)

        ;; Locate the resource
        (let [resource (locate-resource db (:uri request))]

          ;; Check method allowed
          (spin/check-method-not-allowed! request resource)

          (let [ ;; Fix the date, this will be used as the message origination
                ;; date.
                date (new java.util.Date)

                ;; Get the 'current' representations of the resource.
                current-representations (current-representations db resource)

                opts {::db-atom *database}

                ;; Negotiate the best representation, determining the vary
                ;; header.
                {representation-metadata :juxt.pick.alpha/representation
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

                selected-representation (::attached-representation representation-metadata)

                representation-metadata
                (as-> representation-metadata %
                  (dissoc % ::attached-representation)
                  ;; Remove the extraneous keyword entries added by pick.
                  (filter (comp string? first) %)
                  (into {} %))

                ;; Pin the vary header
                representation-metadata
                (cond-> representation-metadata
                  (and representation-metadata (seq vary))
                  (assoc "vary" (str/join ", " vary)))]

            ;; Process the request method
            (case (:request-method request)
              (:get :head)
              (GET request resource
                   date selected-representation representation-metadata
                   current-representations
                   {::db db})

              :post
              (POST request resource date opts)

              :put
              (PUT request resource representation-metadata date opts)

              :delete
              (DELETE request resource representation-metadata date opts)

              :options
              (OPTIONS request resource date opts))))

        (catch clojure.lang.ExceptionInfo e
          ;;          (tap> e)
          (let [exdata (ex-data e)]
            (or
             (::spin/response exdata)
             {:status 500 :body "Internal Error\r\n"})))))))


(defn handler [req]
  ((make-handler *database) req))

(defn run [opts]
  (jetty/run-jetty
   (make-handler *database)
   {:port (Integer/parseInt (get opts "--port" "8080")) :join? true}))
