;; Copyright © 2020, JUXT LTD.

(ns demo.app
  (:require
   [demo.representation
    :refer [make-char-sequence-representation
            IRepresentation]]
   [clojure.string :as str]
   [hiccup.page :as hp]
   [juxt.reap.alpha.encoders :refer [format-http-date format-content-range]]
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
      :representation (first (::representations res))
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
         {::methods #{:get :put :delete}
          ::representations [(make-comment comment)]})
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
    (assoc ::path path))
   (when (re-matches #"/articles/[a-z][a-z0-9-]*.adoc" path)
     ;; Return a resource that represents the missing article. This is the
     ;; resource that will be added to the database
     {::path path
      ::methods #{:get :head :put :options}
      ::representations []
      ::max-content-length 1024
      ::acceptable
      {"accept" "text/asciidoc,text/plain"
       "accept-charset" "utf-8"}})))

(defn current-representations [db resource]
  (mapcat
   (fn [rep]
     (if (string? rep)
       (current-representations db (get (:resources db) rep))
       [rep]))
   (::representations resource)))

(def *database
  (atom
   { ;; Resources - this contain methods, authorization details, and current representations.
    :resources
    {"/"
     {::methods #{:get :head :options}
      ::representations
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
     {::methods #{:get :head :options}
      ::representations
      ["/en/index.html"
       "/de/index.html"
       "/es/index.html"]}

     "/en/index.html"
     {::methods #{:get :head :options}
      ::representations
      [(index-page-representation
        "Welcome to the spin demo!"
        {"content-language" "en-US"
         "content-location" "/en/index.html"
         "last-modified" (-> "2020-12-25T09:00:00Z"
                             java.time.Instant/parse
                             java.util.Date/from
                             format-http-date)})]}

     "/de/index.html"
     {::methods #{:get :head :options}
      ::representations
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
     {::methods #{:get :head :options}
      ::representations
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
     {::methods #{:get :head :options}
      ::representations
      [(reify
         IRepresentation
         (representation-metadata [_ date _]
           {"content-type" "text/html;charset=utf-8"
            "content-location" "/comments.html"})
         (payload [_ date range {::keys [db]}]
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
     {::methods #{:get :head :options}
      ::representations
      [(reify
         IRepresentation
         (representation-metadata [_ date opts]
           {"content-type" "text/plain;charset=utf-8"
            "content-location" "/comments.txt"})
         (payload [_ date range {::keys [db]}]
           (assert db)
           (let [bytes (.getBytes
                        (str/join
                         (for [{:keys [representation]} (get-comments db)]
                           (str (:char-sequence representation) "\r\n"))))]
             {:headers {"content-length" (str (count bytes))}
              :body (reify StreamableResponseBody
                      (write-body-to-stream [_ response output-stream]
                        (.write output-stream bytes)))})))]
      ::accept-ranges ["bytes" "comments"]}

     "/comments"
     {::methods #{:get :head :post :options}
      ::representations
      ["/comments.html" "/comments.txt"]}

     "/bytes.txt"
     {::methods #{:get :head :options}
      ::representations
      [(let [limit (* 8 100)]
         (reify
           IRepresentation
           (representation-metadata [_ date opts]
             {"content-type" "text/plain;charset=US-ASCII"
              "etag" "\"abc\""
              "last-modified" (format-http-date #inst "2020-12-31T16:00:00Z")})
           (payload [_ date {:juxt.reap.alpha.rfc7233/keys [units byte-range-set] :as range} {::keys [db]}]
             (let [bytes (.getBytes
                          (str/join (map #(format "%08d" %) (clojure.core/range 0 limit 8)))
                          "US-ASCII")]
               (if range
                 (case units
                   "bytes"
                   (if (= (count byte-range-set) 1)
                     (let [{:juxt.reap.alpha.rfc7233/keys
                            [first-byte-pos last-byte-pos suffix-length]} (first byte-range-set)
                           first-byte-pos (or first-byte-pos (- (count bytes) suffix-length))
                           last-byte-pos (if (or (nil? last-byte-pos) (>= last-byte-pos (count bytes)))
                                           (dec (count bytes))
                                           last-byte-pos)

                           _ (when (< last-byte-pos first-byte-pos)
                               (throw
                                (ex-info
                                 "Invalid range"
                                 {:demo.handler/response
                                  {:status 400
                                   :body "Invalid range\r\n"}})))

                           len (or suffix-length (inc (- last-byte-pos first-byte-pos)))

                           body (.toByteArray
                                 (doto (new java.io.ByteArrayOutputStream)
                                   (.write bytes first-byte-pos len)))]


                       (cond-> {:status 206
                                :headers
                                {"content-length" (str len)
                                 "content-range"
                                 (format-content-range
                                  #:juxt.reap.alpha.rfc7233{:units "bytes"
                                                            :first-byte-pos first-byte-pos
                                                            :last-byte-pos last-byte-pos
                                                            :complete-length (count bytes)})}
                                :body body}))

                     ;; There are multiple ranges requested
                     (throw
                      (ex-info
                       "Don't yet do multiple ranges"
                       {:demo.handler/response
                        {:status 501
                         :body "Don't yet do multiple ranges\r\n"}})))

                   ;; Units is not 'bytes'
                   (throw
                    (ex-info
                     "Unsupported range units (TODO)"
                     {:demo.handler/response
                      {:status 500 :body (format "TODO: Suppose range units of %s\r\n" units)}})))

                 {:headers {"content-length" (str (count bytes))}
                  :body bytes})))))]

      ;; to "indicate that it supports range requests for the target resource."
      ;; -- Section 2.3, RFC 7233
      ::accept-ranges ["bytes" "lines"]}}

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

"ns successfully loaded"

;; TODO: Multiple ranges
;; TODO: If-Range
;; TODO: Tests for all in demo-test !!
;; TODO: Promote code from demo to spin
