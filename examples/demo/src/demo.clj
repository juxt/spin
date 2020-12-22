;; Copyright © 2020, JUXT LTD.

(ns demo
  (:require
   [clojure.java.io :as io]
   [clojure.string :as str]
   [clojure.set :as set]
   [hiccup.page :as hp]
   [juxt.spin.alpha :as spin]
   [ring.adapter.jetty :as jetty]
   [ring.core.protocols :refer [StreamableResponseBody]]
   [juxt.pick.alpha.ring :refer [pick decode-maybe]]
   [juxt.pick.alpha.core :refer [rate-representation]]
   [juxt.reap.alpha.ring :refer [request->decoded-preferences]]
   [juxt.reap.alpha.decoders :as reap]))

(defrecord ByteArrayRepresentation [bytes]
  StreamableResponseBody
  (write-body-to-stream [body response output-stream]
    (.write output-stream bytes)))

(defn make-byte-array-representation [bytes content-type]
  (with-meta
    (->ByteArrayRepresentation bytes)
    {"content-type" content-type
     "content-length" (str (count bytes))
     "etag"
     (format
      "\"%s\""      ; etags MUST be wrapped in DQUOTEs
      (hash         ; Clojure's hash function will do, but we could use another
       {:content bytes
        :content-type content-type}))}))

(defrecord StringRepresentation [s charset]
  StreamableResponseBody
  (write-body-to-stream [body response output-stream]
    (.write output-stream (.getBytes s charset))))

(defn make-string-representation
  [s content-type]
  (let [charset
        (get-in
         (reap/content-type content-type)
         [:juxt.reap.alpha.rfc7231/parameter-map "charset"] "utf-8")]
    (with-meta
      (->StringRepresentation s charset)
      {"content-type" content-type
       "content-length" (str (count (.getBytes s charset)))
       "etag"
       (format
        "\"%s\""     ; etags MUST be wrapped in DQUOTEs
        (hash        ; Clojure's hash function will do, but we could use another
         {:content s
          :content-type content-type}))})))

(defn conj-meta [o metadata]
  (with-meta o
    (conj (meta o) metadata)))

(defn make-comment [comment]
  (->
   (make-string-representation
    comment
    "text/plain;charset=utf-8")
   (conj-meta {"content-language" "en-US"
               "last-modified" (spin/format-http-date (new java.util.Date))})))

(defn index-page-representation [title]
  (make-string-representation
   (str
    (hp/html5
     [:head
      [:title title]]
     [:body
      [:h1 title]
      [:a {:href "/comments.html"} "Comments"]])
    "\r\n\r\n")
   "text/html;charset=utf-8"))

(defn get-comments [db]
  (->>
   (for [[path res] (:resources db)
           :let [n (second (re-matches #"/comments/(\d+)" path))]
           :when n]
       {:location path
        :representation (first (::representations res))
        :ordinal n})
   (sort-by :ordinal)))

(def *database
  (atom
   { ;; Resources - this contain methods, authorization details, and current representations.
    :resources
    {"/"
     {::methods #{:get :head :options}
      ::representations
      [(->
        (make-string-representation
         (str
          (hp/html5 [:head [:meta {"http-equiv" "Refresh" "content" "0; URL=/index.html"}]])
          "\r\n\r\n")
         "text/html;charset=utf-8")
        (conj-meta
         {"last-modified"
          (-> "2020-12-01T09:00:00Z"
              java.time.Instant/parse
              java.util.Date/from
              spin/format-http-date)}))]}

     "/index.html"
     {::methods #{:get :head :options}
      ::representations
      ["/en/index.html"
       "/de/index.html"
       "/es/index.html"]}

     "/en/index.html"
     {::methods #{:get :head :options}
      ::representations
      [(->
        (index-page-representation
         "Welcome to the spin demo!")
        (conj-meta
         {"content-language" "en-US"
          "content-location" "/en/index.html"
          "last-modified"
          (-> "2020-12-25T09:00:00Z"
              java.time.Instant/parse
              java.util.Date/from
              spin/format-http-date)}))]}

     "/de/index.html"
     {::methods #{:get :head :options}
      ::representations
      [(->
        (index-page-representation
         "Willkommen zur Spin-Demo!")
        (conj-meta
         {"content-language" "de"
          "content-location" "/de/index.html"
          "last-modified"
          (-> "2020-12-25T09:00:00Z"
              java.time.Instant/parse
              java.util.Date/from
              spin/format-http-date)}))]}

     "/es/index.html"
     {::methods #{:get :head :options}
      ::representations
      [(->
        (index-page-representation
         "¡Bienvenida a la demo de spin!")
        (conj-meta
         {"content-language" "es"
          "content-location" "/es/index.html"
          "last-modified"
          (-> "2020-12-25T09:00:00Z"
              java.time.Instant/parse
              java.util.Date/from
              spin/format-http-date)}))]}

     "/comments.html"
     {::methods #{:get :head :options}
      ::representations
      [(with-meta
         (reify
           StreamableResponseBody
           (write-body-to-stream [body {::keys [db]} output-stream]
             (.write
              output-stream
              (.getBytes
               (str
                (hp/html5
                 [:head
                  [:title "Comments"]]
                 [:body
                  [:h1 "Comments"]
                  [:ol
                   (for [{:keys [location representation]} (get-comments db)]
                     [:li
                      (:s representation)
                      "&nbsp;"
                      [:small
                       [:a {:href location}
                        "view"]]])]])
                "\r\n\r\n")))))
         {"content-type" "text/html;charset=utf-8"
          "content-location" "/comments.html"})]}

     "/comments.txt"
     {::methods #{:get :head :options}
      ::representations
      [(with-meta
         (reify
           StreamableResponseBody
           (write-body-to-stream [body {::keys [db]} output-stream]
             (assert db)
             (.write
              output-stream
              (.getBytes
               (str/join
                (for [{:keys [representation]} (get-comments db)]
                  (str (:s representation) "\r\n")))))))
         {"content-type" "text/plain;charset=utf-8"
          "content-location" "/comments.txt"})]}

     "/comments"
     {::methods #{:get :head :post :options}
      ::representations
      ["/comments.html" "/comments.txt"]}}

    :next-comment-id 1

    ;; Representations - these are streams of bytes representing the current, or
    ;; intended, state of a resource. Representations are not automatically
    ;; public. There must be a corresponding resource, supporting at least GET
    ;; method, and there could be authorization aspects too.
    }))

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
      ::max-content-length 5
      ::acceptable {"accept" "text/asciidoc,text/plain"
                    "accept-charset" "utf-8"}})))

(defn current-representations [db resource]
  (mapcat
   (fn [rep]
     (if (string? rep)
       (current-representations db (get (:resources db) rep))
       [rep]))
   (::representations resource)))


#_(map meta
     (current-representations
      @*database
      (get-in @*database [:resources "/index.html"])

      ))

;; ------

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

(swap! *database #(-> %
                      (add-comment "Here is the first comment")
                      (add-comment "Here is another comment")))

(defn post! [*db request resource]
  (assert (= (:uri request) (::path resource)))
  (case (:uri request)
    ;; TODO: Should look at some hint in the resource as to functionality,
    ;; perhaps via a defmethod
    "/comments"
    (do
      (when-not (get-in request [:headers "content-length"])
        (throw
         (ex-info
          "No Content-Length header found"
          {::response
           {:status 411
            :body "Length Required\r\n"}})))

      (when-not (:body request)
        (throw
         (ex-info
          "No body in request"
          {::response
           {:status 400
            :body "Bad Request\r\n"}})))

      ;; TODO: Check input type, can throw a 415 error response if necessary
      ;; (But use reap)
      ;; (if (= (get-in request [:headers "content-type"]) "text/plain"))

      (let [out (java.io.ByteArrayOutputStream.)]
        (with-open [in (:body request)]
          (io/copy in out))
        (spin/created
         (:last-location
          (swap!
           *db
           add-comment
           (String. (.toByteArray out)))))))))

(defn put!
  "Replace the state of a resource with the state defined by the representation
  enclosed in the request message payload. Neither argument can be nil."
  [*db request resource]
  (assert (= (:uri request) (::path resource)))

  ;; If resource just has one representation, we wish to put over it. We should
  ;; be able to do this in the general case.

  (when-not (get-in request [:headers "content-length"])
    (throw
     (ex-info
      "No Content-Length header found"
      {::response
       {:status 411
        :body "Length Required\r\n"}})))

  (if-let [content-length (get-in request [:headers "content-length"])]
    (when-let [max-content-length (::max-content-length resource)]
      (try
        (let [content-length (Long/parseLong content-length)]
          (when (> content-length max-content-length)
            (throw
             (ex-info
              "Payload too large"
              {::response
               {:status 413
                :body "Payload Too Large\r\n"}}))))

        (catch NumberFormatException e
          (throw
           (ex-info
            "Bad content length"
            {::response
             {:status 400
              :body "Bad Request\r\n"}})))))

    ;; No content-length
    (throw
     (ex-info
      "Length Required"
      {::response
       {:status 411
        :body "Length Required\r\n"}})))

  (when-not (:body request)
    (throw
     (ex-info
      "No body in request"
      {::response
       {:status 400
        :body "Bad Request\r\n"}})))

  (when-let [acceptable (::acceptable resource)]
    (let [prefs (request->decoded-preferences {:headers acceptable})
          request-rep (rate-representation
                       prefs
                       (decode-maybe (:headers request)))]
      (when-not (:juxt.pick.alpha/acceptable? request-rep)
        (throw
         (ex-info
          "No body in request"
          {::representation-in-request request-rep
           ::response
           (if (= (:juxt.pick.alpha/content-type-qvalue request-rep) 0.0)
             {:status 415
              :body "Unsupported Media Type\r\n"}
             {:status 409
              :body "Conflict\r\n"})})))))

  (let [out (java.io.ByteArrayOutputStream.)]
    (with-open [in (:body request)]
      (io/copy in out))

    (let [content-type (get-in request [:headers "content-type"])
          representation
          (->
           (make-byte-array-representation
            (.toByteArray out)
            content-type)
           (conj-meta
            (merge
             (select-keys
              (:headers request)
              ["content-language" "content-encoding"])
             ;; Add validators
             {"last-modified" (spin/format-http-date (new java.util.Date))})))

          new-resource (-> resource
                           (assoc ::representations [representation])
                           (dissoc ::path))]

      (swap!
       *db
       (fn [db]
         (assoc-in
          db [:resources (:uri request)] new-resource)))

      ;; TODO: Return 201

      {:status 200}))

  ;; TODO: Must read 6.3.2 and 7.2 to properly understand 201, especially: "The
  ;; 201 response payload typically describes and links to the resource(s)
  ;; created."

  ;; "If the target resource does not have a current representation and the PUT
  ;; successfully creates one, then the origin server MUST inform the user agent
  ;; by sending a 201 (Created) response."
  )

(defn delete! [*db path]
  (swap! *db #(update % :resources dissoc path))
  {:status 200 :body "Deleted\r\n"})

(defn handler [request]
  (let [db @*database]
    (try
      ;; Check method is known
      (if-let [response (spin/unknown-method? request)]
        response

        ;; Locate the resource
        (let [resource (locate-resource db (:uri request))]

          ;; Check method allowed
          (if-let [response
                   (if resource
                     (spin/method-not-allowed? request (::methods resource))
                     ;; We forbid POST, PUT and DELETE on a nil resource
                     (when (#{:put :delete :post} (:request-method request))
                       {:status 405
                        :headers {"allow" (spin/allow-header #{:get :head})}
                        :body "Method Not Allowed\r\n"}))]
            response

            ;; Select the representation (only if GET or HEAD)
            (let [{:keys [representation vary]}
                  (when (#{:get :head} (:request-method request))
                    (let [current (current-representations db resource)]
                      (when (empty? current)
                        (throw
                         (ex-info
                          "Not Found"
                          {::response
                           {:status 404
                            :body "Not Found\r\n"}})))

                      (let [negotiation-result
                            (pick
                             request
                             (for [rep current]
                               (assoc (meta rep) ::representation rep))
                             {:juxt.pick.alpha/vary? true})]

                        (if-let [representation (::representation (:juxt.pick.alpha/representation negotiation-result))]
                          {:representation representation
                           :vary (:juxt.pick.alpha/vary negotiation-result)}
                          (throw
                           (ex-info
                            "Not Acceptable"
                            { ;; TODO: Must add list of available representations
                             ::response
                             {:status 406
                              :body "Not Acceptable\r\n"}}))))))

                  response
                  (cond-> {}
                    (seq vary)
                    (assoc "vary" (str/join ", " vary)))]


              ;; Conditional requests
              (if-let [not-modified-response
                       (spin/not-modified? request (meta representation))]
                not-modified-response

                ;; Process the request method
                (case (:request-method request)
                  (:get :head)
                  ;; GET (or HEAD)
                  (cond-> {::db db
                           :status 200
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
                  (post! *database request resource)

                  :put
                  (put! *database request resource)

                  :delete
                  (delete! *database (:uri request))

                  :options
                  ;; TODO: Allow user to take control of this, e.g. WebDAV
                  (spin/options (::methods resource))))))))

      (catch clojure.lang.ExceptionInfo e
        (let [exdata (ex-data e)]
          (or
           (::response exdata)
           {:status 500 :body "Internal Error\r\n"}))))))

(defn run [opts]
  (jetty/run-jetty
   handler
   {:port (Integer/parseInt (get opts "--port" "8080")) :join? true}))


;;(map to-pick current)
