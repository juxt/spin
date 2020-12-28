;; Copyright © 2020, JUXT LTD.

(ns demo
  (:require
   [clojure.java.io :as io]
   [clojure.string :as str]
   [hiccup.page :as hp]
   [juxt.spin.alpha :as spin]
   [ring.adapter.jetty :as jetty]
   [ring.core.protocols :refer [StreamableResponseBody]]
   [juxt.pick.alpha.ring :refer [pick decode-maybe]]
   [juxt.pick.alpha.core :refer [rate-representation]]
   [juxt.reap.alpha.encoders :refer [format-http-date]]
   [juxt.reap.alpha.rfc7231 :as rfc7231]
   [juxt.reap.alpha.rfc7232 :as rfc7232]
   [juxt.reap.alpha.ring :refer [headers->decoded-preferences]]
   [juxt.reap.alpha.decoders :as reap]))

(defprotocol IRepresentationMetadata
  (metadata [_] "Return a map containing representation metadata"))

(defrecord ByteArrayRepresentation [bytes metadata]
  StreamableResponseBody
  (write-body-to-stream [_ response output-stream]
    (.write output-stream bytes))
  IRepresentationMetadata
  (metadata [_] metadata))

(defn make-byte-array-representation [bytes content-type]
  (->ByteArrayRepresentation
   bytes
   {"content-type" content-type
    "content-length" (str (count bytes))
    "etag"
    (format
     "\"%s\""        ; etags MUST be wrapped in DQUOTEs
     (hash           ; Clojure's hash function will do, but we could use another
      {:content (vec bytes)
       :content-type content-type}))}))

(defrecord StringRepresentation [s charset metadata]
  StreamableResponseBody
  (write-body-to-stream [body response output-stream]
    (.write output-stream (.getBytes s charset)))
  IRepresentationMetadata
  (metadata [_] metadata))

(defn make-string-representation
  [s content-type]
  (let [charset
        (get-in
         (reap/content-type content-type)
         [:juxt.reap.alpha.rfc7231/parameter-map "charset"] "utf-8")]
    (->StringRepresentation
     s
     charset
     {"content-type" content-type
      "content-length" (str (count (.getBytes s charset)))
      "etag"
      (format
       "\"%s\""      ; etags MUST be wrapped in DQUOTEs
       (hash         ; Clojure's hash function will do, but we could use another
        {:content s
         :content-type content-type}))})))

(defrecord CustomRepresentation [f metadata]
  StreamableResponseBody
  (write-body-to-stream [body response output-stream]
    (f body response output-stream))
  IRepresentationMetadata
  (metadata [_] metadata))

(defn make-custom-representation [f metadata]
  (->CustomRepresentation f metadata))

(defn conj-metadata [o metadata]
  (update o :metadata conj metadata))

(defn make-comment [comment]
  (->
   (make-string-representation
    comment
    "text/plain;charset=utf-8")
   (conj-metadata
    {"content-language" "en-US"
     "last-modified" (format-http-date (new java.util.Date))})))

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
        (conj-metadata
         {"last-modified"
          (-> "2020-12-01T09:00:00Z"
              java.time.Instant/parse
              java.util.Date/from
              format-http-date)}))]}

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
        (conj-metadata
         {"content-language" "en-US"
          "content-location" "/en/index.html"
          "last-modified"
          (-> "2020-12-25T09:00:00Z"
              java.time.Instant/parse
              java.util.Date/from
              format-http-date)}))]}

     "/de/index.html"
     {::methods #{:get :head :options}
      ::representations
      [(->
        (index-page-representation
         "Willkommen zur Spin-Demo!")
        (conj-metadata
         {"content-language" "de"
          "content-location" "/de/index.html"
          "last-modified"
          (-> "2020-12-25T09:00:00Z"
              java.time.Instant/parse
              java.util.Date/from
              format-http-date)}))]}

     "/es/index.html"
     {::methods #{:get :head :options}
      ::representations
      [(->
        (index-page-representation
         "¡Bienvenida a la demo de spin!")
        (conj-metadata
         {"content-language" "es"
          "content-location" "/es/index.html"
          "last-modified"
          (-> "2020-12-25T09:00:00Z"
              java.time.Instant/parse
              java.util.Date/from
              format-http-date)}))]}

     "/comments.html"
     {::methods #{:get :head :options}
      ::representations
      [(make-custom-representation
        (fn [_ {::keys [db]} output-stream]
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
             "\r\n\r\n"))))
        {"content-type" "text/html;charset=utf-8"
         "content-location" "/comments.html"})]}

     "/comments.txt"
     {::methods #{:get :head :options}
      ::representations
      [(make-custom-representation
        (fn [_ {::keys [db]} output-stream]
          (assert db)
          (.write
           output-stream
           (.getBytes
            (str/join
             (for [{:keys [representation]} (get-comments db)]
               (str (:s representation) "\r\n"))))))
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

;; -------

(defn evaluate-if-match!
  "Evaluate an If-None-Match precondition header field in the context of a
  resource. If the precondition is found to be false, an exception is thrown
  with ex-data containing the proper response."
  [request resource selected-representation]
  ;; (All quotes in this function's comments are from Section 3.2, RFC 7232,
  ;; unless otherwise stated).
  (let [header-field (reap/if-match (get-in request [:headers "if-match"]))]
    (cond
      ;; "If the field-value is '*' …"
      (and (map? header-field) (::rfc7232/wildcard header-field))
      ;; "… the condition is false if the origin server does not have a current
      ;; representation for the target resource."
      (when (empty? (::representations resource))
        (throw
         (ex-info
          "If-Match precondition failed"
          {::message "No current representations for resource, so * doesn't match"
           ::response {:status 412
                       :body "Precondition Failed\r\n"}})))

      (sequential? header-field)
      (when-let [rep-etag (some-> (get (metadata selected-representation) "etag") reap/entity-tag)]
        (when-not (seq
                   (for [etag (map ::rfc7232/entity-tag header-field)
                         ;; "An origin server MUST use the strong comparison function
                         ;; when comparing entity-tags"
                         :when (rfc7232/strong-compare-match? etag rep-etag)]
                     etag))
          (throw
           (ex-info
            "If-Match precondition failed"
            {::message "No strong matches between if-match and current representations"
             ::if-match header-field
             ::response
             ;; TODO: "unless it can be determined that the state-changing
             ;; request has already succeeded (see Section 3.1)"
             {:status 412
              :body "Precondition Failed\r\n"}})))))))

;; TODO: Make a test from this:
(comment
  (evaluate-if-match!
   {:headers {"if-match" "\"abc\",\"def\""}}
   {::representations [^{"etag" "\"abc\""} {}]}
   nil))

(defn evaluate-if-none-match!
  "Evaluate an If-None-Match precondition header field in the context of a
  resource and, when applicable, a selected representation. If the precondition
  is found to be false, an exception is thrown with ex-data containing the
  proper response."
  [request resource selected-representation]
  ;; (All quotes in this function's comments are from Section 3.2, RFC 7232,
  ;; unless otherwise stated).
  (let [header-field (reap/if-none-match (get-in request [:headers "if-none-match"]))]
    (cond
      (sequential? header-field)
      (when-let [rep-etag (some-> (get (metadata selected-representation) "etag") reap/entity-tag)]
        ;; "If the field-value is a list of entity-tags, the condition is false
        ;; if one of the listed tags match the entity-tag of the selected
        ;; representation."
        (doseq [etag (map ::rfc7232/entity-tag header-field)]
          ;; "A recipient MUST use the weak comparison function when comparing
          ;; entity-tags …"
          (when (rfc7232/weak-compare-match? etag rep-etag)
            (throw
             (ex-info
              "If-None-Match precondition failed"
              {::message "One of the etags in the if-none-match header matches the selected representation"
               ::entity-tag etag
               ::representation selected-representation
               ::response
               ;; "the origin server MUST respond with either a) the 304 (Not
               ;; Modified) status code if the request method is GET or HEAD …"
               (if (#{:get :head} (:request-method request))
                 {:status 304
                  :body "Not Modified\r\n"}
                 ;; "… or 412 (Precondition Failed) status code for all other
                 ;; request methods."
                 {:status 304
                  :body "Precondition Failed\r\n"})})))))

      ;; "If-None-Match can also be used with a value of '*' …"
      (and (map? header-field) (::rfc7232/wildcard header-field))
      ;; "… the condition is false if the origin server has a current
      ;; representation for the target resource."
      (when (seq (::representations resource))
        (throw
         (ex-info
          "If-None-Match precondition failed"
          {::message "At least one representation already exists for this resource"
           ::resource resource
           ::response
           ;; "the origin server MUST respond with either a) the 304 (Not
           ;; Modified) status code if the request method is GET or HEAD …"
           (if (#{:get :head} (:request-method request))
             {:status 304
              :body "Not Modified\r\n"}
             ;; "… or 412 (Precondition Failed) status code for all other
             ;; request methods."
             {:status 304
              :body "Precondition Failed\r\n"})}))))))

(comment
  (evaluate-if-none-match!
   {:headers {"if-none-match" "\"abc\",\"def\""}}
   {::representations [^{"etag" "\"abc\""} {}]}
   ^{"etag" "\"abc\""} {}))

(comment
  (evaluate-if-none-match!
   {:headers {"if-none-match" "*"}}
   {::representations [^{"etag" "\"abc\""} {}]}
   ^{"etag" "\"abc\""} {}))

(defn evaluate-if-unmodified-since! [if-unmodified-since selected-representation]
  (let [if-unmodified-since-date (::rfc7231/date (reap/http-date if-unmodified-since))
        rep-last-modified-date (some-> (get (metadata selected-representation) "last-modified") reap/http-date ::rfc7231/date)]
    (when (.isAfter
           (.toInstant rep-last-modified-date)
           (.toInstant if-unmodified-since-date))
      (throw
       (ex-info
        "Precondition failed"
        {::representation selected-representation
         ::response
         {:status 412 :body "Precondition Failed\r\n"}})))))

(comment
  (nil?
   (evaluate-if-unmodified-since!
    "Sat, 26 Dec 2020 17:08:50 GMT"
    ^{"last-modified" "Sat, 26 Dec 2020 17:08:50 GMT"} {})))

(comment
  (evaluate-if-unmodified-since!
   "Sat, 26 Dec 2020 17:08:40 GMT"
   ^{"last-modified" "Sat, 26 Dec 2020 17:08:50 GMT"} {}))

(defn evaluate-if-modified-since! [if-modified-since selected-representation]
  (let [if-modified-since-date (::rfc7231/date (reap/http-date if-modified-since))
        rep-last-modified-date (some-> (get (metadata selected-representation) "last-modified") reap/http-date ::rfc7231/date)]
    (when-not (.isAfter
               (.toInstant rep-last-modified-date)
               (.toInstant if-modified-since-date))
      (throw
       (ex-info
        "Not modified"
        {::representation selected-representation
         ::response
         {:status 304 :body "Not Modified\r\n"}})))))

(comment
  (evaluate-if-modified-since!
   "Sat, 26 Dec 2020 17:00:00 GMT"
   ^{"last-modified" "Sat, 26 Dec 2020 17:00:00 GMT"} {}))

(defn evaluate-preconditions!
  "Implementation of Section 6 of RFC 7232."
  [request resource selected-representation]
  ;; "… a server MUST ignore the conditional request header fields … when
  ;; received with a request method that does not involve the selection or
  ;; modification of a selected representation, such as CONNECT, OPTIONS, or
  ;; TRACE." -- Section 5, RFC 7232
  (when (not (#{:connect :options :trace} (:request-method request)))
    (if (get-in request [:headers "if-match"])
      ;; Step 1
      (evaluate-if-match! request resource selected-representation)
      ;; Step 2
      (when-let [if-unmodified-since (get-in request [:headers "if-match"])]
        (evaluate-if-unmodified-since! if-unmodified-since selected-representation)))
    ;; Step 3
    (if (get-in request [:headers "if-none-match"])
      (evaluate-if-none-match! request resource selected-representation)
      ;; Step 4, else branch: if-none-match is not present
      (when (#{:get :head} (:request-method request))
        (when-let [if-modified-since (get-in request [:headers "if-modified-since"])]
          (evaluate-if-modified-since! if-modified-since selected-representation))))

    ;; Step 5 (range requests)
    ;; (TODO)

    ;; Step 6: continue
    ;; TODO: We should attempt to run this in a transaction when modifying the resource
    ))

(defn GET [db request resource selected-representation current-representations vary]
  ;; GET (or HEAD)
  ;; Conditional requests
  (let [response
        ;; TODO: Could we add the Date header here?
        (cond-> {}
          (seq vary)
          (assoc "vary" (str/join ", " vary)))]

    (when (empty? current-representations)
      (throw
       (ex-info
        "Not Found"
        {::response
         {:status 404
          :body "Not Found\r\n"}})))

    (when-not selected-representation
      (throw
       (ex-info
        "Not Acceptable"
        { ;; TODO: Must add list of available representations
         ::response
         {:status 406
          :body "Not Acceptable\r\n"}})))

    (evaluate-preconditions! request resource selected-representation)

    (let [content-location
          (get (metadata selected-representation) "content-location")]
      (cond-> {::db db
               :status 200
               :headers
               (cond-> (conj
                        response
                        (select-keys
                         (metadata selected-representation)
                         ;; representation metadata
                         ["content-type" "content-encoding" "content-language"
                          ;; validators
                          "last-modified" "etag"
                          ;; payload header fields too
                          "content-length" "content-range"]))

                 ;; content-location is only set if different from the effective uri
                 (not= content-location (:uri request))
                 (assoc "content-location" content-location))}

        (= (:request-method request) :get)
        (assoc :body selected-representation)))))

(defn POST [*db request resource]
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

(defn PUT
  "Replace the state of a resource with the state defined by the representation
  enclosed in the request message payload. Neither argument can be nil."
  [*db request resource selected-representation]
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
              :body "Bad Request\r\n"}}
            e)))))

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

  (let [decoded-representation
        (decode-maybe
         (select-keys
          (merge {"content-encoding" "identity"} (:headers request))
          ["content-type"
           "content-encoding"
           "content-language"]))]

    (when-let [acceptable (::acceptable resource)]
      (let [prefs (headers->decoded-preferences acceptable)
            request-rep (rate-representation prefs decoded-representation)]

        (when (or (get prefs "accept") (get prefs "accept-charset"))
          (cond
            (not (contains? (:headers request) "content-type"))
            (throw
             (ex-info
              "Request must contain Content-Type header"
              {::response
               {:status 415
                :body "Unsupported Media Type\r\n"}}))

            (= (:juxt.pick.alpha/content-type-qvalue request-rep) 0.0)
            (throw
             (ex-info
              "The content-type of the request payload is not supported by the resource"
              {::request request
               ::resource resource
               ::acceptable acceptable
               ::content-type (get request-rep "content-type")
               ::response
               {:status 415
                :body "Unsupported Media Type\r\n"}}))

            (and
             (= "text" (get-in request-rep [:juxt.reap.alpha.rfc7231/content-type :juxt.reap.alpha.rfc7231/type]))
             (get prefs "accept-charset")
             (not (contains? (get-in request-rep [:juxt.reap.alpha.rfc7231/content-type :juxt.reap.alpha.rfc7231/parameter-map]) "charset")))
            (throw
             (ex-info
              "The Content-Type header in the request is a text type and is required to specify its charset as a media-type parameter"
              {::request request
               ::resource resource
               ::acceptable acceptable
               ::content-type (get request-rep "content-type")
               ::response
               {:status 415
                :body "Unsupported Media Type\r\n"}}))

            (= (:juxt.pick.alpha/charset-qvalue request-rep) 0.0)
            (throw
             (ex-info
              "The charset of the Content-Type header in the request is not supported by the resource"
              {::request request
               ::resource resource
               ::acceptable acceptable
               ::content-type (get request-rep "content-type")
               ::response
               {:status 415
                :body "Unsupported Media Type\r\n"}}))))

        (when (get prefs "accept-encoding")
          (cond
            (= (:juxt.pick.alpha/content-encoding-qvalue request-rep) 0.0)
            (throw
             (ex-info
              "The content-encoding in the request is not supported by the resource"
              {::request request
               ::resource resource
               ::acceptable acceptable
               ::content-language (get-in request [:headers "content-encoding"] "identity")
               ::response
               {:status 409
                :body "Conflict\r\n"}}))))

        (when (get prefs "accept-language")
          (cond
            (not (contains? (:headers request) "content-language"))
            (throw
             (ex-info
              "Request must contain Content-Language header"
              {::response
               {:status 409
                :body "Conflict\r\n"}}))

            (= (:juxt.pick.alpha/content-language-qvalue request-rep) 0.0)
            (throw
             (ex-info
              "The content-language in the request is not supported by the resource"
              {::request request
               ::resource resource
               ::acceptable acceptable
               ::content-language (get-in request [:headers "content-language"])
               ::response
               {:status 415
                :body "Unsupported Media Type\r\n"}}))))))

    (let [out (java.io.ByteArrayOutputStream.)]
      (with-open [in (:body request)]
        (io/copy in out))

      (let [content-type (:juxt.reap.alpha.rfc7231/content-type decoded-representation)
            charset (get-in decoded-representation [:juxt.reap.alpha.rfc7231/content-type :juxt.reap.alpha.rfc7231/parameter-map "charset"])

            new-representation
            (->
             (case (:juxt.reap.alpha.rfc7231/type content-type)
               "text"
               (make-string-representation
                (new String (.toByteArray out) charset)
                (get decoded-representation "content-type"))

               (make-byte-array-representation
                (.toByteArray out)
                (get decoded-representation "content-type")))

             (conj-metadata
              (merge
               (select-keys
                (:headers request)
                ["content-language" "content-encoding"])
               ;; Add validators
               {"last-modified" (format-http-date (new java.util.Date))})))

            ;; TODO: Model representations at atom in a resource

            new-resource (-> resource
                             (assoc ::representations [new-representation])
                             (dissoc ::path))]

        (swap!
         *db
         (fn [db]
           (evaluate-preconditions! request resource selected-representation)
           (assoc-in
            db [:resources (:uri request)] new-resource)))

        ;; TODO: Return 201
        {:status 200})))

  ;; TODO: Must read 6.3.2 and 7.2 to properly understand 201, especially: "The
  ;; 201 response payload typically describes and links to the resource(s)
  ;; created."

  ;; "If the target resource does not have a current representation and the PUT
  ;; successfully creates one, then the origin server MUST inform the user agent
  ;; by sending a 201 (Created) response."
  )

(defn DELETE [*db request resource selected-representation]
  (swap! *db #(update % :resources dissoc (::path resource)))
  {:status 200 :body "Deleted\r\n"})

(defn handler [request]
  (let [db @*database]
    (try

      ;; Check method implemented
      (when-let [response (spin/not-implemented? request)]
        (throw (ex-info "Method not implemented" {::response response})))

      ;; Locate the resource
      (let [resource (locate-resource db (:uri request))]

        ;; Check method allowed
        (when-let [response
                   (if resource
                     (spin/method-not-allowed? request (::methods resource))
                     ;; We forbid POST, PUT and DELETE on a nil resource
                     (when (#{:put :delete :post} (:request-method request))
                       {:status 405
                        :headers {"allow" (spin/allow-header #{:get :head})}
                        :body "Method Not Allowed\r\n"}))]
          (throw (ex-info "Method not allowed" {::response response})))

        ;; Select the current representation
        (let [current (current-representations db resource)

              ;; Negotiate best representation
              negotiation-result
              (when (seq current)
                (pick
                 request
                 (for [rep current]
                   (assoc (metadata rep) ::representation rep))
                 {:juxt.pick.alpha/vary? true}))

              selected-representation
              (get-in
               negotiation-result
               [:juxt.pick.alpha/representation ::representation])

              vary
              (:juxt.pick.alpha/vary negotiation-result)]

          ;; Process the request method
          (case (:request-method request)
            (:get :head)
            (GET db request resource selected-representation current vary)

            :post
            (POST *database request resource)

            :put
            (PUT *database request resource selected-representation)

            :delete
            (DELETE *database request resource selected-representation)

            :options
            ;; TODO: Allow user to take control of this, e.g. WebDAV
            (spin/options (::methods resource)))))

      (catch clojure.lang.ExceptionInfo e
        ;;(tap> e)
        (let [exdata (ex-data e)]
          (or
           (::response exdata)
           {:status 500 :body "Internal Error\r\n"}))))))

(defn run [opts]
  (jetty/run-jetty
   handler
   {:port (Integer/parseInt (get opts "--port" "8080")) :join? true}))
