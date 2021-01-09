;; Copyright © 2020-2021, JUXT LTD.

(ns demo
  (:require
   [clojure.java.io :as io]
   [clojure.set :as set]
   [clojure.string :as str]
   [hiccup.page :as hp]
   [juxt.reap.alpha.encoders :refer [format-http-date www-authenticate]]
   [juxt.reap.alpha.decoders :as reap]
   [juxt.spin.alpha.ranges :as ranges]
   [juxt.spin.alpha.representation :refer [make-char-sequence-representation]]
   [juxt.spin.alpha.negotiation :as spin.negotiation]
   [juxt.spin.alpha :as spin]
   [ring.adapter.jetty :as jetty]
   [juxt.reap.alpha.rfc7235 :as rfc7235]
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

(defn current-representations [db resource date]
  (mapcat
   (fn [rep]
     (if (string? rep)
       (current-representations db (get (:resources db) rep) date)
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
      [{::spin/representation-metadata
        {"content-type" "text/html;charset=utf-8"
         "content-location" "/comments.html"}
        ::spin/representation-data
        {::spin/payload-generator ::comments}}]}

     "/comments.txt"
     {::spin/methods #{:get :head :options}
      ::spin/representations
      [{::spin/representation-metadata
        {"content-type" "text/plain;charset=utf-8"
         "content-location" "/comments.txt"}
        ::spin/representation-data
        {::spin/payload-generator ::comments}}]
      ::spin/accept-ranges ["bytes" "comments"]}

     "/comments"
     {::spin/methods #{:get :head :post :options}
      ::spin/representations
      ["/comments.html" "/comments.txt"]}

     "/bytes.txt"
     {::spin/methods #{:get :head :options}
      ::spin/representations
      [(let [limit (* 8 100)
             bytes (.getBytes
                    (str/join (map #(format "%08d" %) (clojure.core/range 0 limit 8)))
                    "US-ASCII")]
         {::spin/representation-metadata
          {"content-type" "text/plain;charset=US-ASCII"
           "etag" "\"abc\""
           "last-modified" (format-http-date #inst "2020-12-31T16:00:00Z")}

          ::spin/representation-data
          {::spin/payload-header-fields {"content-length" (str (count bytes))}
           ::spin/bytes bytes}})]

      ;; to "indicate that it supports range requests for the target resource."
      ;; -- Section 2.3, RFC 7233
      ::spin/accept-ranges ["bytes"]}

     "/protected-area.html"
     {::spin/methods #{:get :head :options}
      ::spin/authentication-scheme "Basic"
      ::spin/realm "Winterfell"
      ::spin/representations
      [{::spin/representation-metadata
        {"content-type" "text/html;charset=utf-8"
         "last-modified" "Tue, 1 Dec 2020 09:00:00 GMT"}
        ::spin/representation-data
        {::spin/payload-header-fields {}
         ::spin/bytes (.getBytes "<h1>Hidden Area</h1><p>Access Granted</p>")}}]
      ::required-role {:get #{::valid-user}
                       :head #{::valid-user}
                       :options #{::valid-user}}}}

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

(defn partial-representation-payload [{::spin/keys [body]}
                                      {:juxt.reap.alpha.rfc7233/keys [units byte-range-set]
                                       :as ranges-specifier}
                                      representation-metadata]
  (case units
    "bytes" (ranges/byte-ranges-payload
             body ranges-specifier representation-metadata)))

(defn representation-payload
  "Returns a map of ::spin/payload-header-fields, ::spin/body and (optionally) ::spin/status for a given representation"
  [representation
   date
   ranges-specifier
   {::keys [db]}]

  (cond

    (and (get-in representation [::spin/representation-data ::spin/payload-header-fields])
         (get-in representation [::spin/representation-data ::spin/bytes]))
    (cond-> {::spin/payload-header-fields (get-in representation [::spin/representation-data ::spin/payload-header-fields])
             ::spin/body (get-in representation [::spin/representation-data ::spin/bytes])}
      ranges-specifier (partial-representation-payload
                        ranges-specifier
                        (::spin/representation-metadata representation)))

    (= (get-in representation [::spin/representation-data ::spin/payload-generator]) ::comments)
    (let [bytes
          (case (get-in representation [::spin/representation-metadata "content-type"])
            "text/html;charset=utf-8"
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
                    ;; NOTE: This feels like a bit of a hack
                    (String. (get-in representation [::spin/representation-data ::spin/bytes]))
                    "&nbsp;"
                    [:small
                     [:a {:href location}
                      "view"]]])]])
              "\r\n\r\n"))

            "text/plain;charset=utf-8"
            (.getBytes
             (str/join
              (for [{:keys [representation]} (get-comments db)]
                (str (String. (get-in representation [::spin/representation-data ::spin/bytes])) "\r\n")))))]
      {::spin/payload-header-fields {"content-length" (str (count bytes))}
       ::spin/body bytes})

    :else
    (throw (ex-info "Error (TODO)" {::spin/response {:status 500 :body "TODO"}}))))

(defn GET
  "The GET method."
  [request resource
   date selected-representation selected-representation-metadata
   opts]

  ;; Check for a 304 Not Modified
  (spin/evaluate-preconditions! request resource selected-representation-metadata)

  ;; "The Range header field is evaluated after evaluating the precondition
  ;; header fields defined in [RFC7232], and only if the result in absence
  ;; of the Range header field would be a 200 (OK) response.  In other
  ;; words, Range is ignored when a conditional GET would result in a 304
  ;; (Not Modified) response.

  (let [ranges-specifier (spin/ranges-specifier request resource selected-representation-metadata)

        ;; Here we determine the status (optional), payload headers and body of
        ;; the representation.
        {::spin/keys [status payload-header-fields body]}
        (representation-payload
         selected-representation
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

               payload-header-fields (merge payload-header-fields))}

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

(defn authenticate
  "Authenticate a request. Return the request with any credentials, roles and
  entitlements added to it. The resource can be used to determine the particular
  Protection Space that it is part of."
  [request resource]
  (let [roles
        (when-let [authorization-header (get-in request [:headers "authorization"])]
          (let [{:juxt.reap.alpha.rfc7235/keys [auth-scheme token68 #_auth-params]}
                (reap/authorization authorization-header)]
            (case auth-scheme
              "Basic"
              (let [[_ user password]
                    (re-matches #"([^:]*):([^:]*)"
                                (String. (.decode (java.util.Base64/getDecoder) token68)))]
                (when (and (= user "mal")
                           (= password "tara"))
                  #{::valid-user})))))]
    (cond-> request
      roles (assoc ::roles roles))))

(defn authorize
  "Return the resource, as it appears to the request after authorization rules
  have been applied."
  [request resource]
  (when-let [required-role (get resource ::required-role)]
    (let [acquired-roles (get request ::roles)]
      (when-not (set/intersection required-role acquired-roles)
        (let [authorization-exists? (get-in request [:headers "authorization"])]
          (throw
           (ex-info
            (if authorization-exists? "Forbidden" "Unauthorized")
            {::spin/response
             {:status (if authorization-exists? 403 401)
              :headers
              (cond-> {}
                (not authorization-exists?)
                (assoc
                 "www-authenticate"
                 (www-authenticate
                  [#::rfc7235{:auth-scheme "Basic"
                              :auth-params
                              [#::rfc7235{:auth-param-name "realm",
                                          :auth-param-value "Winterfell"}]}])))
              :body "Credentials are required to access this page\r\n"}}))))))
  ;; Return the resource, changed if necessary
  resource)

(defn make-handler [*database]
  (fn [request]
    (let [db @*database]
      ;; Check method implemented
      (spin/check-method-not-implemented! request)

      ;; Locate the resource
      (let [resource (locate-resource db (:uri request))
            request (authenticate request resource)
            resource (authorize request resource)]

        ;; Validate the request (check query params, body, other constraints)

        ;; Check method allowed
        (spin/check-method-not-allowed! request resource)

        (let [ ;; Fix the date, this will be used as the message origination
              ;; date.
              date (new java.util.Date)

              ;; Get the 'current' representations of the resource.
              current-representations (current-representations db resource date)

              ;; Check for a 404 Not Found
              _ (when (contains? #{:get :head} (:request-method request))
                  (spin/check-not-found! current-representations))

              ;; Negotiate the best representation
              {:keys [selected-representation
                      selected-representation-metadata]}
              (spin.negotiation/negotiate-representation request current-representations date {::db db})]

          ;; Process the request method
          (case (:request-method request)

            (:get :head)
            (GET request resource date
                 selected-representation selected-representation-metadata
                 {::db db})

            :post
            (POST request resource date {::db-atom *database})

            :put
            (PUT request resource selected-representation-metadata date {::db-atom *database})

            :delete
            (DELETE request resource selected-representation-metadata date {::db-atom *database})

            :options
            (OPTIONS request resource date {::db db})))))))

(defn handler*
  "A handler that throws exceptions on errors."
  [req]
  (let [h (make-handler *database)]
    (h req)))

(defn wrap-exception-handler [h]
  (fn [req]
    (try
      (h req)
      (catch clojure.lang.ExceptionInfo e
        ;;          (tap> e)
        (let [exdata (ex-data e)]
          (or
           (::spin/response exdata)
           {:status 500 :body "Internal Error\r\n"}))))))

(defn handler
  "A handler that handles errors by wrapping in an exception handler."
  [req]
  (let [h (->
           (make-handler *database)
           wrap-exception-handler)]
    (h req)))

(defn run [opts]
  (jetty/run-jetty
   (make-handler *database)
   {:port (Integer/parseInt (get opts "--port" "8080")) :join? true}))
