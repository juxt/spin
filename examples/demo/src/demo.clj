;; Copyright © 2020, JUXT LTD.

(ns demo
  (:require
   [clojure.string :as str]
   [hiccup.page :as hp]
   [juxt.pick.alpha.ring :refer [pick]]
   [juxt.spin.alpha :as spin]
   [ring.adapter.jetty :as jetty]))

(def resources
  {"/index.html" {::spin/methods #{:get}}
   "/en/index.html" {::spin/methods #{:get}}
   "/de/index.html" {::spin/methods #{:get}}
   "/es/index.html" {::spin/methods #{:get}}})

(defn index-page [title]
  (str
   (hp/html5
    [:head
     [:title title]]
    [:body
     [:h1 title]])
   "\r\n\r\n"))

(def representations
  {"/en/index.html" (index-page "Welcome to the spin demo!")
   "/de/index.html" (index-page "Willkommen zur Spin-Demo!")
   "/es/index.html" (index-page "¡Bienvenida a la demo de spin!")})

(def representation-metadata
  (let [en {"content-type" "text/html;charset=utf-8"
            "content-language" "en-US"
            "content-location" "/en/index.html"
            "content-length" (str (count (get representations "/en/index.html")))}

        de {"content-type" "text/html;charset=utf-8"
            "content-language" "de"
            "content-location" "/de/index.html"
            "content-length" (str (count (get representations "/de/index.html")))}

        es {"content-type" "text/html;charset=utf-8"
            "content-language" "es"
            "content-location" "/es/index.html"
            "content-length" (str (count (get representations "/es/index.html")))}]

    {"/index.html" [en de es]
     "/en/index.html" [en]
     "/de/index.html" [de]
     "/es/index.html" [es]}))

(defn locate-resource [path]
  (when-let [resource
             (get resources path)]
    (conj resource [::spin/path path])))

(defn available-representations [resource]
  (get representation-metadata (::spin/path resource) []))

(defn to-pick [{:strs [content-type content-encoding content-language] :as representation}]
  (cond-> representation
    content-type (conj [:juxt.pick.alpha/content-type content-type])
    content-encoding (conj [:juxt.pick.alpha/content-encoding content-encoding])
    content-language (conj [:juxt.pick.alpha/content-language content-language])))

(defn response-body [representation]
  (when-let [content-location (get representation "content-location")]
    (get representations content-location)))

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

          ;; Select the representation (only if GET)
          (let [{:keys [representation vary]}
                (when (#{:get :head} (:request-method request))
                  (let [available (when resource (available-representations resource))]
                    (when (empty? available)
                      (throw
                       (ex-info
                        "Not Found"
                        {::spin/response {:status 404}})))

                    (let [negotiation-result
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
                           ::spin/response {:status 406}}))))))]

            ;; Process the request method
            (case (:request-method request)
              (:get :head)
              ;; GET (or HEAD)
              (cond-> {:status 200
                       :headers
                       (cond-> {}
                         (seq vary) (conj ["vary" (str/join ", " vary)])

                         true
                         (conj
                          (select-keys
                           representation
                           ;; representation metadata
                           ["content-type" "content-encoding" "content-language"
                            ;; payload header fields too
                            "content-length" "content-range"]))

                         ;; content-location is only set if different from the effective uri
                         (not= (get representation "content-location") (:uri request))
                         (conj ["content-location" (get representation "content-location")]))}

                (= (:request-method request) :get)
                (conj [:body (response-body representation)])))))))

    (catch clojure.lang.ExceptionInfo e
      (let [exdata (ex-data e)]
        (conj
         (or (::spin/response exdata) {:status 500})
         [:body (str (.getMessage e) "\n")])))))

(defonce server (jetty/run-jetty #'handler {:port 8080 :join? false}))
