;; Copyright © 2020, JUXT LTD.

(ns demo-test
  (:require
   [clojure.test :refer [deftest testing is]]
   [demo :as demo]))

(deftest demo-test
  (is
   (=
    {:status 200
     :headers
     {"vary" "accept-language"
      "content-type" "text/html;charset=utf-8"
      "content-language" "en-US"
      "content-length" "165"
      "content-location" "/en/index.html"
      "etag" (format
              "\"%s\""
              (hash
               {:content (get demo/static-representations "/en/index.html")
                :content-type "text/html;charset=utf-8"
                :content-language "en-US"
                :content-encoding ""}))
      "last-modified" "Fri, 25 Dec 2020 09:00:00 GMT"}
     :body
     "<!DOCTYPE html>\n<html><head><title>Welcome to the spin demo!</title></head><body><h1>Welcome to the spin demo!</h1><a href=\"/comments\">Comments</a></body></html>\r\n\r\n"}

    (demo/handler
     {:uri "/index.html"
      :request-method :get})))

  (testing "proactive content-negotiation on language"
    (is
     (=
      {:status 200
       :headers
       {"vary" "accept-language"
        "content-type" "text/html;charset=utf-8"
        "content-language" "es"
        "content-length" "175"
        "content-location" "/es/index.html"
        "etag" (format
                "\"%s\""
                (hash
                 {:content (get demo/static-representations "/es/index.html")
                  :content-type "text/html;charset=utf-8"
                  :content-language "es"
                  :content-encoding ""}))
        "last-modified" "Fri, 25 Dec 2020 09:00:00 GMT"}
       :body
       "<!DOCTYPE html>\n<html><head><title>¡Bienvenida a la demo de spin!</title></head><body><h1>¡Bienvenida a la demo de spin!</h1><a href=\"/comments\">Comments</a></body></html>\r\n\r\n"}

      (demo/handler
       {:uri "/index.html"
        :request-method :get
        :headers {"accept-language" "es"}}))))

  (testing "location of de index page"
    (is
     (=
      {:status 200
       :headers
       {"content-type" "text/html;charset=utf-8"
        "content-language" "de"
        "content-length" "165"
        "etag"  (format
                 "\"%s\""
                 (hash
                  {:content (get demo/static-representations "/de/index.html")
                   :content-type "text/html;charset=utf-8"
                   :content-language "de"
                   :content-encoding ""}))
        "last-modified" "Fri, 25 Dec 2020 09:00:00 GMT"}
       :body
       "<!DOCTYPE html>\n<html><head><title>Willkommen zur Spin-Demo!</title></head><body><h1>Willkommen zur Spin-Demo!</h1><a href=\"/comments\">Comments</a></body></html>\r\n\r\n"}

      (demo/handler
       {:uri "/de/index.html"
        :request-method :get}))))

  (testing "Prefer Spanish but accept German"
    (is
     (=
      {:status 200
       :headers
       {"content-type" "text/html;charset=utf-8"
        "content-language" "de"
        "content-length" "165"
        "etag" (format
                "\"%s\""
                (hash
                 {:content (get demo/static-representations "/de/index.html")
                  :content-type "text/html;charset=utf-8"
                  :content-language "de"
                  :content-encoding ""}))
        "last-modified" "Fri, 25 Dec 2020 09:00:00 GMT"}
       :body
       "<!DOCTYPE html>\n<html><head><title>Willkommen zur Spin-Demo!</title></head><body><h1>Willkommen zur Spin-Demo!</h1><a href=\"/comments\">Comments</a></body></html>\r\n\r\n"}

      (demo/handler
       {:uri "/de/index.html"
        :request-method :get
        :headers {"accept-language" "es, de;q=0.1"}}))))

  (testing "Not acceptable language"
    (is
     (=
      {:status 406
       :body "Not Acceptable\r\n"}

      (demo/handler
       {:uri "/de/index.html"
        :request-method :get
        :headers {"accept-language" "es"}}))))

  (testing "HEAD"
    (is
     (=
      {:status 200
       :headers
       {"vary" "accept-language"
        "content-type" "text/html;charset=utf-8"
        "content-language" "en-US"
        "content-length" "165"
        "content-location" "/en/index.html"
        "etag" (format
                "\"%s\""
                (hash
                 {:content (get demo/static-representations "/en/index.html")
                  :content-type "text/html;charset=utf-8"
                  :content-language "en-US"
                  :content-encoding ""}))
        "last-modified" "Fri, 25 Dec 2020 09:00:00 GMT"}}
      (demo/handler
       {:uri "/index.html"
        :request-method :head}))))

  (testing "Conditional requests"
    (testing "GET with if-modified-since"
      (is
       (=
        {:status 304
         :body "Not Modified\r\n"}

        (demo/handler
         {:uri "/index.html"
          :request-method :get
          :headers {"accept-language" "en"
                    "if-modified-since" "Fri, 25 Dec 2020 09:00:00 GMT"}})))

      (is
       (=
        {:status 200
         :headers
         {"vary" "accept-language"
          "content-type" "text/html;charset=utf-8"
          "content-language" "en-US"
          "content-length" "165"
          "content-location" "/en/index.html"
          "etag" (format
                  "\"%s\""
                  (hash
                   {:content (get demo/static-representations "/en/index.html")
                    :content-type "text/html;charset=utf-8"
                    :content-language "en-US"
                    :content-encoding ""}))
          "last-modified" "Fri, 25 Dec 2020 09:00:00 GMT"}
         :body
         "<!DOCTYPE html>\n<html><head><title>Welcome to the spin demo!</title></head><body><h1>Welcome to the spin demo!</h1><a href=\"/comments\">Comments</a></body></html>\r\n\r\n"}

        (demo/handler
         {:uri "/index.html"
          :request-method :get
          :headers {"accept-language" "en"
                    "if-modified-since" "Fri, 25 Dec 2020 08:00:00 GMT"}}))))

    (testing "GET with if-none-match"
      (is
       (=
        {:status 304
         :body "Not Modified\r\n"}
        (demo/handler
         {:uri "/index.html"
          :request-method :get
          :headers {"accept-language" "en"
                    "if-none-match" (format
                                     "\"%s\""
                                     (hash
                                      {:content (get demo/static-representations "/en/index.html")
                                       :content-type "text/html;charset=utf-8"
                                       :content-language "en-US"
                                       :content-encoding ""}))}})))

      (is
       (=
        {:status 200
         :headers
         {"vary" "accept-language"
          "content-type" "text/html;charset=utf-8"
          "content-language" "en-US"
          "content-length" "165"
          "content-location" "/en/index.html"
          "etag" (format
                  "\"%s\""
                  (hash
                   {:content (get demo/static-representations "/en/index.html")
                    :content-type "text/html;charset=utf-8"
                    :content-language "en-US"
                    :content-encoding ""}))
          "last-modified" "Fri, 25 Dec 2020 09:00:00 GMT"}
         :body
         "<!DOCTYPE html>\n<html><head><title>Welcome to the spin demo!</title></head><body><h1>Welcome to the spin demo!</h1><a href=\"/comments\">Comments</a></body></html>\r\n\r\n"}
        (demo/handler
         {:uri "/index.html"
          :request-method :get
          :headers {"accept-language" "en"
                    "if-none-match" "\"dummy\""}}))))))

;; TODO: Test for POST on /comments, GET with conneg, and PUTs of individual
;; comments

(comment
  (demo/handler
   {:uri "/comments.txt"
    :request-method :get}))

(comment
  (let [in (.getBytes "This is another comment")]
    (demo/handler
     {:uri "/comments"
      :request-method :post
      :headers {"content-length" (str (count in))}
      :body (java.io.ByteArrayInputStream. in)})))
