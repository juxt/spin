;; Copyright © 2020, JUXT LTD.

;; (remove-ns 'demo-test)

(ns demo-test
  (:require
   [clojure.string :as str]
   [clojure.test :refer [deftest is use-fixtures]]
   [ring.core.protocols :refer [write-body-to-stream]]
   [clojure.pprint :refer [pprint]]
   [demo :as demo]))

(def initial-db @demo/*database)

(defmacro run [& body]
  `(do
     (add-tap pprint)
     (with-redefs [demo/*database (atom initial-db)]
       @~body)
     (remove-tap pprint)))

(defn fix-database [f]
  (with-redefs [demo/*database (atom initial-db)]
    (f)))

(defn tap-errors [f]
  (add-tap pprint)
  (f)
  (Thread/sleep 20)
  (remove-tap pprint))

(use-fixtures :each fix-database tap-errors)

(deftest not-implemented-test
  (let [{status :status}
        (demo/handler
         {:uri "/index.html"
          :request-method :brew})]
    (is (= 501 status))))

(deftest get-test
  (let [{status :status
         {:strs [content-type content-length content-language]} :headers
         :as response}
        (demo/handler
         {:uri "/en/index.html"
          :request-method :get})]
    (is (= 200 status))
    (is (= "text/html;charset=utf-8" content-type))
    (is (= "170" content-length))
    (is (= "en-US" content-language))
    (is (= #{"content-type" "content-language"
             "last-modified" "etag"
             "content-length"}
           (set (keys (:headers response)))))
    (is (:body response))))

(deftest head-test
  (let [{status :status
         {:strs [content-type content-length content-language]} :headers
         :as response}
        (demo/handler {:uri "/en/index.html"
                       :request-method :head})]
    (is (= 200 status))
    (is (= "text/html;charset=utf-8" content-type))
    (is (= "170" content-length))
    (is (= "en-US" content-language))
    (is (= #{"content-type" "content-language"
             "last-modified" "etag"
             "content-length"}
           (set (keys (:headers response)))))
    (is (not (:body response)))))

(deftest get-with-accept-language-test
  (let [{status :status
         {:strs [content-language]} :headers
         :as response}
        (demo/handler
         {:uri "/de/index.html"
          :request-method :get
          :headers {"accept-language" "es, de=0.8"}})]
    (is (= 200 status))
    (is (= "de" content-language))
    (is (= #{"content-type" "content-language"
             "last-modified" "etag"
             "content-length"}
           (set (keys (:headers response)))))))

(deftest get-with-unacceptable-language-test
  (let [{status :status}
        (demo/handler
         {:uri "/de/index.html"
          :request-method :get
          :headers {"accept-language" "es"}})]
    (is (= 406 status))))

(deftest get-with-proactive-content-negotiation-default-language-test
  (let [{status :status
         {:strs [content-type content-length content-language content-location vary]} :headers
         :as response}
        (demo/handler {:uri "/index.html"
                       :request-method :get})]
    (is (= 200 status))
    (is (= "text/html;charset=utf-8" content-type))
    (is (= "170" content-length))
    (is (= "en-US" content-language))
    (is (= #{"content-type" "content-language"
             "content-location"
             "last-modified" "etag"
             "content-length"
             "vary"}
           (set (keys (:headers response)))))
    (is (= "/en/index.html" content-location))
    (is (= "accept-language" vary))))

(deftest get-with-proactive-content-negotiation-accept-language-test
  (let [{status :status
         {:strs [content-language content-location vary]} :headers}
        (demo/handler {:uri "/index.html"
                       :request-method :get
                       :headers {"accept-language" "es"}})]
    (is (= 200 status))
    (is (= "es" content-language))

    (is (= "/es/index.html" content-location))
    (is (= "accept-language" vary))))

(deftest get-with-if-modified-since-test
  (let [{status :status}
        (demo/handler
         {:uri "/en/index.html"
          :request-method :get
          :headers {"if-modified-since" "Fri, 25 Dec 2020 09:00:00 GMT"}})]
    (is (= 304 status)))
  (let [{status :status}
        (demo/handler
         {:uri "/en/index.html"
          :request-method :get
          :headers {"if-modified-since" "Fri, 25 Dec 2020 08:00:00 GMT"}})]
    (is (= 200 status))))

(deftest get-with-if-none-match-test
  (let [{status1 :status
         {:strs [etag]} :headers}
        (demo/handler
         {:uri "/en/index.html"
          :request-method :get})
        {status2 :status}
        (demo/handler
         {:uri "/en/index.html"
          :request-method :get
          :headers {"if-none-match" etag}})]
    (is (= 200 status1))
    (is (= 304 status2))))

(deftest disallowed-method-test
  (let [{status :status
         {:strs [allow] :as headers} :headers}
        (demo/handler
         {:uri "/index.html"
          :request-method :put})]
    (is (= 405 status))
    (is (= "GET, HEAD, OPTIONS" allow))
    (is (= #{"allow"}
           (set (keys headers))))))

(deftest put-but-no-content-length-test
  (let [{status :status}
        (demo/handler
         {:uri "/articles/test.adoc"
          :request-method :put})]
    (is (= 411 status))))

(deftest put-but-badly-formatted-length-test
  (let [{status :status}
        (demo/handler
         {:uri "/articles/test.adoc"
          :request-method :put
          :headers {"content-length" "10a"}})]
    (is (= 400 status))))

(deftest put-but-no-body-test
  (let [article "= Test Article\r\n"
        {status :status}
        (demo/handler
         {:uri "/articles/test.adoc"
          :request-method :put
          :headers {"content-length" (str (count (.getBytes article)))}})]
    (is (= 400 status))))

(deftest put-but-body-too-large-test
  (let [article (str/join " " (repeat 1000 "blah"))
        {status :status}
        (demo/handler
         {:uri "/articles/test.adoc"
          :request-method :put
          :headers {"content-length" (str (count (.getBytes article)))}})]
    (is (= 413 status))))

(deftest put-but-no-content-type-test
  (let [article "= Test Article\r\n"
        {status :status}
        (demo/handler
         {:uri "/articles/test.adoc"
          :request-method :put
          :headers {"content-length" (str (count (.getBytes article)))}
          :body (new java.io.ByteArrayInputStream (.getBytes article))})]
    (is (= 415 status))))

(deftest put-but-no-content-type-charset-test
  (let [article "= Test Article\r\n"
        {status :status}
        (demo/handler
         {:uri "/articles/test.adoc"
          :request-method :put
          :headers {"content-length" (str (count (.getBytes article)))
                    "content-type" "text/asciidoc"}
          :body (new java.io.ByteArrayInputStream (.getBytes article))})]
    (is (= 415 status))))

(deftest put-but-bad-content-type-charset-test
  (let [article "= Test Article\r\n"
        {status :status}
        (demo/handler
         {:uri "/articles/test.adoc"
          :request-method :put
          :headers {"content-length" (str (count (.getBytes article)))
                    "content-type" "text/asciidoc;charset=utf-16"}
          :body (new java.io.ByteArrayInputStream (.getBytes article))})]
    (is (= 415 status))))

(deftest put-with-body-test
  (let [article "= Test Article\r\n"
        {status :status}
        (demo/handler
         {:uri "/articles/test.adoc"
          :request-method :put
          :headers {"content-length" (str (count (.getBytes article)))
                    "content-type" "text/asciidoc;charset=utf-8"}
          :body (new java.io.ByteArrayInputStream (.getBytes article))})]
    (is (= 200 status))))

;; TODO: Need some way of testing language restrictions - perhaps we can put
;; over /es/index.html? perhaps locate-resource can add accept-language="es"
;; there?

#_(with-redefs [demo/*database (atom initial-db)]

  )

(defn get-body-str [response]
  (let [out (new java.io.ByteArrayOutputStream)]
    (write-body-to-stream (:body response) response out)
    (new String (.toByteArray out))))

(deftest put-with-body-then-get-request-test
  (let [article "= Test Article\r\n"
        {status-1 :status}
        (demo/handler
         {:uri "/articles/test.adoc"
          :request-method :put
          :headers {"content-length" (str (count (.getBytes article)))
                    "content-type" "text/asciidoc;charset=utf-8"}
          :body (new java.io.ByteArrayInputStream (.getBytes article))})
        {status-2 :status :as response}
        (demo/handler
         {:uri "/articles/test.adoc"
          :request-method :get})]

    (is (= 200 status-1))
    (is (= 200 status-2))
    (is (= article (get-body-str response)))))

#_(with-redefs [demo/*database (atom initial-db)]
  (let [article "= Test Article\r\n"
        {status :status}
        (demo/handler
         {:uri "/articles/test.adoc"
          :request-method :put
          :headers {"content-length" (str (count (.getBytes article)))}
          :body (new java.io.ByteArrayInputStream (.getBytes article))})]
    (is (= 200 status))))


#_(with-redefs [demo/*database (atom initial-db)]
  (let [article "= Test Article\r\n"
        {status :status}
        (demo/handler
         {:uri "/articles/test.adoc"
          :request-method :put
          :headers {"content-length" (str (count (.getBytes article)))}
          :body (new java.io.ByteArrayInputStream (.getBytes article))})]
    (is (= 200 status)))


  (let [comment "this is a test"
        {status-on-put :status}
        (demo/handler
         {:uri "/comments/1"
          :request-method :put
          :headers {"content-length" (str (count (.getBytes comment)))}
          :body (new java.io.ByteArrayInputStream (.getBytes comment))})
        {status-on-get :status :as response}
        (demo/handler
         {:uri "/comments/1"
          :request-method :get})
        body (String. (:bytes (:body response)))
        {status-on-index-get :status :as response}
        (demo/handler
         {:uri "/comments.txt"
          :request-method :get})
        ]
    (is (= comment body))
    (body->str response)
    )
  )

;; TODO: Test adoc resource config restriction (then commit)

;; TODO: GET, then PUT over, then repeat PUT again with failure

;; Add conditional tests for weak/strong comparisons, PUTs, etc.

#_(deftest demo-test

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

#_(comment
  (demo/handler
   {:uri "/comments.txt"
    :request-method :get}))

#_(comment
  (let [in (.getBytes "This is another comment")]
    (demo/handler
     {:uri "/comments"
      :request-method :post
      :headers {"content-length" (str (count in))}
      :body (java.io.ByteArrayInputStream. in)})))
