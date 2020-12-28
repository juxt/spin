;; Copyright © 2020, JUXT LTD.

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

(deftest if-match-precondition-star-test
  (let [article-v1 "= Test Article\r\n"
        response-1
        (demo/handler
         {:uri "/articles/test.adoc"
          :request-method :put
          :headers {"content-length" (str (count (.getBytes article-v1)))
                    "content-type" "text/asciidoc;charset=utf-8"
                    "if-match" "*"}
          :body (new java.io.ByteArrayInputStream (.getBytes article-v1))})]

    (is (= 412 (:status response-1)))))

(deftest if-match-precondition-test
  (let [article-v1 "= Test Article\r\n"

        ;; Should be a 404, document doesn't yet exist
        initial-get
        (demo/handler
         {:uri "/articles/test.adoc"
          :request-method :get})

        _ (is (= 404 (:status initial-get)))

        put-v1
        (demo/handler
         {:uri "/articles/test.adoc"
          :request-method :put
          :headers {"content-length" (str (count (.getBytes article-v1)))
                    "content-type" "text/asciidoc;charset=utf-8"}
          :body (new java.io.ByteArrayInputStream (.getBytes article-v1))})

        _ (is (= 200 (:status put-v1)))

        get-v1
        (demo/handler
         {:uri "/articles/test.adoc"
          :request-method :get})

        _ (is (= 200 (:status get-v1)))
        _ (is (= "\"-73628034\"" (get-in get-v1 [:headers "etag"])))

        article-v2 "= Test Article V2\r\n"

        put-v2
        (demo/handler
         {:uri "/articles/test.adoc"
          :request-method :put
          :headers {"content-length" (str (count (.getBytes article-v2)))
                    "content-type" "text/asciidoc;charset=utf-8"
                    "if-match" (get-in get-v1 [:headers "etag"])}
          :body (new java.io.ByteArrayInputStream (.getBytes article-v2))})

        _ (is (= 200 (:status put-v2)))

        get-v2
        (demo/handler
         {:uri "/articles/test.adoc"
          :request-method :get})

        _ (is (= 200 (:status get-v2)))
        _ (is (= "\"-1459800632\"" (get-in get-v2 [:headers "etag"])))

        ;; TODO: There are opportunities here for using if-match in GETs to
        ;; ensure the representation has been applied.

        ;; Note Section 3.1, RFC 7232 states that If-Match "can also be used
        ;; with safe methods to abort a request if the selected representation
        ;; does not match one already stored (or partially stored) from a prior
        ;; request". This is what we test for here. By sending the 'old'
        ;; entity-tag (from response-2) in an If-Match header, we should cause
        ;; the precondition to fail and receive a 412.
        get-v1-stale
        (demo/handler
         {:uri "/articles/test.adoc"
          :request-method :get
          :headers {"if-match" (get-in get-v1 [:headers "etag"])}})

        _ (is (= 412 (:status get-v1-stale)))

        article-v3 "= Test Article V2 (avoid lost update)\r\n"

        put-v3-fail
        (demo/handler
         {:uri "/articles/test.adoc"
          :request-method :put
          :headers {"content-length" (str (count (.getBytes article-v3)))
                    "content-type" "text/asciidoc;charset=utf-8"
                    "if-match" (get-in get-v1 [:headers "etag"])}
          :body (new java.io.ByteArrayInputStream (.getBytes article-v3))})

        _ (is (= 412 (:status put-v3-fail)))

        put-v3-succeed
        (demo/handler
         {:uri "/articles/test.adoc"
          :request-method :put
          :headers {"content-length" (str (count (.getBytes article-v3)))
                    "content-type" "text/asciidoc;charset=utf-8"
                    "if-match" (get-in get-v2 [:headers "etag"])}
          :body (new java.io.ByteArrayInputStream (.getBytes article-v3))})

        _ (is (= 200 (:status put-v3-succeed)))]))

(comment
  (with-redefs [demo/*database (atom initial-db)]
    ))
