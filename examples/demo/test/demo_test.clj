;; Copyright © 2020, JUXT LTD.

(ns demo-test
  (:require
   [clojure.string :as str]
   [clojure.test :refer [deftest is use-fixtures]]
   [ring.core.protocols :refer [write-body-to-stream]]
   [clojure.pprint :refer [pprint]]
   [demo :as demo]
   [demo.app :as app]))

(def initial-db @demo.app/*database)

(defmacro run [& body]
  `(do
     (add-tap pprint)
     (with-redefs [demo/*database (atom initial-db)]
       @~body)
     (remove-tap pprint)))

(defn fix-database [f]
  (with-redefs [app/*database (atom initial-db)]
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
    (is (= #{"date"
             "content-type" "content-language"
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
    (is (= #{"date"
             "content-type" "content-language"
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
          :headers {"accept-language" "es, de;q=0.8"}})]
    (is (= 200 status))
    (is (= "de" content-language))
    (is (= #{"date"
             "content-type" "content-language"
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
    (is (= #{"date"
             "content-type" "content-language"
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
  (let [article "= Test Article\r\n"
        response
        (demo/handler
         {:uri "/articles/test.adoc"
          :request-method :put
          :headers {"content-length" (str (count (.getBytes article)))
                    "content-type" "text/asciidoc;charset=utf-8"
                    "if-match" "*"}
          :body (new java.io.ByteArrayInputStream (.getBytes article))})]

    (is (= 412 (:status response))))

  (let [article "= Test Article\r\n"
        response
        (demo/handler
         {:uri "/articles/test.adoc"
          :request-method :put
          :headers {"content-length" (str (count (.getBytes article)))
                    "content-type" "text/asciidoc;charset=utf-8"}
          :body (new java.io.ByteArrayInputStream (.getBytes article))})]

    (is (= 200 (:status response))))

  (let [article "= Test Article\r\n"
        response
        (demo/handler
         {:uri "/articles/test.adoc"
          :request-method :put
          :headers {"content-length" (str (count (.getBytes article)))
                    "content-type" "text/asciidoc;charset=utf-8"
                    "if-match" "*"}
          :body (new java.io.ByteArrayInputStream (.getBytes article))})]

    (is (= 200 (:status response)))))

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

(deftest if-none-match-start-test
  (let [article "= Test Article\r\n"
        response
        (demo/handler
         {:uri "/articles/test.adoc"
          :request-method :put
          :headers {"content-length" (str (count (.getBytes article)))
                    "content-type" "text/asciidoc;charset=utf-8"
                    "if-none-match" "*"}
          :body (new java.io.ByteArrayInputStream (.getBytes article))})]

    (is (= 200 (:status response))))

  (let [article "= Test Article\r\n"
        response
        (demo/handler
         {:uri "/articles/test.adoc"
          :request-method :put
          :headers {"content-length" (str (count (.getBytes article)))
                    "content-type" "text/asciidoc;charset=utf-8"
                    "if-none-match" "*"}
          :body (new java.io.ByteArrayInputStream (.getBytes article))})]

    (is (= 412 (:status response)))))

;; RFC 7233 tests

(deftest accept-ranges-test
  (let [{:keys [headers]}
        (demo/handler
         {:uri "/bytes.txt"
          :request-method :get})]
    (is (= "bytes, lines" (get headers "accept-ranges")))))

(deftest single-byte-range-set-byte-range-spec-test
  (let [{:keys [status headers]}
        (demo/handler
         {:uri "/bytes.txt"
          :request-method :get
          :headers {"range" "bytes=8-15"}})]
    (is (= 206 status))
    (is (= "8" (get headers "content-length")))
    (is (= "bytes 8-15/800" (get headers "content-range")))))

(deftest single-byte-range-set-suffix-byte-range-spec-test
  (let [{:keys [status headers]}
        (demo/handler
         {:uri "/bytes.txt"
          :request-method :get
          :headers {"range" "bytes=-16"}})]
    (is (= 206 status))
    (is (= "16" (get headers "content-length")))
    (is (= "bytes 784-799/800" (get headers "content-range")))))

;; A single range spanning the whole representation is still served as a range
(deftest single-byte-range-set-whole-representation-test
  (let [{:keys [status headers]}
        (demo/handler
         {:uri "/bytes.txt"
          :request-method :get
          :headers {"range" "bytes=0-799"}})]
    (is (= 206 status))
    (is (= "800" (get headers "content-length")))
    (is (= "bytes 0-799/800" (get headers "content-range")))))

(deftest single-byte-range-absent-last-byte-pos-test
  (let [{:keys [status headers] :as response}
        (demo/handler
         {:uri "/bytes.txt"
          :request-method :get
          :headers {"range" "bytes=10-"}})]
    (is (= 206 status))
    (is (= "790" (get headers "content-length")))
    (is (= "bytes 10-799/800" (get headers "content-range")))
    response))

(deftest single-byte-range-invalid-byte-pos-test
  (let [{:keys [status] :as response}
        (demo/handler
         {:uri "/bytes.txt"
          :request-method :get
          :headers {"range" "bytes=20-10"}})]
    (is (= 400 status))
    response))

(deftest if-range-test
  (let [{:keys [status]}
        (demo/handler
         {:uri "/bytes.txt"
          :request-method :get
          :headers {"if-range" "\"abc\""
                    "range" "bytes=10-20"}})]
    (is (= 206 status))))

;; if-range doesn't match etag, therefore, return a full response (200)
(deftest if-range-unmatched-test
  (let [{:keys [status]}
        (demo/handler
         {:uri "/bytes.txt"
          :request-method :get
          :headers {"if-range" "\"xyz\""
                    "range" "bytes=10-20"}})]
    (is (= 200 status))))

(deftest if-range-weak-comparison-test
  (let [{:keys [status]}
        (demo/handler
         {:uri "/bytes.txt"
          :request-method :get
          :headers {"if-range" "W/\"abc\""
                    "range" "bytes=10-20"}})]
    ;; if-range does match etag, but is weak, return a full response (200)
    (is (= 200 status))))

;; if-range does match date, return a partial response (206)
(deftest if-range-date-matches-test
  (let [{:keys [status]}
        (demo/handler
         {:uri "/bytes.txt"
          :request-method :get
          :headers {"if-range" "Thu, 31 Dec 2020 16:00:00 GMT"
                    "range" "bytes=10-20"}})]
    (is (= 206 status))))

;; slightly earlier time in if-range, means client has not got the most recent
;; representation, so a full one is served.
(deftest if-range-date-not-matches-test
  (let [{:keys [status]}
        (demo/handler
         {:uri "/bytes.txt"
          :request-method :get
          :headers {"if-range" "Thu, 31 Dec 2020 12:00:00 GMT"
                    "range" "bytes=10-20"}})]
    (is (= 200 status))))

(deftest multiple-byte-specs-test
  (let [{:keys [status] :as response}
        (demo/handler
         {:uri "/bytes.txt"
          :request-method :get
          :headers {"range" "bytes=10-20,30-40"}})]

    (is (= 206 status))
    (is (re-matches #"^multipart/byteranges; boundary=.*" (get-in response [:headers "content-type"])))
    (is (=  "288" (get-in response [:headers "content-length"])))))

;; TODO: Now go back and read through all of RFC 7232 and RFC 7233, check and
;; annotate source code. (pay attention to strong/weak comparison of if-range)


#_(with-redefs [demo.app/*database (atom initial-db)]
  (demo/handler
         {:uri "/en/index.html"
          :request-method :get
          :headers {"if-modified-since" "Fri, 25 Dec 2020 09:00:00 GMT"}})
  )
