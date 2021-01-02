;; Copyright © 2020, JUXT LTD.

;; The intention here is to promote this into skip

(ns demo.ranges
  (:require
   [clojure.string :as str]
   [juxt.reap.alpha.encoders :refer [format-content-range]]
   [juxt.reap.alpha.rfc7233 :as rfc7233]))

(defn evaluate-spec [{:juxt.reap.alpha.rfc7233/keys [first-byte-pos last-byte-pos suffix-length]} total-byte-count]
  (let [first-byte-pos (or first-byte-pos (- total-byte-count suffix-length))
        last-byte-pos (if (or (nil? last-byte-pos) (>= last-byte-pos total-byte-count))
                        (dec total-byte-count)
                        last-byte-pos)]

    (when (< last-byte-pos first-byte-pos)
      (throw
       (ex-info
        "Invalid range"
        {:demo.handler/response
         {:status 400
          :body "Invalid range\r\n"}})))

    {:first-byte-pos first-byte-pos
     :last-byte-pos last-byte-pos}))

(defn byte-ranges-payload
  "Return a representation's payload (as per IRepresentation/payload) which
  conforms to the requested bytes range. This function has the downside of
  requiring the entire byte-array of the representation, therefore do not use
  for large byte arrays and instead implement a more efficient custom
  implementation (perhaps using this as a guide)."
  [bytes {:juxt.reap.alpha.rfc7233/keys [units byte-range-set] :as range}]

  (assert range)
  (assert (= units "bytes"))

  (if (= (count byte-range-set) 1)
    (let [{:keys [first-byte-pos last-byte-pos]} (evaluate-spec (first byte-range-set) (count bytes))
          len (inc (- last-byte-pos first-byte-pos))

          body (.toByteArray
                (doto (new java.io.ByteArrayOutputStream)
                  (.write bytes first-byte-pos len)))]

      (cond-> {:status 206
               :headers
               {"content-length" (str len)
                "content-range"
                (format-content-range
                 #:juxt.reap.alpha.rfc7233
                 {:units "bytes"
                  :first-byte-pos first-byte-pos
                  :last-byte-pos last-byte-pos
                  :complete-length (count bytes)})}
               :body body}))

    ;; There are multiple ranges requested
    (let [boundary (apply str (map char (repeatedly 32 #(rand-nth (clojure.core/range (int \a) (inc (int \z)))))))]
      {:status 206
       :headers {"content-type" (format "multipart/byteranges; boundary=%s" boundary)}
       :body (new java.io.SequenceInputStream
                  (java.util.Collections/enumeration
                   (concat
                    (mapcat
                     seq
                     (for [spec byte-range-set
                           :let [{:keys [first-byte-pos last-byte-pos]} (evaluate-spec spec (count bytes))]]
                       [(new java.io.ByteArrayInputStream
                             (->
                              (str
                               (format "--%s\r\n" boundary)
                               "Content-Type: text/plain;charset=US-ASCII\r\n"
                               "Content-Range:"
                               (format-content-range
                                #:juxt.reap.alpha.rfc7233
                                {:units "bytes"
                                 :first-byte-pos first-byte-pos
                                 :last-byte-pos last-byte-pos
                                 :complete-length (count bytes)})
                               "\r\n"
                               "\r\n")
                              (.getBytes "US-ASCII")))

                        (new java.io.ByteArrayInputStream
                             bytes first-byte-pos (inc (- last-byte-pos first-byte-pos)))

                        (new java.io.ByteArrayInputStream
                             (.getBytes "\r\n" "US-ASCII"))]))

                    [(new java.io.ByteArrayInputStream
                          (->
                           (format "--%s--\r\n" boundary)
                           (.getBytes "US-ASCII")))])))})))
