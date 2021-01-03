;; Copyright © 2020-2021, JUXT LTD.

(ns juxt.spin.alpha.representation
  (:require
   [juxt.reap.alpha.decoders :as reap]
   [juxt.spin.alpha :as spin]))

;; The separation of representation-metadata and payload is to allow
;; representation-metadata to be relatively inexpensive—it is called against
;; the representation during content-negotiation, even if the representation
;; isn't selected.
#_(defprotocol IRepresentation
  (representation-metadata [_ date opts]
    "Return a map containing any relevant representation metadata, as at date,
     including content-type, content-encoding, content-language and/or
     content-location. See Section 3.1 of RFC 7231. Also, include any
     metadata (etag, last-modified) that can be used as a 'validator' in a
     precondition. See Section 7.2 of RFC 7231 and all of RFC 7232.")

  (payload [_ metadata date ranges-specifier opts]
    "Return a map containing the representation data, at the given date, as a
     StreamableResponseBody in :body and payload header fields in :headers,
     including content-length, content-range, trailer and transfer-encoding, see
     Sections 3.2 and 3.3 of RFC 7231. In some cases (such as for
     multipart/byteranges responses) it is permissable to also override the
     content-type. The representation's content-type is passed in the
     representation-metadata argument. Usually the date is the time of 'message
     origination'. If ranges-specifier is not nil, return a partial payload."))

(defn make-byte-array-representation [bytes representation-metadata]
  (let [content-type (get representation-metadata "content-type")]
    {::spin/representation-metadata
     (merge
      {"etag"
       (format
        "\"%s\""     ; etags MUST be wrapped in DQUOTEs
        (hash        ; Clojure's hash function will do, but we could use another
         {:content (vec bytes)
          :content-type content-type}))}
      representation-metadata)
     ::spin/representation-data
     {::spin/payload-header-fields {"content-length" (str (count bytes))}
      ::spin/bytes bytes}}))

(defn make-char-sequence-representation
  [char-sequence representation-metadata]
  (let [content-type (get representation-metadata "content-type")
        _ (assert content-type)
        charset
        (get-in
         (reap/content-type content-type)
         [:juxt.reap.alpha.rfc7231/parameter-map "charset"] "utf-8")]
    {::spin/representation-metadata
     (merge
      {"etag"
       (format
        "\"%s\""     ; etags MUST be wrapped in DQUOTEs
        (hash        ; Clojure's hash function will do, but we could use another
         {:content char-sequence
          :content-type content-type}))}
      representation-metadata)
     ::spin/representation-data
     {::spin/payload-header-fields
      {"content-length" (str (count (.getBytes char-sequence charset)))}
      ::spin/bytes (.getBytes char-sequence charset)}}))
