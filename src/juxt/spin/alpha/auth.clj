;; Copyright © 2020-2021, JUXT LTD.

(ns juxt.spin.alpha.auth
  (:require
   [juxt.reap.alpha.decoders :as reap]
   [juxt.reap.alpha.rfc7235 :as rfc7235]
   [juxt.spin.alpha :as spin]
   [juxt.reap.alpha.encoders :refer [www-authenticate]]))

(defmulti authenticate (fn [{:juxt.reap.alpha.rfc7235/keys [auth-scheme]}] auth-scheme))

(defmethod authenticate "Basic" [{:juxt.reap.alpha.rfc7235/keys [token68]}]
  (let [[_ user password]
        (re-matches #"([^:]*):([^:]*)"
                    (String. (.decode (java.util.Base64/getDecoder) token68)))]
    {::user user
     ::password password}))

(defn decode-authorization-header [request]
  (when-let [authorization-header (get-in request [:headers "authorization"])]
    (let [{::rfc7235/keys [auth-scheme] :as m}
          (reap/authorization authorization-header)]
      (merge
       {::spin/auth-scheme auth-scheme}
       (authenticate m)))))

(defn basic-auth-www-authenticate [realm]
  (www-authenticate
   [#::rfc7235{:auth-scheme "Basic"
               :auth-params
               [#::rfc7235{:auth-param-name "realm",
                           :auth-param-value realm}]}]))
