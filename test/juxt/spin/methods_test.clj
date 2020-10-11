;; Copyright © 2020, JUXT LTD.

(ns juxt.spin.methods-test
  (:require
   [clojure.test :refer [deftest is use-fixtures are testing]]
   [clojure.java.io :as io]
   [juxt.spin.alpha.methods :refer [http-method]]
   [juxt.spin.alpha.resource :as r]
   [juxt.spin.alpha.server :as s]
   [ring.mock.request :refer [request]]
   [clojure.tools.logging :as log]
   [clojure.tools.logging.impl :as impl]
   [juxt.spin.alpha.resource :as resource])
  (:import
   (java.util.logging LogManager Logger Level Handler)))

(comment
  (.setLevel (Logger/getLogger Logger/GLOBAL_LOGGER_NAME) Level/FINEST)

  (.setLevel (impl/get-logger log/*logger-factory* *ns*) Level/FINEST)

  (.getLevel (Logger/getLogger "juxt.spin.methods-test"))
  (.getLevel (Logger/getLogger Logger/GLOBAL_LOGGER_NAME))


  (log/trace "foo")

  (log/log* (impl/get-logger log/*logger-factory* *ns*) :trace nil "Hi")

  (impl/enabled? (impl/get-logger log/*logger-factory* *ns*) :trace)

  (.getLevel (impl/get-logger log/*logger-factory* *ns*))

  (type (impl/get-logger log/*logger-factory* *ns*))

  (.getName (impl/get-logger log/*logger-factory* *ns*))

  (.log (impl/get-logger log/*logger-factory* *ns*) Level/FINE "Hi")

  (io/resource "conf/logging.properties")

  (LogManager/getLogManager))

;;(type log/*logger-factory*)


;; Need to add a handler that captures trace-level log messages and asserts on them.
;; These are incestuous internal tests because we need really good tracing in the code to debug requests.

;;(.reset (LogManager/getLogManager))

(comment
  (.reset (LogManager/getLogManager))

  (.addHandler
   (doto (Logger/getGlobal)
     (.setLevel Level/INFO))
   (doto (new java.util.logging.ConsoleHandler)
     (.setLevel Level/INFO)
     ))

  (.addHandler
   (doto (Logger/getLogger "juxt")
     (.setLevel Level/FINEST))
   (doto (new java.util.logging.ConsoleHandler)
     (.setLevel Level/FINEST)
     )))

;; Does juxt propagate log-records up to root? 	YES - but not via global, at least after LM 'reset'

(comment
  (log/info "14")

  (.getUseParentHandlers (Logger/getLogger "juxt"))

  (.log (.getParent (Logger/getLogger "juxt")) Level/INFO "hi")

  (.log (Logger/getGlobal) Level/INFO "hi")

  (.getParent (Logger/getGlobal))

  (.getParent (Logger/getLogger "juxt")))


(comment
  (Logger/getLogger "juxt")

  (seq (.getHandlers (Logger/getLogger "juxt")))

  (seq (.getHandlers (.getParent (Logger/getGlobal))))

  (.setLevel (.getParent (Logger/getGlobal)) Level/FINEST)

  (.setLevel (first (seq (.getHandlers (.getParent (Logger/getGlobal))) )) Level/FINEST)

  (log/trace "1"))


(comment
  (.getUseParentHandlers (Logger/getLogger "juxt"))

  (log/info (impl/get-logger "juxt") "hi")

  (.getUseParentHandlers (impl/get-logger log/*logger-factory* "juxt"))

  (log/info "Hi again from a juxt namespace")

  (.addHandler
   (Logger/getLogger "juxt")
   (doto (new java.util.logging.ConsoleHandler)
     (.setLevel Level/FINEST)
     ))

  (.setUseParentHandlers (Logger/getLogger "juxt") false))

;; What does the logger hierarchy look like if we DON'T reset it???

;;(.getParent (Logger/getLogger "juxt"))
;;(Logger/getGlobal)

(def ^:dynamic  *log-records* nil)


(defn with-log-capture [f]
  (binding [*log-records* (atom [])]
    (let [juxt-logger (Logger/getLogger "juxt")
          h (proxy [Handler] []
              (close [] nil)
              (flush [] nil)
              (publish [lr] (swap! *log-records* conj lr)))
          old-level (.getLevel juxt-logger)
          ]
      (try
        (.addHandler juxt-logger h)
        (.setLevel juxt-logger Level/FINEST)
        (f)
        (finally
          (.setLevel juxt-logger old-level)
          (.removeHandler juxt-logger h))))))

(use-fixtures :each with-log-capture)


;;(defmulti http-method (fn [resource-provider server-provider resource response request respond raise] (:request-method request)))

(def NIL_SERVER_PROVIDER nil)
(def NIL_RESOURCE nil)

(deftest get-test
  (let [response (promise)]
    (http-method
     (reify
       r/GET
       (get-or-head [_ server resource request respond raise]
         (respond {:body "Hello World!"})))
     NIL_SERVER_PROVIDER
     NIL_RESOURCE
     {} ; response
     (request :get "/")
     (fn [r]
       (deliver response r))
     (fn [_]))

    (is (= ["resource-provider satisfies? resource/GET"]
           (map (memfn getMessage) @*log-records*)))
    (is (= {:body "Hello World!"} (deref response 0 :timeout)))))
