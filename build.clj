(ns build
  (:require
   [clojure.java.io :as io]
   [clojure.string :as str]))

(.mkdirs (io/file "target"))

(println "Generating README")

(spit
 "target/README.adoc"
 (str/join
  "\n"
  (mapcat
   (fn [line]
     (if-let [[_ insert] (re-matches #"//// insert: (.*)" line)]
       (let [f (io/file (format "doc/%s.adoc" insert))]
         (if (.exists f)
           (line-seq (io/reader f))
           (throw (ex-info (format "File not found: %s" f) {:file f}))))
       [line]))
   (line-seq (io/reader "README.adoc")))))
