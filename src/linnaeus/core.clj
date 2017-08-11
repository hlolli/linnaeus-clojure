(ns linnaeus.core
  #_(:require [clojure.java.shell :refer [sh]]
              [clojure.java.io :as io]
              [clojure.spec.alpha :as spec]))


(def basic
  "  \\version \"2.19.59\"

  {
   c' e' g' e'
   }
  ")

(def basic
  "  \\version \"2.19.59\"

  {
   c' e' g' e' f'
   }
  ")

#_(defpart violinI
    (legato (n :c4 1/4) (n :c4) (n :cis4))
    )

(time (ly-compile-from-string "hentumer" basic))

(defn ly-compile-from-string
  [filename ly-str]
  (let [process (.start (ProcessBuilder. (into-array String ["lilypond" "-o" filename "-"])))
        writer (java.io.PrintWriter. (.getOutputStream process))]
    (.println writer ly-str)
    ;; (.flush writer)
    (.close writer)
    (future (.waitFor process) (println "finished"))))



