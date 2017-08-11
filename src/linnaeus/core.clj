(ns linnaeus.core
  (:require [clojure.java.shell :refer [sh]]
            [clojure.java.io :as io]
            [clojure.core.reducers :as r]
            [clojure.string :as string]
            [linnaeus.notenames :as nnames]
            [linnaeus.utils :as utils]))

"\\version \"2.19.59\""

(def ly-version "2.19.59")

(def ly-language ["deutsch" linnaeus.notenames/deutsch])

(def ly-config (atom {:version ly-version
                      :language (first ly-language)
                      :notenames (second ly-language)
                      :csound-midi? true}))

(def ly-parts (atom {}))

(def ly-scores (atom {}))

(def basic
  "  \\version \"2.19.59\"

  {
   c' e' g' e'
   }
  ")


#_(defpart violinI
    (legato (n :c4 1/4) (n :c4) (n :cis4))
    )

(ly-compile-from-string "hentumer" basic)

(defn ly-compile-from-string
  [filename ly-str start-time]
  (let [process (.start (ProcessBuilder. (into-array String ["lilypond" "-o" filename "-"])))
        writer (java.io.PrintWriter. (.getOutputStream process))]
    (.println writer ly-str)
    (future (io/copy (.getErrorStream process) (System/err)))
    ;; (future (io/copy (.getInputStream process) (System/out)))
    ;; (.flush writer)
    (.close writer)
    (future (.waitFor process)
            (println "Finished in"
                     (str (/ (double (- (. System (nanoTime)) start-time)) 1000000.0) "ms")))
    :rendering))

(defn n [note & {:keys [dur] :as opts}]
  (prn "keys opts" (keys opts))
  (if-let [note-form (get (:notenames @ly-config) note)]
    (fn [global]
      (let [lilynote (:ly note-form)
            lilydur  (if dur (int (/ 1 dur)) "")]
        [(str lilynote lilydur)
         (if (:csound-midi? @ly-config) (:midi note-form) (:freq note-form))]))
    (throw (Exception. (str note " not valid notename")))))


(defn defpart [part-name & body]
  (swap! ly-parts assoc (keyword part-name)
         (fn [global]
           (let [;; ly# (map first ~body)
                 timeline (for [expr body]
                            (expr global))]             
             [(format "\n%s = {%s}" part-name (string/join " " (map first timeline))) nil]
             ))))


(defn defscore [score-name headers global & parts]
  (swap! ly-scores assoc (keyword score-name)
         (fn []
           (let [all-parts (mapv #((keyword %) @ly-parts) parts)
                 ly-csnd-v (for [part all-parts]
                             (part global))]
             ;; (prn ((first all-parts) {}))
             [(str (format (str "\\version \"%s\"\n"
                                "\\language \"%s\"\n")
                           (:version @ly-config)
                           (name (:language @ly-config)))
                   (apply str (map first ly-csnd-v))
                   (format "\\score { << %s >>}"
                           (string/join " " (map #(str "\\" %) parts)))) nil]))))

(defn render! [score-name]
  (let [start-time (. System (nanoTime))
        compiled-v (((keyword score-name) @ly-scores))]
    (ly-compile-from-string
     (str score-name)
     (first compiled-v)
     start-time)))


(defpart 'violin
  (n :c3 :dur 1/4)
  (n :d3)
  (n :e3 :dur 1/2)
  (n :f3 :dur 1)
  (n :g3))

(defpart 'viola
  (n :c3 :dur 1/2)
  (n :h2)
  (n :a2)
  (n :g2)
  (n :f2))

(defscore 'partitur {} {}
  'violin
  'viola)

(render! 'partitur)

