(ns linnaeus.core
  (:require [clojure.java.shell :refer [sh]]
            [clojure.java.io :as io]
            [clojure.core.reducers :as r]
            [clojure.string :as string]
            [linnaeus.notenames :as nnames]
            [linnaeus.utils :as utils]))


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

;; (ly-compile-from-string "hentumer" basic)

(defn ly-compile-from-string
  [filename ly-str start-time]
  (let [process (.start (ProcessBuilder. (into-array String ["lilypond" "-o" (str "out/" filename) "-"])))
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


(defn parse-csound-event [dynamic-time current-dur csound-note opts global]
  ;; (prn "csnd opts: " global)
  (let [extra-p-fields (loop [csound-params (:csound-params global)
                              extra-p ""
                              cur-param 4]
                         (let [param-key (keyword (str "p" cur-param))]
                           (if-not (contains? csound-params param-key)
                             extra-p
                             (let [param-name (first (param-key csound-params))]
                               (recur (dissoc csound-params param-key)
                                      (case param-name
                                        :freq (str extra-p " " csound-note)
                                        :amp (str extra-p " " -12)
                                        (or (get (:csound opts) param-name)
                                            (get (:csound global) param-name)
                                            (second (param-key csound-params))))
                                      (inc cur-param))))))]
    (format "i \"%s\" %s %s %s"
            (:part-name global)
            (float dynamic-time)
            (float current-dur)
            extra-p-fields)))

;; Triplets of [ly csnd options]

(defn n [note & {:keys [dur chord? chord-index] :as opts}]
  ;; (prn "keys opts" (keys opts))
  (if-let [note-form (get (:notenames @ly-config) note)]
    (fn [global]
      (let [lilynote (:ly note-form)
            lilydur  (if dur (int (/ 1 dur)) "")
            csound-note (if (:csound-midi? @ly-config)
                          (:midi note-form) (:freq note-form))
            current-dur (if chord?
                          (:last-dur global)
                          (if dur dur (:last-dur global)))
            linear-time (if chord? (:linear-time global) (+ (:last-dur global) (:linear-time global)))
            dynamic-time linear-time
            csound-event (parse-csound-event dynamic-time current-dur csound-note opts global)
            global (assoc global
                          :last-dur current-dur
                          :linear-time linear-time
                          :dynamic-time dynamic-time)]
        ;;{:ly lilynote :csnd nil}
        [(str lilynote lilydur)
         
         csound-event
         global]))
    (throw (Exception. (str note " not valid notename")))))

(defn flatten-lists [body]
  (reduce (fn [init val]
            (if (fn? val)
              (conj init val)
              (if (fn? (first val))
                (into init val)
                (into init (flatten val))))) [] body))

(defn defpart [part-name csound-params & body]
  (swap! ly-parts assoc (keyword part-name)
         (fn [global]
           (let [;; ly# (map first ~body)
                 body (flatten-lists body)
                 global (assoc global
                               :part-name part-name
                               :csound-params csound-params
                               :linear-time 0
                               :dynamic-time 0
                               :last-dur 0)
                 timeline (reduce (fn [init expr]
                                    (if (empty? init)
                                      (let [[ly csnd glob] (expr global)]
                                        (conj init ly csnd glob))
                                      (let [[ly csnd glob] (expr (nth init 2))]
                                        (assoc init
                                               0 (str (nth init 0) " " ly)
                                               1 (str (nth init 1) "\n" csnd)
                                               2 glob))))
                                  [] body)
                 #_(for [expr body]
                     (expr global))]
             ;; (prn timeline)
             (if timeline
               [(format "\n%s = {%s}" part-name (nth timeline 0) ;;part-name (string/join " " (map :ly timeline))
                        ) (nth timeline 1)]
               [nil nil])))))


(defn defscore [score-name headers global & parts]
  (swap! ly-scores assoc (keyword score-name)
         (fn []
           (let [all-parts (mapv #((keyword %) @ly-parts) parts)
                 ly-csnd-v (for [part all-parts]
                             (part global))]
             ;; (prn "ly-csnd-v" ly-csnd-v)
             [(str (format (str "\\version \"%s\"\n"
                                "\\language \"%s\"\n")
                           (:version @ly-config)
                           (name (:language @ly-config)))
                   (apply str (map first ly-csnd-v))
                   (format "\\score { << %s >>}"
                           (string/join " " (map #(str "\\" %) parts))))
              (string/join "\n" (map second ly-csnd-v))]))))

(defn render-debug [score-name]
  (((keyword score-name) @ly-scores)))

(defn render! [score-name]
  (let [start-time (. System (nanoTime))
        compiled-v (((keyword score-name) @ly-scores))]
    (spit (str "out/" score-name ".sco") (second compiled-v))
    (ly-compile-from-string
     (str score-name)
     (first compiled-v)
     start-time)))


(defpart 'violin
  {:p4 [:amp -8] :p5 [:freq 440]}
  (repeat 20
          (list (n :c3 :dur 1/4)
                (n :d3)
                (n :e3 :dur 1/2)
                (n :f3 :dur 1)
                (n :g3))))

(defpart 'viola
  {:p4 [:amp -8] :p5 [:freq 440]}
  (n :c3 :dur 1/8)
  (repeat 10 (list (n :h2) (n :c4)))
  (repeat 10 (list (n :a2)
                   (n :g2)
                   (n :f2)))
  (repeat 40 (list (n :d2)
                   (n :c2)
                   (n :e2))))

(defscore 'partitur {} {}
  'violin
  'viola)

(render! 'partitur)

