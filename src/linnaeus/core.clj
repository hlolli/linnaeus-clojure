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


(defn parse-csound-event [time-seconds current-dur csound-note opts global]
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
                                      (inc cur-param))))))
        ;; _ (prn (:bpm global))
        p3-seconds (float (* (/ 60 (:bpm global))
                             (/ current-dur (:beat-len global))))]
    (format "i \"%s\" %s %s %s"
            (:part-name global)
            (float time-seconds)
            p3-seconds
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
            global (merge global (if-let [global-fn (:global-fn global)]
                                   (global-fn linear-time) {}))
            time-seconds (if (contains? global :time-seconds)
                           (+ (:time-seconds global)
                              (* (/ 60 (:bpm global)) (:phase global)))
                           linear-time)
            ;; _ (println "Dynamic-time: " (float dynamic-time) "\n Linear-time: " (float linear-time))
            csound-event (parse-csound-event time-seconds current-dur csound-note opts global)
            global (assoc global
                          :last-dur current-dur
                          :linear-time linear-time)]
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

(def global-defaults
  "global = {\\numericTimeSignature \\tempo \"Andante\" 4 = 90}")

(defn defpart [part-name metadata & body]
  (swap! ly-parts assoc (keyword part-name)
         (fn [global]
           (let [;; ly# (map first ~body)
                 body (flatten-lists body)
                 global (assoc global
                               :part-name part-name
                               :csound-params (:csound-params metadata)
                               :linear-time 0
                               :time-seconds 0
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
               [(format (str "\n%s = \\new Staff \\with {"
                             "instrumentName = \"%s\" "
                             "shortInstrumentName = \"%s\" "
                             "} <<{%s} {\\global}>>")
                        (or (:instrument-name metadata) part-name)
                        (or (:instrument-name metadata) part-name)
                        (or (:short-instrument-name metadata) (str (subs (str part-name) 0 3) "."))
                        (nth timeline 0))
                (nth timeline 1)]
               [nil nil])))))

;;\\tempo \"Andante\" 4 = 90

(defn defscore [score-name headers & parts]
  (swap! ly-scores assoc (keyword score-name)
         (fn []
           (let [header (apply str (for [h headers] (str (name (first h)) " = " "\"" (second h) "\"")))
                 global-from-state ((keyword (str score-name "-global")) @ly-parts)
                 global (if global-from-state
                          {:global-fn (second global-from-state)}
                          ;; default data
                          {:bpm 90})
                 ;; global (if global-from-state (second global-from-state) {:bpm 90})
                 global-ly (if global-from-state (first global-from-state)
                               "\nglobal = {\\numericTimeSignature }\n")
                 all-parts (mapv #((keyword %) @ly-parts) parts)
                 ly-csnd-v (for [part all-parts]
                             (part global))]
             ;; (prn "helló?")
             ;; (prn "ly-csnd-v" ly-csnd-v)
             [(str (format (str "\\version \"%s\"\n"
                                "\\language \"%s\"\n")
                           (:version @ly-config)
                           (name (:language @ly-config)))
                   (format "\\header {\n%s\n}" header)
                   global-ly
                   (apply str (map first ly-csnd-v))
                   (format "\\score { << %s >>}"
                           (string/join " " (map #(str "\\" %) parts))))
              (string/join "\n" (map second ly-csnd-v))]))))

(defn time-signature [bar-len beat-len]
  [(format "\n\\time %s/%s" bar-len beat-len)
   {:time-signature [bar-len (/ 1 beat-len)]}])


(defn tempo [tempo-fn ly-text]
  [(if (or (empty? ly-text) (nil? ly-text))
     nil
     [(format "\n\\tempo \"%s\"" ly-text)
      {:tempo tempo-fn}])])

(defn global-key-signature
  "change key signature globally
   no-op on csound output"
  [root mode]
  [(format "\n\\key %s \\%s" (name root) (name mode))
   nil])

(defn timeline-drop [index]
  (fn [v]
    (if (>= 1 (count v))
      v
      (if (>= index (first (second v)))
        (rest v) v))))


;; (get-data-from-global {:bpm-timeline [[0 90] [4 60] [8 30]] :bpm 90
;;                        :time-signature-timeline [[0N 1 1/4] [1/4 2 1/4] [1/2 7 1/4] [3/4 3 1/4] [1N 1 1/2]]
;;                        :bar-len 1 :beat-len 1/4
;;                        ;; Note to self: bar and beat start at 1
;;                        :bar 1 :beat 1 :time-seconds 0} 0 0.5)


(defn get-data-from-global [initial-global-map index phase]
  (-> (nth (iterate (fn [global-map]
                      (-> global-map
                          (update :tempo-timeline (timeline-drop index))
                          (as-> global-map'
                              (update global-map' :bpm
                                      (fn [_] ((-> (:tempo-timeline global-map')
                                                   first second) global-map'))))
                          (update :time-signature-timeline (timeline-drop index))
                          (as-> global-map'
                              (update global-map' :bar-len
                                      (fn [bar-len] (-> (:time-signature-timeline global-map')
                                                        first second)))
                            (update global-map' :beat-len
                                    (fn [bar-len] (-> (:time-signature-timeline global-map')
                                                      first (nth 2)))))
                          (as-> global-map'
                              (update global-map'
                                      :time-seconds (fn [time-seconds]
                                                      (let [sec-per-beat (/ 60 (:bpm global-map'))]
                                                        (+ sec-per-beat time-seconds)))))
                          (as-> global-map'
                              (update global-map' :beat
                                      (fn [beat] (if (== beat (:bar-len global-map'))
                                                   1 (inc beat)))))
                          (as-> global-map'
                              (update global-map' :bar
                                      (fn [bar] (if (== 1 (:beat global-map'))
                                                  (inc bar) bar))))))
                    initial-global-map) index)
      (assoc :phase phase)))

(defn do-at
  ([bar change]
   (if (> 1 bar)
     (throw (Exception. (str bar ", musical bar must start at number 1")))
     (list bar 1 change)))
  ([bar beat change]
   (if (or (> 1 bar)
           (> 1 beat))
     (throw (Exception. (str bar beat ", musical bar or beat must start at number 1")))
     (list bar beat change))))

(defn defglobal [score-name & body]
  (swap! ly-parts assoc (keyword (str score-name "-global"))
         (let [time-signatures (->> (filter #(contains? (second (last %)) :time-signature) body)
                                    (sort #(< (first %1) (first %2))))
               tempo-indicators(->> (filter #(contains? (second (last %)) :tempo) body)
                                    (sort #(< (first %1) (first %2))))
               [initial-bar-len initial-beat-len] (if (= 1 (ffirst time-signatures))
                                                    (:time-signature (second (last (first time-signatures))))
                                                    [4 1/4])
               bb-times-mapping (loop [times time-signatures
                                       bb-times []
                                       last-change-bar 1
                                       last-change-linear-time 0
                                       cur-bar-len initial-bar-len
                                       cur-beat-len initial-beat-len]
                                  (if (empty? times)
                                    bb-times
                                    (let [time-sig (:time-signature (second (last (first times))))]
                                      (if (= 1 (ffirst times))
                                        (recur (rest times)
                                               bb-times
                                               last-change-bar
                                               last-change-linear-time
                                               (first time-sig)
                                               (second time-sig))
                                        (let [cur-change-bar (ffirst times)
                                              cur-change-linear-time (+ last-change-linear-time
                                                                        (* cur-bar-len
                                                                           (- cur-change-bar
                                                                              last-change-bar)))]
                                          (recur (rest times)
                                                 ;; Spec: triplet [bar linear-time [bar-len beat-len]]
                                                 (conj bb-times [(ffirst times)
                                                                 cur-change-linear-time
                                                                 [(first time-sig)
                                                                  (second time-sig)]])
                                                 cur-change-bar
                                                 cur-change-linear-time
                                                 (first time-sig)
                                                 (second time-sig)))))))
               nearest-mapping-fn (fn [bar beat]
                                    (loop [time-map bb-times-mapping
                                           cur-time-map (if (= 1 (ffirst bb-times-mapping))
                                                          (first bb-times-mapping)
                                                          [1 0 [initial-bar-len initial-beat-len]])]
                                      (if (empty? time-map)
                                        cur-time-map
                                        (if (and (>= beat (first cur-time-map))
                                                 (< beat (ffirst time-map)))
                                          cur-time-map
                                          (recur (rest time-map)
                                                 (first time-map))))))
               ;; todo: rename index to linear-time??
               bb-to-index (fn [bar beat]
                             (if (empty? bb-times-mapping)
                               (+ (* initial-bar-len initial-beat-len (dec beat))
                                  (* initial-beat-len (dec bar)))
                               (let [nearest-mapping (nearest-mapping-fn bar beat)]
                                 (+ (* (- bar (first nearest-mapping))
                                       (first (last nearest-mapping))
                                       (second (last nearest-mapping)))
                                    (* (dec beat) (second (last nearest-mapping)))
                                    (second nearest-mapping)))))
               
               linear-time-map (if (empty? bb-times-mapping)
                                 []
                                 (loop [bb-time bb-times-mapping
                                        linear-time []
                                        cur-bar-len initial-bar-len
                                        last-bar 1
                                        last-linear-time 0]
                                   (if (empty? bb-time)
                                     linear-time
                                     (let [new-bar (ffirst bb-time)
                                           new-linear-time (+ (* cur-bar-len
                                                                 (if (empty? linear-time)
                                                                   (dec (ffirst bb-time))
                                                                   (- new-bar last-bar)))
                                                              last-linear-time)]
                                       (recur (rest bb-time)
                                              (conj linear-time [new-linear-time
                                                                 (last (first bb-time))])
                                              (-> bb-time first last first)
                                              new-bar
                                              new-linear-time)))))

               linear-time-nearest-mapping-fn (fn [linear-time]
                                                (loop [time-map linear-time-map
                                                       cur-time-map (if (zero? (ffirst linear-time-map))
                                                                      (first linear-time-map)
                                                                      [0 [initial-bar-len initial-beat-len]])]
                                                  ;; (prn "timemap" time-map "cur-time-map" cur-time-map)
                                                  (if (empty? time-map)
                                                    cur-time-map
                                                    (if (and (>= linear-time (first cur-time-map))
                                                             (< linear-time (ffirst time-map)))
                                                      cur-time-map
                                                      (recur (rest time-map)
                                                             (first time-map))))))
               linear-time-to-index (fn [linear-time]
                                      (if (empty? linear-time-map)
                                        ;; duplet [index beat-phase]
                                        (let [index (quot linear-time initial-beat-len)
                                              beat-phase (/ (- (/ linear-time index) initial-beat-len)
                                                            initial-beat-len)
                                              beat-phase (if (zero? index)
                                                           (/ linear-time initial-beat-len)
                                                           beat-phase)]
                                          [index beat-phase])
                                        (let [nearest-mapping (linear-time-nearest-mapping-fn linear-time)
                                              ;; _ (prn nearest-mapping)
                                              index-to-point (quot (first nearest-mapping)
                                                                   (-> nearest-mapping last second))
                                              index-from-last-point (quot (- linear-time (first nearest-mapping))
                                                                          (-> nearest-mapping last second))
                                              index (+ index-to-point index-from-last-point)
                                              beat-phase (if (zero? index-from-last-point)
                                                           (/ (- linear-time (first nearest-mapping))
                                                              (-> nearest-mapping last second))
                                                           (/ (- (/ (- linear-time (first nearest-mapping))
                                                                    index-from-last-point)
                                                                 (-> nearest-mapping last second))
                                                              (-> nearest-mapping last second)))]
                                          ;; (prn "lineat-time: " linear-time "index: " index "phase: " beat-phase)
                                          [index beat-phase])))

               tempo-timeline (if (empty? tempo-indicators)
                                [[0 (constantly 90)]]
                                (let [tempos (apply vector (for [tempo tempo-indicators]
                                                             (into [(apply bb-to-index (subvec (vec tempo) 0 2))]
                                                                   (:tempo (last tempo)))))]
                                  (if (zero? (ffirst tempos))
                                    tempos
                                    (into [[0 (constantly 90)]]
                                          tempos))))

               time-signature-timeline (if (empty? time-signatures)
                                         [[0 initial-bar-len initial-beat-len]]
                                         (let [times-timeline (apply vector (for [times time-signatures]
                                                                              (into [(apply bb-to-index (subvec (vec times) 0 2))]
                                                                                    (:time-signature (last (last times))))))]
                                           (if (zero? (ffirst times-timeline))
                                             times-timeline
                                             (into [[0 initial-bar-len initial-beat-len]]
                                                   times-timeline))))
               ly-golbal-str (loop [event (sort-by #(+ (first %) (* 0.1 (second %))) < body)
                                    time-changes bb-times-mapping
                                    acc-str ""
                                    cur-bar-len initial-bar-len
                                    cur-beat-len initial-beat-len
                                    last-bar 1 ;; test equality so no unneccecary s advancement
                                    last-beat 1]
                               (if (empty? event)
                                 acc-str
                                 (let [cur-event (first event)]
                                   (if-let [ly-str (first (last cur-event))]
                                     (let [next-time-change (first time-changes)
                                           time-change-event? (and (contains? (second (last cur-event)) :time-signature)
                                                                   (== (first cur-event) (ffirst time-changes)))
                                           ly-spaceing (if (and (== last-bar (first cur-event))
                                                                (== last-beat (second cur-event)))
                                                         " "
                                                         (format " s%s*%s " (int (/ 1 cur-beat-len))
                                                                 (int (+ (- (second cur-event)
                                                                            last-beat)
                                                                         (* cur-bar-len
                                                                            (- (first cur-event)
                                                                               last-bar))))))
                                           acc-str (str acc-str ly-spaceing ly-str)
                                           [cur-bar-len cur-beat-len] (if time-change-event?
                                                                        (last (first time-changes))
                                                                        [cur-bar-len cur-beat-len])
                                           ;; cur-beat-len (if time-change-event?
                                           ;;                (second (first time-changes))
                                           ;;                cur-beat-len)
                                           time-changes (if time-change-event?
                                                          (rest time-changes)
                                                          time-changes)]
                                       (recur (rest event)
                                              time-changes
                                              acc-str
                                              cur-bar-len
                                              cur-beat-len
                                              (first cur-event)
                                              (second cur-event)))
                                     (recur (rest event)
                                            time-changes
                                            acc-str
                                            cur-bar-len
                                            cur-beat-len
                                            last-bar
                                            last-bar)))))
               initial-global-map {:tempo-timeline tempo-timeline :bpm 90
                                   :time-signature-timeline time-signature-timeline
                                   :bar-len initial-bar-len :beat-len initial-beat-len
                                   ;; Note to self: bar and beat start at 1
                                   :bar 1 :beat 1 :time-seconds 0}
               ;;(reduce #(conj %1 %2) [] time-signatures)
               ]
           ;; (prn bb-times-mapping)
           ;; (prn time-signature-timeline)
           ;; initial-global-map
           [(format "\nglobal = {\\numericTimeSignature \n %s \n}" ly-golbal-str)
            (fn [linear-time]
              (apply get-data-from-global initial-global-map (linear-time-to-index linear-time)))])))



;; (time (get-data-from-global 

;; (get-data-from-global 5)

;; (nth (cons [666] (iterate inc 1)) 1)

(defn render-debug [score-name]
  (((keyword score-name) @ly-scores)))

(defn render! [score-name]
  (let [start-time (. System (nanoTime))
        compiled-v (((keyword score-name) @ly-scores))]
    (spit (str "out/" score-name ".ly") (first compiled-v))
    (spit (str "out/" score-name ".sco") (second compiled-v))
    (ly-compile-from-string
     (str score-name)
     (first compiled-v)
     start-time)))


(defpart 'violin
  {:p4 [:amp -8] :p5 [:freq 440]}
  (n :c3 :dur 1/4)
  (n :d3)
  (n :e3 )
  (n :f3)
  (n :g3)
  (n :a3 :dur 1/2)
  (n :h3))

(defpart 'viola
  {:p4 [:amp -8] :p5 [:freq 440]}
  (n :c3 :dur 1/8)
  (n :d3)
  (n :e3)
  (n :f3))

(defglobal 'partitur
  ;; (do-at 1 (global-key-signature :d :minor))
  (do-at 1 (time-signature 1 4))
  (do-at 2 (time-signature 2 4))
  (do-at 3 (time-signature 7 4))
  (do-at 4 (time-signature 3 4))
  (do-at 5 (time-signature 1 4))
  )

(defscore 'partitur
  {:title "prufa1"
   :composer "yomama"}
  'violin
  ;;'viola
  )

;; (render! 'partitur)


;; (sort-by #(+ (first %) (* 0.1 (second %))) < '((99 1) (99 3) (99 0) (0 10) (0 1) (0 0)))
