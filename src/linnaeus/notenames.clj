(ns linnaeus.notenames)

(defn midi->freq
  "Convert MIDI Note number to frequency in hertz
   from Steven Yi's score"
  [notenum]
  (* 440 (Math/pow 2.0 (/ (- notenum 69) 12))))


(defn- generate-language [chromatic-map]
  (loop [initial true
         octave -2
         notemap {}]
    (if (= octave 9)
      notemap
      (recur (if initial false initial)
             (inc octave)
             (merge notemap
                    (reduce-kv
                     (fn [init k v]
                       (merge init (reduce #(assoc %1 (keyword (str (name %2) (if initial
                                                                                "" octave)))
                                                   {:midi (+ k (* (+ 2 octave) 12))
                                                    :freq (midi->freq (+ k (* (+ 2 octave) 12)))
                                                    :ly (str (name %2)
                                                             (cond
                                                               (< octave 2) (apply str (repeat (+ 2 (Math/abs octave)) ","))
                                                               (= octave 2) ""
                                                               (> octave 2) (apply str (repeat (- octave 2) "'"))))})
                                           {} v)))
                     notemap chromatic-map))))))

(def deutsch
  (generate-language
   {0 [:c :his :deses]
    1 [:cis :des :hisis]
    2 [:d :cisis :eses]
    3 [:dis :es :feses]
    4 [:e :disis :fes]
    5 [:f :eis :geses]
    6 [:fis :ges :eisis]
    7 [:g :fisis :ases]
    8 [:gis :as]
    9 [:a :gisis :heses]
    10 [:ais :b :ceses]
    11 [:h :aisis :ces]}))



