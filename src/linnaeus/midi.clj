(ns linnaeus.midi
  (:require [clojure.java.io :as io])
  (:import [javax.sound.midi]
           [javax.sound.midi Sequence MidiSystem ShortMessage MidiEvent]))


(use 'clojure.reflect)

(defn all-methods [x]
  (->> x reflect 
       :members 
       (filter :return-type)  
       (map :name) 
       sort 
       (map #(str "." %) )
       distinct
       println))


(defn create-midi-track [sequence]
  (let [enable-poly (doto (new ShortMessage)
                      (.setMessage 176 127 0))
        enable-poly-event (new MidiEvent enable-poly 0)]
    (doto (.createTrack sequence)
      (.add enable-poly-event))))

(defn create-midi-note-event [])

(defn generate-midi-file [filename {:keys [track1 track2] :as tracks}]
  (let [sequence (new Sequence Sequence/PPQ 256)
        track (create-midi-track sequence)
        midi-file (io/file (str "out/" filename ".midi"))]
    ;; (all-methods track)
    ;; (MidiSystem/write sequence 1 midi-file)
    ;; (.getTempoInBPM sequence)
    ))

;; Data needed: [[abs-time midi-note velocity] ..]
;; sort -> loop and subtract 

;; (generate-midi-file "henda" {:track1 666})
