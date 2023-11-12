(ns cedict-parser
  (:require [clojure.string :as str]))

(defn get-hanzi 
  [string script]
  ((case script
     :traditional second
     :simplified  last)
   (seq (re-find #"^([^ ]+) ([^ ]+)" string))))

(defn get-pinyin [line]
  (second (re-find #"\[(.*)\] \/" line))) ; Get second capture group (.*)

(defn get-meanings [line]
  (seq (map second (re-seq #"\/([^\/]+)" line))))

(defn get-tones [line]
  (seq (map last (str/split (get-pinyin line) #" "))))

(defn debug-line [line]
  (println (str "Line:       " line))
  (println (str "Simplified: " (get-hanzi line :simplified)))
  (println (str "Pinyin:     " (get-pinyin line)))
  (println (str "Tones:      " (get-tones line)))
  (println (str "Meaning(s): " (str (get-meanings line)))))
