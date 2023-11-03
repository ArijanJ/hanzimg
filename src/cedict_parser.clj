(ns cedict-parser)

(defn get-hanzi 
  [string script]
  ((case script
     :traditional second
     :simplified  last)
   (re-find #"^([^ ]+) ([^ ]+)" string)))

(defn get-pinyin [line]
  (second (re-find #"\[(.*)\] \/" line))) ; Get second capture group (.*)

(defn get-meanings [line]
  (into [] (map second (re-seq #"\/([^\/]+)" line))))

(defn get-tones [line]
  (into [] (map last (clojure.string/split (get-pinyin line) #" "))))

(defn debug-line [line]
  (println (str "Line:       " line))
  (println (str "Simplified: " (get-hanzi line :simplified)))
  (println (str "Pinyin:     " (get-pinyin line)))
  (println (str "Tones:      " (get-tones line)))
  (println (str "Meaning(s): " (str (get-meanings line)))))
