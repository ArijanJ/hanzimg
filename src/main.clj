(ns main
  (:require [image]
            [clojure.java.io :as io]))

(defn random-line []
  (with-open [r (io/reader "/path/to/cedict.txt")]
    (rand-nth (line-seq r))))

(defn -main []
  (-> (main/random-line)
      image/render-line
      (image/save-png "output")))
