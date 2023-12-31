(ns main
  (:require [image]
            [clojure.java.io :as io]))

(defn random-line []
  (with-open [r (io/reader "/path/to/cedict.txt")]
    (rand-nth (line-seq r))))

(defn -main []
  (let [input (io/file "input.png")]
    (-> (random-line)
        (image/render-line
          (apply image/image-graphics-pair
                 (if (.exists input)
                   [input]
                   [1920 1080])))
        (image/save-png "output"))))
