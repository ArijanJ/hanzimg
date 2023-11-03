(ns image
  (:import [java.awt Color Font]
           [java.awt.image BufferedImage]
           [javax.imageio ImageIO]
           [java.io File])
  (:require [cedict-parser :as zh]))

(def half #(/ % 2))

(defn make-image [width height]
  (BufferedImage. width height BufferedImage/TYPE_INT_RGB))

(defn make-text 
  ([text font size color]
   {:text text, :font font, :size size, :color color})
  ([text size]
   (make-text text size Color/WHITE))
  ([text size color]
   (make-text text "Monospaced" size color)))

(defn build-font [name size]
  (Font. name Font/PLAIN size))

(defn draw-text 
  [gfx x y {:keys [text font size color]}]
  (.setColor   gfx color)
  (.setFont    gfx (build-font font size))
  (.drawString gfx (str text) x y))

(defn hanzi-with-tones [line]
  (let [hanzi (seq (zh/get-hanzi line :simplified))
        tones (zh/get-tones line)]
    (map vector hanzi tones)))

(defn offset-word-position
  "Offsets a position on the screen by [n-chars] to the left of the center"
  [width n-chars char-width]
  (let [text-width (* n-chars char-width)]
    (-> (- width text-width)
        (/ 2))))

(defn tone->color [tone]
  (get {\1 Color/RED, 
        \2 Color/YELLOW, 
        \3 Color/GREEN, 
        \4 Color/BLUE, 
        \5 Color/WHITE}
       tone))

(defn render-line [line]
  (let [width  1920
        height 1080, vertical-middle (half height)
        character-size 256
        pinyin-size 64
        meaning-size 32
        image (make-image 1920 1080)
        gfx (.createGraphics image)]

    ; Render hanzi
    (loop [all-hanzi (hanzi-with-tones line), 
           [zi tone] (first all-hanzi),
           x (offset-word-position width (count all-hanzi) character-size)]
      (when (seq all-hanzi) 
        (draw-text
          gfx x vertical-middle
          (make-text zi character-size (tone->color tone)))
        (let [other-hanzi (rest all-hanzi)]
          (recur other-hanzi (first other-hanzi) (+ x character-size)))))

    ; Render pinyin
    (let [pinyin (zh/get-pinyin line)
          x (offset-word-position width (count pinyin) (- pinyin-size 26))]
      (draw-text 
        gfx x (+ vertical-middle (half character-size))
        (make-text pinyin pinyin-size)))

    ; Render meanings
    (loop [meanings (zh/get-meanings line)
           y (+ (half character-size) pinyin-size vertical-middle)]
      (when (seq meanings) ; While meanings still left
        (let [meaning (first meanings)
              x (offset-word-position width (count meaning) (- meaning-size 14))]
          (draw-text 
            gfx x y
            (make-text meaning meaning-size))
          (recur (rest meanings)
                 (+ y meaning-size)))))
    image))

(defn save-png [image filename]
  (ImageIO/write image "png" (File. (str "./" filename ".png"))))
