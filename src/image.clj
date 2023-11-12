(ns image
  (:import [java.awt Color Font]
           [java.awt.image BufferedImage]
           [javax.imageio ImageIO]
           [java.io File])
  (:require [cedict-parser :as zh]))

(def half #(/ % 2))

(defn image-graphics-pair [width height]
  (let [image (BufferedImage. width height BufferedImage/TYPE_INT_RGB)]
    [image (.createGraphics image)]))

(defn make-text 
  [ & {:keys [text font size color] 
       :or {text "nil", font "Monospaced", size 24, color Color/WHITE}}]
  {:text text, :font font, :size size, :color color})

(defn draw-text 
  [gfx x y {:keys [text font size color]}]
  (doto gfx
    (.setColor color)
    (.setFont (Font. font Font/PLAIN size))
    (.drawString (str text) x y)))

(defn hanzi-with-tones [line]
  (map vector 
       (zh/get-hanzi line :simplified)
       (zh/get-tones line)))

(defn offset-word-position
  "Offsets a position on the screen by [n-chars] to the left of the center"
  [width n-chars char-width]
  (let [text-width (* n-chars char-width)]
    (-> (- width text-width)
        (/ 2))))

(defn tone->color [tone]
  ({\1 Color/RED, \2 Color/YELLOW, \3 Color/GREEN, \4 Color/BLUE, \5 Color/WHITE}
   tone))

(defn render-line [line]
  (let [width  1920
        height 1080, vertical-middle (half height)
        sizes {:characters 256
               :pinyin 64
               :meaning 32}
        image+gfx (image-graphics-pair 1920 1080)]

    (loop [all-hanzi (hanzi-with-tones line), 
           [zi tone] (first all-hanzi),
           x (offset-word-position width (count all-hanzi) (sizes :characters))]
      (when (seq all-hanzi) 
        (draw-text
          (second image+gfx) x vertical-middle
          (make-text :text zi :size (sizes :characters) :color (tone->color tone)))
        (let [other-hanzi (rest all-hanzi)]
          (recur other-hanzi (first other-hanzi) (+ x (sizes :characters))))))

    (let [pinyin (zh/get-pinyin line)
          x (offset-word-position width (count pinyin) (- (sizes :pinyin) 26))]
      (draw-text 
        (second image+gfx) x (+ vertical-middle (half (sizes :characters)))
        (make-text :text pinyin :size (sizes :pinyin))))

    (loop [meanings (zh/get-meanings line)
           y (+ (half (sizes :characters)) (sizes :pinyin) vertical-middle)]
      (when (seq meanings) ; While meanings still left
        (let [meaning (first meanings)
              x (offset-word-position width (count meaning) (- (sizes :meaning) 14))]
          (draw-text 
            (second image+gfx) x y
            (make-text :text meaning :size (sizes :meaning)))
          (recur (rest meanings)
                 (+ y (sizes :meaning))))))

    (first image+gfx)))

(defn save-png [image filename]
  (ImageIO/write image "png" (new File (str "./" filename ".png"))))
