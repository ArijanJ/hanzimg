(ns image
  (:import [java.awt Color Font BasicStroke RenderingHints]
           [java.awt.image BufferedImage]
           [javax.imageio ImageIO]
           [java.io File])
  (:require [cedict-parser :as zh]))

(def half #(/ % 2))

(defn create-graphics-for-image [image]
  (let [hints (RenderingHints. RenderingHints/KEY_TEXT_ANTIALIASING 
                               RenderingHints/VALUE_TEXT_ANTIALIAS_ON)
        gfx (.createGraphics image)]
    (.setRenderingHints gfx hints)
    gfx))

(defn image-graphics-pair
  ([file]
   (let [image (ImageIO/read file)]
     [image (create-graphics-for-image image)]))
  ([width height]
   (let [image (BufferedImage. width height BufferedImage/TYPE_INT_RGB)]
     [image (create-graphics-for-image image)])))
     

(defn make-text 
  [ & {:keys [text font size color] 
       :or {text "nil", font "Monospaced", size 24, color Color/WHITE}}]
  {:text text, :font font, :size size, :color color})

(defn draw-text 
  [gfx x y {:keys [text font size color]}]
  (let [text (str text)]
    (.setFont gfx (Font. font Font/PLAIN size))
    ; Outline
    (.setColor gfx Color/BLACK)
    (.setStroke gfx (BasicStroke. 1))
    (doseq [n (range 0 8)
            m (range 0 8)]
      (let [sides [[(+ x n) (+ y m)]
                   [(- x n) (+ y m)]
                   [(+ x n) (- y m)]
                   [(- x n) (- y m)]]]
        (doseq [[x y] sides]
          (.drawString gfx (str text) x y))))
    ; Text
    (.setColor gfx color)
    (.drawString gfx (str text) x y)))

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

(defn render-line [line image+gfx]
  (let [width  1920
        height 1080, vertical-middle (half height)
        sizes {:characters 256
               :pinyin 64
               :meaning 32}]

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
