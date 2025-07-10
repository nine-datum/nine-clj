(import
  (nine.buffer
    Buffer
  )
  (java.awt.font
    FontRenderContext
    TextLayout
  )
  (java.awt
    Font
    Color
    RenderingHints
  )
  (java.awt.geom
    AffineTransform
  )
  (java.awt.image
    BufferedImage
  )
  (java.io
    File
  )
  (javax.imageio
    ImageIO
  )
)

(defn font
  ([name size] (Font. name (. Font PLAIN) size))
  ([name size style] (Font. name style size))
)

(defn load-font [file size] (.deriveFont (. Font createFont Font/PLAIN (File. file)) Font/PLAIN (float size)))

(defn text-image [font]
  (let
    [
      frc (FontRenderContext. (AffineTransform.)
        (. RenderingHints VALUE_TEXT_ANTIALIAS_ON)
        (. RenderingHints VALUE_FRACTIONALMETRICS_OFF)
      )
      fs (.getSize font)
      img (BufferedImage. (* 16 fs) (* 16 fs) (. BufferedImage TYPE_INT_ARGB))
      g (.createGraphics img)
      char-nums (mapv
        (fn [i]
          (if (<= 192 i 256) (-> i (- 192) (+ 1040)) i) ; rendering cyrillic after 192
        )
        (range 256)
      )
      chars (mapv (comp str char) char-nums)
      image-rects (mapv
        (fn [s]
          (let [
              tl (TextLayout. s font frc)
              b (.getBounds tl)
              px (.getMinX b)
              py (.getMinY b)
              pw (.getWidth b)
              ph (.getHeight b)
            ]
            [px py pw ph]
          )
        )
        chars
      )
      char-rects (mapv #(mapv / % (repeat fs)) image-rects)
    ]
    (.setColor g (. Color WHITE))
    (.setFont g font)
    (doseq [i (range 256)]
      (let [
          n (char-nums i)
          s (chars i)
          [px py pw ph] (image-rects i)
          sx 1
          sy 1
          x (* fs (rem i 16))
          y (* fs (quot i 16))
          t (.getTransform g)
        ]
        (.translate g x y)
        (.translate g (- px) (- py))
        (.scale g sx sy)
        (.drawString g s 0 0)
        (.setTransform g t)
      )
    )
    (.dispose g)
    { :img img :rects char-rects }
  )
)

(defn save-text-png [img file]
  (javax.imageio.ImageIO/write img "png" (java.io.File. file))
)

(defn save-text [font]
  (let [
      file (str "res/fonts/" (.getName font) "-" (.getSize font))
      img-file (str file ".png")
      rects-file (str file "-rects" ".txt")
    ]
    (doto (text-image font)
      (-> :img (save-text-png img-file))
      (->> :rects str (spit rects-file))
    )
  )
)

(save-text (load-font "res/fonts/ubuntu.ttf" 50))
