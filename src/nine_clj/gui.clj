(ns nine-clj.gui
  [:require
    [nine-clj.graph :as graph]
    [nine-clj.text :as text]
    [nine-clj.mac :refer [-->]]
  ]
)

(defn gui-asset [dev]
  (let [
      { :keys [gl storage mouse width height] } dev
      image-shader (graph/load-shader gl storage "res/shaders/image_vertex.glsl" "res/shaders/image_fragment.glsl")
      text-shader image-shader
      text-asset (graph/load-text-asset gl storage "Ubuntu-50")
      button-image (graph/load-image gl storage "res/images/button.png")
      blank-image (graph/load-image-tex gl (.blankTexture gl))
    ]
    {
      :mouse mouse
      :image-shader image-shader
      :text-shader text-shader
      :text-asset text-asset
      :button-image button-image
      :blank-image blank-image
      :width width
      :height height
    }
  )
)

(defn image [asset layout image x y w h color]
  (let [
      [x y w h] (layout asset x y w h)
    ]
    (graph/image image (asset :image-shader) x y w h color)
  )
)

(defn blank [asset layout x y w h color]
  (image asset layout (asset :blank-image) x y w h color)
)

(defn text [asset layout label x y w h color]
  (let [
      [x y w h] (layout asset x y w h)
    ]
    (graph/text (asset :text-asset) (asset :text-shader) label x y w h color)
  )
)

(defn status-bar [asset layout status back-color front-color x y w h]
  (image asset layout (asset :blank-image) x y w h back-color)
  (image asset layout (asset :blank-image) x y (* status w) h front-color)
)

(defn button [asset layout label x y w h]
  (let [
      mouse (asset :mouse)
      [x y w h] (layout asset x y w h)
      [mx my] (mouse :pos)
      [ex ey] (map + [x y] [w h])
      hovered (and (<= x mx ex) (<= y my ey))
      pressed (and hovered (mouse :left-down))
      released (and hovered (mouse :left-up))
      img-color (cond
        pressed [1 1 1 1]
        hovered [0 1 0 1]
        :else [0.5 0.5 0.5 1]
      )
      txt-color (cond
        pressed [1 1 0 1]
        hovered [1 1 1 1]
        :else [0 1 0 1]
      )
    ]
    (graph/image (asset :button-image) (asset :image-shader) x y w h img-color)
    (graph/text (asset :text-asset) (asset :text-shader) label x y w h txt-color)
    released
  )
)

(defn aspect-fit-layout [asset x y w h]
  (let [
      sw (--> asset :width ())
      sh (--> asset :height ())
      m (cond (> sw sh) [(/ sh sw) 1] :else [1 (/ sw sh)])
    ]
    (mapv * [x y w h] (cycle m))
  )
)
