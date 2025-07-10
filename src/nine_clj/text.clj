(ns nine-clj.text
  (:import
    [nine.buffer
      Buffer
    ]
  )
)

(defn load-text [font-name]
  (let [
      file (str "res/fonts/" font-name)
      img-file (str file ".png")
      rects-file (str file "-rects" ".txt")
    ]
    { :img img-file :rects (-> rects-file slurp read-string) }
  )
)

(defn table-index [sym]
  (let [
      i (int sym)
    ]
    (if (<= 1040 i 1104) (-> i (- 1040) (+ 192)) i) ; masking cyrillic
  )
)

(defn text-geom [gl target-size rects text]
  (let [
      rects (mapv (comp rects table-index) text)
      sizes (mapv (comp #(conj % 1) vec (partial drop 2)) rects)
      sizes (mapv (fn [s c] (if (= c \space) [0.5 1 1] s)) sizes text)
      offsets (mapv (comp #(conj % 0) vec (partial take 2)) rects)
      offsets (mapv (fn [[x y z] [sx sy sz]] [x (- 0 y sy) z]) offsets sizes)
      steps (reduce
        (fn [sum [c [px py pz]]]
          (conj sum
            (case c
              \newline (vector 0 (-> sum last second dec) 0)
              (->> sum last (mapv + [px 0 0]))
            )
          )
        )
        [[0 0 0]]
        (map vector text sizes)
      )
      l (.length text)
      r (* 6 l)
      bvs [
        [0 0 0]
        [1 0 0]
        [1 1 0]

        [1 1 0]
        [0 1 0]
        [0 0 0]
      ]
      buv [
        [0 0]
        [1 0]
        [1 1]

        [1 1]
        [0 1]
        [0 0]
      ]
      uv (fn [i]
        (let [
            s (table-index (nth text i))
            x (/ (rem s 16) 16)
            y (/ (quot s 16) 16)
            [sx sy] (sizes i)
            y (- 1 (* sy 1/16) y)
          ]
          (mapv #(mapv + (map * % [sx sy] (repeat 1/16)) [x y]) buv)
        )
      )
      [tsx tsy] target-size
      lines (->> text (filter (partial = \newline)) count inc)
      text-width-mul tsy
      text-size-mul (/ lines)
      [sizes offsets steps] (map
        (partial mapv #(mapv * %1 %2) (repeat [(* text-width-mul text-size-mul) text-size-mul 1]))
        [sizes offsets steps]
      )
      bounds-top-left (->> steps first (mapv + (->> sizes first second (vector 0))))
      bounds-bottom-right [(->> steps (map first) (apply max)) (->> steps (map second) (apply min))]
      bounds-min (mapv min bounds-top-left bounds-bottom-right)
      bounds-max (mapv max bounds-top-left bounds-bottom-right)
      bounds-size (mapv - bounds-max bounds-min)
      [shift-x shift-y] (->>
        (mapv - [1 1] bounds-size)
        (mapv (partial * 0.5))
      )
      bx (fn [i]
        (mapv #(
            ->> %
            (mapv * (sizes i))
            (mapv + (offsets i) (steps i) [shift-x shift-y 0])
          ) bvs
        )
      )
      nfs (constantly (repeat 6 [0 0 1]))
      discard-ws (fn [f] (fn [i] (cond (= \space (-> text (nth i))) [] :else (f i))))
      [bx uv nfs] (map discard-ws [bx uv nfs])
      vs (apply concat (map bx (range l)))
      uvs (apply concat (map uv (range l)))
      nrs (apply concat (map nfs (range l)))
      to-float (partial map float)
      buf (comp vec to-float (partial apply concat))
      geom (-> gl
        (.vao (. Buffer range r))
        (.attribute 3 (. Buffer of (buf vs)))
        (.attribute 2 (. Buffer of (buf uvs)))
        (.attribute 3 (. Buffer of (buf nrs)))
        (.drawing)
      )
    ]
    geom
  )
)
