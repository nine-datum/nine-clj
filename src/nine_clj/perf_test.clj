(ns nine-clj.perf-test
  [:require
    [nine-clj.graph :as graph]
    [nine-clj.math :as math]
    [nine-clj.core :as core]
  ]
)

(declare loop)

(defn setup [dev res]
  {
    :model (-> res :arena :presets :archer :model)
    :anim (-> res :arena :presets :archer :anims (get "idle_pass"))
    :loop loop
  }
)

(defn loop [dev res state]
  (let [
      { :keys [model anim] } state
      { :keys [get-time width height gl] } dev
      [anim obj-anim] (mapv :anim anim)
      time (get-time)
    ]
    (doto (dev :gl)
      .clearDepth
      (.clearColor 1/2 1/2 1/2 1)
    )
    (graph/projection (math/perspective (width) (height) (* Math/PI 1/3) 1 1000))
    (graph/world-light [0 -1 0])
    (mapv
      (fn [i]
        (graph/push-matrix)
        (graph/translate (- 5 (mod i 10)) -2 (+ 5 (quot i 10)))
        (graph/rotate 0 Math/PI 0)
        (graph/animated-model model (graph/animate anim time) (graph/animate obj-anim time))
        (graph/pop-matrix)
      )
      (range 1000)
    )
    state
  )
)

(defn run []
  (core/window 800 600 setup nil)
)
