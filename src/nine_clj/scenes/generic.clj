(ns nine-clj.scenes.generic
  [:require
    [nine-clj.datum :as dat]
    [nine-clj.graph :as graph]
    [nine-clj.geom :as geom]
    [nine-clj.gui :as gui]
    [nine-clj.input :as input]
    [nine-clj.phys :as phys]
    [nine-clj.prof :as prof]
    [nine-clj.math :as math]
    [nine-clj.scripting :as scripting]
    [nine-clj.mac :refer [-->]]
  ]
)

(defn generic-setup [dev res loop render-loop pause-menu level]
  (let
    [
      { :keys [gui-asset] } res
      { :keys [presets models pos rot shapes spawn update-state update-phys next-state] } level
      phys-world (phys/dynamics-world)
      scene-body (mapv #(phys/add-rigid-body phys-world (% :shape) (% :pos) (% :rot) 0) shapes)

      players (spawn phys-world presets)
      player (first players)
      non-players (rest players)
      [campos camrot] (dat/player-cam player)
    ]
    {
      :phys-world phys-world
      :player player
      :non-players non-players
      :items ()
      :scene { :models models :pos pos :rot rot }
      :gui-asset gui-asset
      :campos campos
      :camrot camrot
      :movement [0 0 0]
      :time 0
      :loop loop
      :render-loop render-loop
      :update-state update-state
      :update-phys update-phys
      :next-state next-state
      :pause-menu pause-menu
    }
  )
)

(defn generic-render-loop [dev res state]
  (let [
      { :keys [
          player
          non-players
          items
          scene
          campos
          camrot
          gui-asset
          time
        ]
      } state
      { :keys [ width height ] } dev
      fov (get state :fov 60)
    ]
    (prof/profile :rendering (do
      (doto (dev :gl)
        (.clearDepth)
        (.clearColor 0.5 0.5 0.7 0)
      )
      (graph/world-light [0 -1 0])

      (graph/projection (math/perspective (width) (height) (math/radians fov) 0.3 20000))
      (graph/camera (math/first-person-camera campos camrot))

      (graph/push-matrix)
      (apply graph/translate (scene :pos))
      (apply graph/rotate (scene :rot))
      (doseq [m (scene :models)] (graph/model m))
      (graph/pop-matrix)

      (doseq [n (concat [player] non-players items)] (dat/render-char n time))
    ))
  )
)

(defn generic-loop [dev res state]
  (let [
      { :keys [get-time width height] } dev
      { :keys [
          next-state
          render-loop
          update-state
          update-phys
        ]
      } state
      real-time (get-time)
      last-real-time (get state :last-real-time real-time)
      dt (- real-time last-real-time)
      pdt (min dt 1/10)
      time (state :time)
      time (+ time pdt)
      state (do
        (prof/profile :update-phys (update-phys (state :phys-world) pdt))
        (prof/profile :update-state (update-state dev state))
        (assoc state :time time :last-real-time real-time :delta-time pdt)
      )
      state (prof/profile :next-state (next-state dev res state))
    ]
    (render-loop dev res state)
    (cond (-> dev :keyboard input/escape-up) ((state :pause-menu) dev res state) :else state)
  )
)

(defn render-location [dev res location]
  (graph/push-matrix)
  (apply graph/translate (location :pos))
  (apply graph/rotate (location :rot))
  (apply graph/scale (location :scale))
  (->> location :models
    (map #(graph/replace-materials (dev :gl) % { "Flag-material" (location :color) }))
    (map graph/model)
    dorun
  )
  (graph/pop-matrix)
)
