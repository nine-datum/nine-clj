(ns nine-clj.scenes.location
  [:require
    [nine-clj.scenes.generic :as generic]
    [nine-clj.datum :as dat]
    [nine-clj.phys :as phys]
    [nine-clj.graph :as graph]
    [nine-clj.math :as math]
  ]
)

(declare location-render-loop)

(defn location-setup [dev res player location locations world-state-setup]
  (let [
      { :keys [preset pos look color side] } player
      spawn (location :spawn)
      loc-entry-pos (location :entry-pos)
      loc-entry-look (location :entry-look)
      pause-menu (res :location-pause-menu-setup)
      make-char (fn [phys-world preset pos look color side ai-next ai-in] (dat/load-char phys-world preset pos look color side :idle-pass ai-next ai-in 0))
      level (assoc (select-keys location [:pos :rot :shapes])
        :presets (-> res :arena :presets)
        :update-state dat/update-game-state
        :update-phys phys/update-world
        :next-state dat/next-game-state
        :spawn (fn [phys-world presets]
          (->
            (make-char phys-world preset loc-entry-pos loc-entry-look color side
              dat/passive-ai-next
              dat/passive-ai-in
            )
            (cons (spawn location (fn [kind color side pos look ai-next ai-in] (make-char phys-world (presets kind) pos look color side ai-next ai-in))))
          )
        )
      )
    ]
    (assoc (generic/generic-setup dev res generic/generic-loop location-render-loop pause-menu level)
      :world-state-setup world-state-setup
      :locations locations
    )
  )
)

(defn location-render-loop [dev res state]
  (generic/generic-render-loop dev res state)
  (->> res :world :models (map graph/model) dorun)
  (->> state :locations vals (map #(generic/render-location dev res %)) dorun)
)
