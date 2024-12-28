(require
  '[nine-clj.datum :as dat]
  '[nine-clj.math :as math]
  '[nine-clj.phys :as phys]
  '[nine-clj.scenes.arena :as arena]
  '[nine-clj.graph :as graph]
  '[nine-clj.nav :as nav]
)
(fn [dev world-markers all-presets]
  (let [
      location (fn [& args]
        (let [
            h (apply hash-map args)
            { :keys [id name pos rot scale color side army recruits spawn] } h
            mat (math/transform pos rot scale)
            preset (all-presets name)
            entry (->> (get h :entry "entry"))
            h (assoc h :mat mat :preset preset)
          ]
          (assoc h
            :mat mat
            :preset preset
            :models (preset :models)
            :shapes (->> preset :shapes (mapv #(assoc % :pos pos :rot rot)))
            :entry-pos (-> h (nav/marker entry) nav/marker-pos)
            :entry-look (-> h (nav/marker entry) nav/marker-look)
          )
        )
      )
      guard-spawn (fn [info loc spawn-fn]
        (let [
            { :keys [color side] } loc
            ps (partition 2 info)
            posf #(->> % (nav/marker loc) nav/marker-pos)
            lookf #(->> % (nav/marker loc) nav/marker-look)
            ps (mapv (fn [[kind mark]] (vector kind color side
              (posf mark)
              (lookf mark)
              dat/passive-ai-next
              dat/passive-ai-in)) ps)
          ]
          (mapv #(apply spawn-fn %) ps)
        )
      )
      crowd-group 64
      crowd-mask (bit-not 64)
      crowd-spawn (fn [loc spawn-fn]
        (let [
            { :keys [color side recruits] } loc
            pts (nav/location-nav loc)
            spots (nav/location-spots loc)
            [lx lz] (-> Math/PI (* 2) rand math/clock-xy)
          ]
          (->> pts
            cycle
            (map #(spawn-fn %1 color side %2 [lx 0 lz]
                (partial dat/crowd-ai-next pts spots)
                dat/crowd-ai-in
              )
              recruits
            )
            (mapv #(doto %
              (-> :body (phys/set-group (% :world) crowd-group crowd-mask))
            ))
          )
        )
      )
      all-spawn (fn [info loc spawn-fn]
        (concat
          (crowd-spawn loc spawn-fn)
          (guard-spawn info loc spawn-fn)
        )
      )
    ]
    (->> {
      :castle-red (location
        :name :castle
        :side :red
        :color [1 0 0 1]
        :pos (-> "castle_red" world-markers nav/marker-pos)
        :rot [0 (-> "castle_red" world-markers nav/marker-rot-y) 0]
        :scale [1 1 1]
        :spawn (partial all-spawn [
          :fighter "guard_0"
          :fighter "guard_1"
          :archer "guard_2"
          :archer "guard_3"
          :fighter "guard_4"
          :fighter "guard_5"
          :fighter "guard_6"
          :archer "guard_archer_1"
        ])
        :army (concat
          (repeat 5 :fighter)
          (repeat 10 :archer)
        )
        :recruits (concat
          (repeat 5 :fighter)
          (repeat 5 :archer)
        )
      )
      :castle-blue (location
        :name :castle
        :side :blue
        :color [0 0 1 1]
        :pos (-> "castle_blue" world-markers nav/marker-pos)
        :rot [0 (-> "castle_blue" world-markers nav/marker-rot-y) 0]
        :scale [1 1 1]
        :spawn (partial all-spawn [
          :archer "guard_0"
          :archer "guard_1"
          :archer "guard_2"
          :archer "guard_3"
          :archer "guard_4"
          :archer "guard_5"
          :archer "guard_6"
          :archer "guard_archer_1"
        ])
        :army (repeat 30 :archer)
        :recruits (repeat 10 :archer)
      )
      :castle-sand (location
        :name :castle-desert
        :side :blue
        :color [219/255 154/255 89/255 1]
        :pos (-> "castle_sand" world-markers nav/marker-pos)
        :rot [0 (-> "castle_sand" world-markers nav/marker-rot-y) 0]
        :scale [1 1 1]
        :spawn (partial all-spawn [
          :ninja "guard_0"
          :ninja "guard_1"
          :ninja "guard_2"
          :ninja "guard_3"
          :ninja "guard_4"
          :ninja "guard_5"
          :ninja "guard_6"
          :ninja "guard_archer_1"
        ])
        :army (repeat 30 :ninja)
        :recruits (repeat 10 :ninja)
      )
      :tower-ruby (location
        :name :mage-tower
        :side :ruby
        :color [224/255 17/255 95/255 1]
        :pos (-> "tower_ruby" world-markers nav/marker-pos)
        :rot [0 (-> "tower_ruby" world-markers nav/marker-rot-y) 0]
        :scale [1 1 1]
        :spawn (partial all-spawn [
          :mage "guard_0"
          :mage "guard_1"
          :mage "guard_2"
        ])
        :army (repeat 30 :mage)
        :recruits (repeat 10 :mage)
      )
      :tower-sapp (location
        :name :mage-tower
        :side :sapp
        :color [15/255 182/255 86/255 1]
        :pos (-> "tower_sapp" world-markers nav/marker-pos)
        :rot [0 (-> "tower_sapp" world-markers nav/marker-rot-y) 0]
        :scale [1 1 1]
        :spawn (partial all-spawn [
          :mage "guard_0"
          :mage "guard_1"
          :mage "guard_2"
        ])
        :army (repeat 30 :mage)
        :recruits (repeat 10 :mage)
      )
      :tower-emer (location
        :name :mage-tower
        :side :emer
        :color [0/255 156/255 74/255 1]
        :pos (-> "tower_emer" world-markers nav/marker-pos)
        :rot [0 (-> "tower_emer" world-markers nav/marker-rot-y) 0]
        :scale [1 1 1]
        :spawn (partial all-spawn [
          :mage "guard_0"
          :mage "guard_1"
          :mage "guard_2"
        ])
        :army (repeat 30 :mage)
        :recruits (repeat 10 :mage)
      )
      :tower-ameth (location
        :name :mage-tower
        :side :emer
        :color [153/255 102/255 204/255 1]
        :pos (-> "tower_ameth" world-markers nav/marker-pos)
        :rot [0 (-> "tower_ameth" world-markers nav/marker-rot-y) 0]
        :scale [1 1 1]
        :spawn (partial all-spawn [
          :mage "guard_0"
          :mage "guard_1"
          :mage "guard_2"
        ])
        :army (repeat 30 :mage)
        :recruits (repeat 10 :mage)
      )
    }
    (map (fn [[id l]] [id (assoc l :id id)]))
    (into {}))
  )
)
