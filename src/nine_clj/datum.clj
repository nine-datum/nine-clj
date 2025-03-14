(ns nine-clj.datum
  [:require
    [nine-clj.graph :as graph]
    [nine-clj.geom :as geom]
    [nine-clj.math :as math]
    [nine-clj.text :as text]
    [nine-clj.phys :as phys]
    [nine-clj.input :as input]
    [nine-clj.prof :as prof]
    [nine-clj.gui :as gui]
    [nine-clj.nav :as nav]
    [nine-clj.mac :refer [-->]]
    [clojure.core.matrix :as mat]
  ]
  [:import
    [nine.geometry.collada
      ColladaBasicSkinParser
      ColladaBasicAnimationParser
      ColladaBasicMaterialParser
    ]
  ]
)

(defn load-item-model [gl storage diffuse-shader skin-shader file]
  (graph/load-model
    (graph/load-graphics gl storage diffuse-shader skin-shader)
    file
  )
)

(defn load-model [gl storage diffuse-shader skin-shader name offset-geom]
  (graph/load-animated-model
    (graph/load-graphics gl storage diffuse-shader skin-shader)
    (format "res/datum/%s.dae" name)
  )
)

(defn load-anim [storage model-name anim-name]
  (let [
      af (format "res/datum/anims/%s/%s.anim" model-name anim-name)
      mf (format "res/datum/%s.dae" model-name)
    ]
    (vector
      (graph/load-anim-clj storage (partial = "JOINT") af mf)
      (graph/load-anim-clj storage (partial = "NODE") af mf)
    )
  )
)

(defn preset [model-name offset-geom-name items materials paint-materials & anims]
  {
    :model-name model-name
    :offset-geom offset-geom-name
    :anims anims
    :items items
    :materials materials
    :paint-materials paint-materials
  }
)

(def char-presets
  {
    :archer (preset "archer" "Cube_001-mesh" [ "res/datum/arrow.dae" ]
      {}
      [ "Odezhda_tsvet-material" ]
      "attack"
      "idle"
      "walk"
      "death"
      "dead"
      "idle_pass"
      "walk_pass"
      "walk_crowd"
      "riding"
      "riding_boat"
    )
    :fighter (preset "fighter" "Cube_002-mesh" []
      {}
      [ "Metall_color-material" ]
      "attack"
      "attack_2"
      "block"
      "idle"
      "walk"
      "death"
      "dead"
      "idle_pass"
      "walk_pass"
      "walk_crowd"
      "riding"
      "riding_boat"
    )
    :mage (preset "mage" "Cube_002-mesh" ["res/datum/fireball.dae"]
      {}
      [ "Odezhda_tsvet-material" ]
      "attackspell"
      "spherespell"
      "teleportspell"
      "idle"
      "walk"
      "death"
      "dead"
      "idle_pass"
      "walk_pass"
      "walk_crowd"
      "riding"
      "riding_boat"
    )
    :ninja (preset "ninja" "Cube_003-mesh" []
      {}
      [ "Odezhda_tsvet-material" ]
      "attack"
      "attack_2"
      "attack_3"
      "idle"
      "walk"
      "death"
      "dead"
      "idle_pass"
      "walk_pass"
      "walk_crowd"
      "riding"
      "riding_boat"
    )
  }
)

(defn stats [walk-speed walk-anim-speed attack-damage health resistances]
  {
    :walk-speed walk-speed
    :walk-anim-speed walk-anim-speed
    :walk-crowd-speed 1.5
    :walk-crowd-anim-speed 1
    :attack-damage attack-damage
    :health health
    :resistances resistances
  }
)

(def char-stats {
    :archer (stats 8 1 50 100 {})
    :fighter (stats 5 0.75 25 150 { :arrow 0.9 :melee 0.5 })
    :ninja (stats 10 1.2 35 80 {})
    :mage (stats 7 1 75 80 {})
  }
)

(defn char-anim-time [ch time]
  (-> ch :state :anim-time (apply [ch time]))
)

(defn get-char-stat [ch stat] (-> ch :name char-stats stat))

(defn get-char-resistance [ch dmg-kind]
  (-> ch :name char-stats :resistances (get dmg-kind 0))
)

(defn read-preset [storage key]
  (let
    [
      { :keys [
          model-name
          offset-geom
          anims
          items
          materials
          paint-materials
        ]
      } (char-presets key)
      loader (fn [gl diffuse-shader skin-shader] (load-model gl storage diffuse-shader skin-shader model-name offset-geom))
      anims (
        (comp
          (partial apply hash-map)
          (partial mapcat vector anims)
          (partial pmap (partial load-anim storage model-name))
        )
        anims
      )
      items-loader (fn [gl diffuse-shader skin-shader]
        (mapv (partial load-item-model gl storage diffuse-shader skin-shader) items)
      )
    ] {
      :name key
      :loader loader
      :items-loader items-loader
      :anims anims
      :materials materials
      :paint-materials paint-materials
    }
  )
)

(defn load-preset-materials [gl preset base-materials main-color]
  (let [
      { :keys [materials paint-materials] } preset
      paint-map (apply hash-map (interleave paint-materials (repeat main-color)))
      mat (graph/material-provider-colors gl (merge materials paint-map))
      mat (graph/material-provider-combine mat paint-materials base-materials)
    ]
    mat
  )
)

(defn load-preset [gl diffuse-shader skin-shader preset]
  {
    :name (preset :name)
    :model ((preset :loader) gl diffuse-shader skin-shader)
    :items ((preset :items-loader) gl diffuse-shader skin-shader)
    :materials-loader (partial load-preset-materials gl preset)
    :anims (preset :anims)
  }
)

(defn load-presets [gl storage diffuse-shader skin-shader]
  (zipmap (keys char-presets) (mapv
    (partial load-preset gl diffuse-shader skin-shader)
    ((comp vec pmap)
      (partial read-preset storage)
      (keys char-presets)
    )
  ))
)

(defn render-preset [preset materials anim time]
  (let [
      { :keys [model anims] } preset
      [skin-anim obj-anim] (mapv :anim (anims anim))
      skin-anim (graph/animate skin-anim time)
      obj-anim (graph/animate obj-anim time)
    ]
    (graph/animated-model model skin-anim obj-anim materials)
  )
)

(defn char-call [ch sym & args]
  (apply (ch sym) (cons ch args))
)

(defn char-list-call [chs sym & args]
  (apply mapv char-call chs (repeat sym) (map repeat args))
)

(defn next-char [ch in time] (char-call ch :next in time))

(defn update-char [ch in time] (char-call ch :update in time))

(defn render-char [ch time] (char-call ch :render time))

; state scheme
; {
;   :update (self in time -> ())
;   :effect (self res in physics time -> [effects])
;   :next (self in effect time -> next-state)
;   :render (self time -> ())
; }

(defn new-state
  ([name anim time next] (new-state name anim time next (constantly ())))
  ([name anim time next update] (new-state name anim time next update (constantly [])))
  ([name anim time next update effect]
    {
      :name name
      :anim anim
      :start time
      :next next
      :update update
      :effect effect
      :anim-time (fn [ch time] time)
    }
  )
)

(defn once-hit-check []
  (let [
      hits (atom (hash-set))
    ]
    (fn [b]
      (let [ c (contains? @hits b) ]
        (swap! hits #(conj % b))
        (false? c)
      )
    )
  )
)

(defn apply-damage [ch kind damage]
  (cond
    (-> ch :state :name (= :block)) ch
    :else (update ch :health #(->> (get-char-resistance ch kind) (- 1) (* damage) (- %)))
  )
)

(defn damage-effect [body hit-check kind side damage]
  [
    (fn [ch] (cond
      (and
        (-> ch :body hit-check)
        (-> ch :side (not= side))
        (-> ch :body (= body))
      ) (apply-damage ch kind damage)
      :else ch
    ))
    identity
  ]
)

(defn spawn-item-effect [item]
  [
    identity
    (fn [s] (update s :items (partial cons item)))
  ]
)

(defn multi-effect [effects] 
  [
    (fn [ch] (reduce #((first %2) %1) ch effects))
    (fn [chs] (reduce #((second %2) %1) chs effects))
  ]
)

(defn remove-item-effect [item]
  [
    (fn [i] (cond (= (i :body) (item :body)) (phys/remove-body (i :world) (i :body))) i)
    (fn [state]
      (->> state
        :items
        (filter (comp (partial not= (item :body)) :body))
        list*
        (assoc state :items)
      )
    )
  ]
)

(defn remove-item-by-id-effect [id]
  [
    identity
    (fn [state]
      (->> state
        :items
        (filter #(-> % :id (= id) not))
        list*
        (assoc state :items)
      )
    )
  ]
)

(defn blood [particles pos rot time]
  {
    :id (java.util.UUID/randomUUID)
    :start time
    :render (fn [item time]
      (graph/play-particles particles pos rot [1 1 1] (-> item :start (- time)))
    )
    :next (fn [item time] item)
    :effect (fn [item res in phys time]
      (cond
        (->> item :start (- time) (< 1)) [(remove-item-by-id-effect (item :id))]
        :else []
      )
    )
  }
)

(defn blood-damage-effect [body hit-check kind side damage blood-particles blood-check pos rot time]
  (cond @blood-check
    (do
      (reset! blood-check false)
      (multi-effect [
        (damage-effect body hit-check kind side damage)
        (spawn-item-effect (blood blood-particles pos rot time))
      ])
    )
    :else
    (damage-effect body hit-check kind side damage)
  )
)

(defn render-item [size item time]
  (graph/push-matrix)
  (graph/apply-matrix (-> :body item phys/get-matrix math/mat4f))
  (apply graph/scale size)
  (graph/model (item :model))
  (graph/pop-matrix)
)

(def projectile-group (short 16))
(def projectile-mask (-> projectile-group bit-not short))

(def arena-size 58)

(def arrow-initial-speed 20)
(def arrow-lifetime 5)
(def arrow-y+ 1.5)
(def arrow-z+ 1)

(defn arrow [time phys-world owner pos rot model]
  {
    :pos pos
    :vel [0 0 0]
    :start time
    :model model
    :world phys-world
    :body (doto
      (phys/capsule phys-world pos (mapv + [(/ Math/PI -2) 0 0] rot) 0.2 1.2 0.1)
      (phys/set-group phys-world projectile-group projectile-mask)
    )
    :render (fn [s time]
      (let [
          { :keys [pos vel] } s
          [vx vy vz] (math/normalize vel)
          rot-y (math/clock vx vz)
          rot-x (-> vy Math/asin - (+ (/ Math/PI 2)))
        ]
        (graph/push-matrix)
        (apply graph/translate pos)
        (graph/rotate rot-x rot-y 0)
        (graph/scale 1/4 1/4 1/4)
        (-> s :model graph/model)
        (graph/pop-matrix)
      )
    )
    :next (fn [s time]
      (assoc s
        :pos (-> s :body phys/get-position)
        :vel (-> s :body phys/get-velocity)
      )
    )
    :hit-check (once-hit-check)
    :effect (fn [item res in phys time]
      (let [
          { :keys [contacts body-to-char] } phys
          c (get contacts (item :body) ())
          dmg (get-char-stat (body-to-char owner) :attack-damage)
          blood-particles (res :blood-particles)
          blood-pos (-> item :body phys/get-position)
          blood-rot [0 0 0]
          side (-> owner body-to-char :side)
        ]
        (cond
          (->> item :start (- time) (< arrow-lifetime)) [ (remove-item-effect item) ]
          (or (= c ()) (= c owner)) []
          (-> body-to-char (contains? c) false?) (do
            (phys/remove-body (item :world) (item :body))
            []
          )
          (-> c body-to-char :state :name (= :block)) []
          :else [ (remove-item-effect item) (blood-damage-effect c (item :hit-check) :arrow side dmg blood-particles (atom true) blood-pos blood-rot time) ]
        )
      )
    )
  }
)

(defn fireball [time phys-world owner pos rot model]
  {
    :model model
    :world phys-world
    :body (doto
      (phys/sphere phys-world pos rot 0.5 1)
      (phys/set-group phys-world projectile-group projectile-mask)
      (phys/set-gravity [0 0 0])
    )
    :next (fn [s time] s)
    :render (partial render-item [1/2 1/2 1/2])
    :effect (fn [item res in phys time]
      (let [
          c (get (phys :contacts) (item :body) ())
          btc (phys :body-to-char)
          owner-char (btc owner)
          dmg (get-char-stat owner-char :attack-damage)
          side (owner-char :side)
        ]
        (cond
          (= c owner) []
          (= c ()) []
          :else [(remove-item-effect item) (damage-effect c (once-hit-check) :fire side dmg)]
        )
      )
    )
  }
)

(defn state-age [s time]
  (- time (s :start))
)

(defn is-alive [ch] (-> ch :health (> 0)))

(defn char-anim [ch name]
  ((ch :anims) name)
)

(defn char-anim-length [s name]
  ((first (char-anim s name)) :length)
)

(defn move-char [ch mvec]
  (phys/move-char (ch :body) mvec)
)

(defn next-char-idle [s ch]
  (let[
      body (ch :body)
      pos (mapv - (phys/get-position body) [0 3/4 0])
    ]
    (assoc ch :pos pos :state s)
  )
)

(defn next-char-mov [s ch in]
  (let [
      look (ch :look)
      in-look (in :look)
      look (cond
        (math/zero-len? in-look) look
        :else (math/normalize-checked in-look)
      )
    ]
    (assoc (next-char-idle s ch) :look look)
  )
)


(defn move-in [m]
  { :movement m :action :none :look m }
)

(defn look-in [l]
  { :movement [0 0 0] :action :none :look l }
)

(defn move-action-in [m a]
  { :movement m :action a :look m }
)

(defn look-action-in [l a]
 { :movement [0 0 0] :action a :look l }
)

(defn real-look-in [ch torq delta-time look]
  (cond
    (-> look mat/length zero?) (-> ch :look look-in)
    :else
    (let [
        dt delta-time
        [cx cy cz] (ch :look)
        [lx ly lz] look
        c-angle (math/clock cx cz)
        l-angle (math/clock lx lz)
        delta (math/angle- l-angle c-angle)
        eps (* Math/PI dt torq)
        m (cond
          (> (Math/abs delta) eps) (* (Math/signum delta) Math/PI torq dt)
          :else delta
        )
        [lx lz] (math/clock-xy (+ c-angle m))
      ]
      (look-in (math/normalize [lx ly lz]))
    )
  )
)

(defn real-move-in [ch torq delta-time mov]
  (let [
      mnorm (math/normalize mov)
      look (->> mnorm (real-look-in ch torq delta-time) :look)
      d (mat/dot mnorm look)
      d (if (> d 0) d 0)
    ]
    ;(assoc (move-in (mapv * (repeat d) mov)) :look look)
    (assoc (->> look (mapv * (repeat (mat/length mov))) (move-in)) :look look)
  )
)

(defn real-move-action-in [ch delta-time mov action]
  (assoc (real-move-in ch delta-time mov) :action action)
)

(defn real-look-action-in [ch delta-time look action]
  (assoc (real-look-in ch delta-time look) :action action)
)

(defn ch-move-in [ch delta-time mov]
  (update (move-in mov) :look #(cond (-> % mat/length zero?) (ch :look) :else %))
)
(defn ch-look-in [ch delta-time look]
  (look-in look)
)
(defn ch-look-action-in [ch delta-time look action]
  (look-action-in look action)
)
(defn ch-move-action-in [ch delta-time mov action]
  (move-action-in mov action)
)

(defn target-dir [ch body-to-char]
  (let [
      { :keys [ pos target ] } ch
      tdir (cond
        (= target ()) [0 0 0]
        :else (mapv - (-> target body-to-char :pos) pos)
      )
    ]
    tdir
  )
)

(defn ai-target [ch chs] ()
  (let [
      { :keys [ side pos ] } ch
      alive (filter is-alive chs)
      enemies (filter (comp (partial not= side) :side) alive)
      grouped (mapv vector (map (comp (partial mapv -) (partial mapv - pos) :pos) enemies) enemies)
      sorted (sort-by (comp mat/length first) grouped)
      [tdir t] (cond (empty? sorted) [[] {}] :else (first sorted))
    ]
    (if (empty? sorted) () (t :body))
  )
)

(defn ai-in-fighter [ch chs body-to-char delta-time]
  (let [
      target (ch :target)
      tdir (target-dir ch body-to-char)
      [mx my mz] (math/normalize tdir)
    ]
    (cond
      (= target ()) (ch-move-in ch delta-time [0 0 0])
      (->> tdir mat/length (> 2)) (ch-look-action-in ch delta-time tdir :attack)
      :else (ch-move-in ch delta-time [mx 0 mz])
    )
  )
)

(defn ai-in-ninja [ch chs body-to-char delta-time]
  (let [
      target (ch :target)
      tdir (target-dir ch body-to-char)
      dist (mat/length tdir)
      seed (-> ch :body .hashCode (rem 5))
      ang (-> dist (quot 5) (+ seed) (rem 2) zero? (if 1 -1) (* Math/PI 1/3))
      [mx my mz] (math/normalize tdir)
      mang (+ ang (math/clock mx mz))
      [mx mz] (cond
         (-> ch :pos mat/length (< arena-size)) (math/clock-xy mang)
         :else [mx mz]
      )
    ]
    (cond
      (= target ()) (ch-move-in ch delta-time [0 0 0])
      (->> tdir mat/length (> 2)) (ch-look-action-in ch delta-time tdir :attack)
      :else (ch-move-in ch delta-time [mx 0 mz])
    )
  )
)

(defn ai-in-mage [ch chs body-to-char delta-time]
  (let [
      target (ch :target)
      tdir (target-dir ch body-to-char)
      [mx my mz] (math/normalize tdir)
    ]
    (cond
      (= target ()) (ch-move-in ch delta-time [0 0 0])
      (-> tdir mat/length (< 20)) (ch-look-action-in ch delta-time tdir :attack)
      :else (ch-move-in ch delta-time [mx 0 mz])
    )
  )
)

(defn ai-in-archer [ch chs body-to-char delta-time]
  (let [
      target (ch :target)
      tdir (target-dir ch body-to-char)
      dist (mat/length tdir)
      [mx my mz] (math/normalize tdir)
      g 9.8
      n arrow-initial-speed
      d (- dist arrow-z+)
      aim-angle (-> g (* 2 d) (/ 2 n n) Math/asin (/ 2))
      aim-y (Math/sin aim-angle)
      aim-l (->> (* aim-y aim-y) (- 1) Math/sqrt)
      aim (as-> [mx 0 mz] l
        (math/normalize l)
        (mapv * l (repeat aim-l))
        (assoc l 1 aim-y)
      )
    ]
    (cond
      (= target ()) (ch-move-in ch delta-time [0 0 0])
      (and
        (< dist 20)
        (-> ch :state :name (= :attack) not)
        (-> ch :pos mat/length (< arena-size))
      ) (ch-move-in ch delta-time (mapv - [mx 0 mz]))
      (< dist 40) (ch-look-action-in ch delta-time aim :attack)
      :else (ch-move-in ch delta-time [mx 0 mz])
    )
  )
)

(defn combat-ai-in [ch chs body-to-char delta-time]
  ((case (ch :name)
    :fighter ai-in-fighter
    :ninja ai-in-ninja
    :mage ai-in-mage
    :archer ai-in-archer
  ) ch chs body-to-char delta-time)
)

(defn passive-ai-in [ch chs body-to-char delta-time]
  (let [
      { :keys [pos look] } ch
      target-pos (get ch :target-pos pos)
      target-look (get ch :target-look look)
      mov (mapv - target-pos pos)
      mov (assoc mov 1 0)
      in (cond
        (< (mat/length mov) 1) (ch-look-in ch delta-time target-look)
        :else (ch-move-in ch delta-time (math/normalize mov))
      )
    ]
    in
  )
)

(defn passive-ai-next [chs body-to-char ch time delta-time]
  (let [
      target-pos (get ch :target-pos (ch :pos))
      target-look (get ch :target-look (ch :look))
      next (char-call ch :next (passive-ai-in ch chs body-to-char delta-time) time)
    ]
    (assoc next :target-pos target-pos :target-look target-look)
  )
)

(defn crowd-ai-in [ch chs body-to-char delta-time]
  (let [
      path (-> ch :state :path)
    ]
    (cond
      (empty? path) (move-in [0 0 0])
      :else (-> (mapv - (first path) (ch :pos)) (assoc 1 0) math/normalize move-in)
    )
  )
)

(declare crowd-walk-state)
(declare idle-pass-state)
(declare walk-pass-state)

(defn crowd-idle-state [nav spots time]
  (->
    (idle-pass-state time)
    (assoc
      :crowd? true
      :wait-time (-> (rand) (* 10))
      :next (fn [s ch in time]
        (cond
          (< (state-age s time) (s :wait-time)) (next-char-mov s ch in)
          :else (assoc ch :state
            (crowd-walk-state nav spots
              (->> spots count rand-int spots (nav/path nav (ch :pos)) first)
              time
            )
          )
        )
      )
    )
  )
)

(defn crowd-walk-state [nav spots path time]
  (-> (walk-pass-state time)
    (assoc
      :anim "walk_crowd"
      :walk-stat :walk-crowd-speed
      :walk-anim-stat :walk-crowd-anim-speed
      :crowd? true
      :path path
      :next (fn [s ch in time] (let [
            path (s :path)
            pos (ch :pos)
            path (cond
              (empty? path) path
              (->> path first (mapv - pos) mat/length (> 1)) (rest path)
              :else path
            )
            next (cond
              (empty? path) (assoc ch :state (crowd-idle-state nav spots time))
              :else (-> s (assoc :path path) (next-char-mov ch in))
            )
          ]
          next
        )
      )
    )
  )
)

(defn crowd-ai-next [nav spots chs body-to-char ch time delta-time]
  (let [
      ch (cond
        (-> ch :state :crowd?) ch
        :else (assoc ch :state (crowd-idle-state nav spots time))
      )
      next (char-call ch :next (crowd-ai-in ch chs body-to-char delta-time) time)
    ]
    next
  )
)

(defn combat-ai-next [chs body-to-char ch time delta-time]
  (let [ next (char-call ch :next (combat-ai-in ch chs body-to-char delta-time) time) ]
    (cond
      (-> ch :target (= ()))
        (assoc next :target (ai-target ch chs))
      (-> ch :target body-to-char is-alive false?)
        (assoc next :target ())
      :else next
    )
  )
)

(declare map-state)
(declare class-state)

(defn idle-state [time]
  (new-state :idle "idle" time (fn [s ch in time]
      (let [
          { :keys [movement action] } in
        ]
        (cond
          (= action :attack) (map-state ch :attack time)
          (math/zero-len? movement) (next-char-mov s ch in)
          :else (map-state ch :walk time)
        )
      )
    )
    (fn [s ch in time] (move-char ch [0 0 0]))
  )
)

(defn walk-state [time]
  (assoc (new-state :walk "walk" time (fn [s ch in time]
        (let [
            { :keys [movement action] } in
          ]
          (cond
            (= action :attack) (map-state ch :attack time)
            (math/zero-len? movement) (map-state ch :idle time)
            :else (next-char-mov s ch in)
          )
        )
      )
      (fn [s ch in time]
        (let [
            { :keys [movement] } in
          ]
          (move-char ch (mapv (partial * (get-char-stat ch (s :walk-stat))) movement))
          ()
        )
      )
    )
    :walk-stat :walk-speed
    :walk-anim-stat :walk-anim-speed
    :anim-time (fn [ch time] (->> ch :state :walk-anim-stat (get-char-stat ch) (* time)))
  )
)


(defn walk-pass-state [time]
  (assoc
    (walk-state time)
    :anim "walk_pass"
    :next (fn [s ch in time]
      (cond
        (-> in :movement math/zero-len?) (map-state ch :idle-pass time)
        :else (next-char-mov s ch in)
      )
    )
  )
)

(defn idle-pass-state [time]
  (assoc (idle-state time)
    :anim "idle_pass"
    :next (fn [s ch in time]
      (cond
        (-> in :movement math/zero-len?) (next-char-mov s ch in)
        :else (map-state ch :walk-pass time)
      )
    )
  )
)


(defn attack-state [attack-anims time]
  (let [
      anim (attack-anims (rand-int (count attack-anims)))
    ]
    (new-state :attack anim time
      (fn [s ch in time]
        (cond
          (< (char-anim-length ch anim) (state-age s time)) (map-state ch :idle time)
          :else (next-char-mov s ch in)
        )
      )
      (fn [s ch in time] (move-char ch [0 0 0]))
    )
  )
)

(defn projectile-attack-state [attack-anim projectile-spawn spawn-time spawn-force spawn-y+ spawn-z+ time]
  (let [
      atk (attack-state [attack-anim] time)
      effect-fn
      (fn [s ch res in phys time]
        (cond
          (or (-> s :has-arrow deref true?) (-> s (state-age time) (< spawn-time))) []
          :else (let [
              { :keys [look pos body world] } ch
              [lx ly lz] look
              arr-pos (mapv + pos (mapv * look (repeat spawn-z+)) [0 spawn-y+ 0])
              arr-rot [0 (math/clock lx lz) 0]
              arr (projectile-spawn time world body arr-pos arr-rot (-> ch :items first))
            ]
            (phys/set-velocity (arr :body) (mapv * look (repeat spawn-force)))
            (reset! (s :has-arrow) true)
            [(spawn-item-effect arr)]
          )
        )
      )
      state (assoc atk :effect effect-fn :has-arrow (atom false))
    ]
    state
  )
)

(defn block-state [time]
  (update
    (assoc (idle-state time)
      :name :block
      :anim "block"
      :anim-time (constantly 3/8)
    ) :next #(fn [s ch in time]
      (cond
        (-> in :action (= :block)) (% s ch in time)
        :else (map-state ch :idle time)
      )
    )
  )
)

(defn wrap-fighter-state [state-fn time]
  (update (state-fn time)
    :next #(fn [s ch in time]
      (cond
        (-> in :action (= :block)) (map-state ch :block time)
        :else (% s ch in time)
      )
    )
  )
)

(defn melee-attack-state [anims time]
  (let [
      atk (attack-state anims time)
      eff-fn (fn [s ch res in phys time]
        (let [
            { :keys [pos look body side] } ch
            { :keys [hit-check blood-check] } s
            { :keys [blood-particles] } res
            ctr (mapv + pos [0 1 0])
            cs (phys/sphere-check (ch :world) ctr (mapv + ctr (mapv (partial * 2) look)) 0.5)
            cs (disj (set cs) body)
            dmg (get-char-stat ch :attack-damage)
            body-to-char (phys :body-to-char)
          ]
          (mapv #(blood-damage-effect % hit-check :melee side dmg blood-particles blood-check (-> % body-to-char :pos) (repeatedly 3 rand) time)
            (->> cs
              (map body-to-char)
              (filter (comp not nil?))
              (filter #(-> % :state :name (not= :block)))
              (filter #(-> % :side (not= side)))
              (map :body)
            )
          )
        )
      )
    ]
    (assoc atk :effect eff-fn :hit-check (once-hit-check) :blood-check (atom true))
  )
)

(defn death-state [time]
  (assoc (new-state :death "death" time (fn [s ch in time]
      (cond
        (>= (state-age s time) (- (char-anim-length ch "death") 0.1)) (map-state ch :dead time)
        :else (next-char-idle s ch)
      )
    )
    (fn [s ch in time] (when
        (-> s :disposed deref false?)
        (phys/remove-body (ch :world) (ch :body))
        (-> s :disposed (reset! true))
      )
    )
  ) :disposed (atom false))
)
(defn dead-state [time]
  (new-state :dead "dead" time (fn [s ch in time] (next-char-idle s ch)))
)

(defn wrap-mortal [factory]
  (fn [time]
    (let [state (factory time)]
      (assoc state :next
        (fn [s ch in time]
          (cond
            (<= (ch :health) 0) (map-state ch :death time)
            :else ((state :next) s ch in time)
          )
        )
      )
    )
  )
)

(def base-state {
    :idle (wrap-mortal idle-state)
    :walk (wrap-mortal walk-state)
    :idle-pass idle-pass-state
    :walk-pass walk-pass-state
    :death death-state
    :dead dead-state
  }
)

(def states-map {
    :archer (assoc base-state
      :attack (wrap-mortal (partial projectile-attack-state "attack" arrow 1.2 arrow-initial-speed arrow-y+ arrow-z+))
    )
    :fighter (assoc base-state
      :attack (wrap-mortal (partial melee-attack-state ["attack" "attack_2"]))
      :block block-state
      :idle (partial wrap-fighter-state (base-state :idle))
    )
    :mage (assoc base-state
      :attack (wrap-mortal (partial projectile-attack-state "attackspell" fireball 1 10 1.5 1))
    )
    :ninja (assoc base-state
      :attack (wrap-mortal (partial melee-attack-state ["attack" "attack_2" "attack_3"]))
    )
  }
)

(defn class-state [name sym time]
  ((-> states-map name sym) time)
)

(defn map-state [ch sym time]
  (assoc ch :state (class-state (ch :name) sym time))
)

(defn load-char-materials [preset color]
  ((preset :materials-loader) (-> preset :model :materials) color)
)

(defn load-char [world preset pos look color side entry ai-next ai-in time]
  {
    :ai-next ai-next
    :ai-in ai-in
    :target ()
    :side side
    :health (-> preset :name char-stats :health)
    :world world
    :items (preset :items)
    :name (preset :name)
    :anims (preset :anims)
    :materials (load-char-materials preset color)
    :body (-> world
      (phys/capsule (mapv + [0 1 0] pos) [0 0 0] 0.25 3/2 1)
      (phys/set-rotation-enabled false)
    )
    :pos pos
    :look look
    :state (class-state (preset :name) entry time)
    :next (fn [ch in time]
      (let [
          state (ch :state)
          next (state :next)
        ]
        (next state ch in time)
      )
    )
    :update (fn [ch in time]
      (char-call (ch :state) :update ch in time)
    )
    :render (fn [ch time]
      (let [
          { :keys [state look pos materials] } ch
          { :keys [anim start] } state
          [lx ly lz] look
          anim-time (partial char-anim-time ch)
        ]
        (graph/push-matrix)
        (apply graph/translate pos)
        (graph/apply-matrix (math/rotation 0 (math/clock lx lz) 0))
        (render-preset preset materials anim (-> time (- start) anim-time))
        (graph/pop-matrix)
        ()
      )
    )
    :effect (fn [ch res in phys time]
      (-> ch :state (char-call :effect ch res in phys time))
    )
  }
)

(defn update-game-state [dev state]
  (let [
      { :keys [player non-players movement action time delta-time] } state
      all (cons player non-players)
      body-to-char (zipmap (map :body all) all)
    ]
    (update-char player (ch-move-action-in player delta-time movement action) time)
    (doseq [n non-players] (update-char n (--> n :ai-in (n all body-to-char delta-time)) time))
  )
)

(defn render-game-gui [dev res state]
  (let [
      { :keys [gui-asset] } res
      { :keys [player] } state
      health (/ (player :health) (get-char-stat player :health))
    ]
    (gui/status-bar gui-asset gui/aspect-fit-layout health
      [0 0 0 1]
      [2/3 0 0 1]
      -0.5 -0.9 1 0.06
    )
  )
)

(def cam+ 2)
(def camdist 8)
(def camrotx+ (/ Math/PI 12))

(defn player-cam [p]
  (let [
    look (p :look)
    pos (p :pos)
    [lx ly lz] look
  ] [
      (mapv + pos [0 cam+ 0] (mapv * look (repeat (- camdist))))
      [camrotx+ (math/clock lx lz) 0]
    ]
  )
)

(defn cam-rel-movement [keyboard camrot]
  (let [
      cammat (apply math/rotation camrot)
      cam-fwd (math/get-column-3 cammat 2)
      cam-right (math/get-column-3 cammat 0)
      
      [wasd-x wasd-y] (input/wasd keyboard)
      cam-fwd (mapv (partial * wasd-y) cam-fwd)
      cam-right (mapv (partial * wasd-x) cam-right)
      [mov-x mov-y mov-z] (mapv + cam-fwd cam-right)
      movement (math/normalize [mov-x 0 mov-z])
    ]
    movement
  )
)

(defn player-move-action-in [player keyboard camrot]
  (let[
      kind (player :name)
      movement (cam-rel-movement keyboard camrot)
      action (cond
        (input/key-down keyboard \f) :attack
        (and (= kind :fighter) (input/space-down keyboard)) :block
        :else :none
      )
    ]
    (move-action-in movement action)
  )
)

(defn next-game-state [dev res state]
  (let [
      { :keys [camrot campos player non-players items phys-world time delta-time] } state
      { :keys [keyboard mouse] } dev
      { :keys [movement action] } (player-move-action-in player keyboard camrot)

      contacts (phys/get-all-contacts phys-world)
      all-players (cons player non-players)
      body-to-char (zipmap (map :body all-players) all-players)
      phys { :contacts contacts :body-to-char body-to-char }
      in (ch-move-action-in player delta-time movement action)
      effects (apply concat (char-list-call (concat [player] non-players items) :effect res in phys time))
      [effect global-effect] (multi-effect effects)
      player (next-char player in time)
      player (effect player)
      non-players (mapv #(--> % :ai-next (all-players body-to-char % time delta-time)) non-players)
      non-players (mapv effect non-players)
      items (char-list-call items :next time)

      [player non-players]
      (cond (or (-> player is-alive false?) (input/key-up keyboard \c))
        (let [
            ps (zipmap (range) non-players)
            player-pred (every-pred is-alive #(-> % :side (= (player :side))))
            [i p] ((comp first filter) (comp player-pred second) (sort-by first ps))
          ]
          (cond
            (= p nil) [player non-players]
            :else
            (let [
                ps (->> (dissoc ps i) (sort-by first) (map second) vec)
                n (conj ps player)
              ] [p n]
            )
          )
        )
        :else [player non-players]
      )

      playerpos (player :pos)
      [lx ly lz] (player :look)
      camrot-xy (get state :camrot-xy [camrotx+ (math/clock lx lz)])
      arrows (input/arrows keyboard)
      aim? (input/shift-down keyboard)
      fov (if aim? 20 60)
      cam+ (if aim? (inc cam+) cam+)
      [arrows-x arrows-y] (cond aim? (mapv * arrows (repeat 1/6)) :else arrows)
      [cx cy] camrot-xy
      camrot-xy [(-> arrows-y - (* Math/PI delta-time) (+ cx)) (-> arrows-x (* Math/PI delta-time) (+ cy)) 0]
      camrot-xy (update camrot-xy 0 #(-> % (min (* Math/PI 1/3)) (max (* Math/PI 1/3 -1))))
      camrot camrot-xy
      cammat (apply math/rotation camrot)
      campiv (mapv + playerpos [0 cam+ 0])
      camdir (->> camdist - (math/vec3f 0 0) (.transformVector cammat) math/floats-from-vec3f)

      ray-origin campiv
      ray-len (mat/length camdir)
      { :keys [has-hit? dist normal] } (phys/ray-cast phys-world ray-origin camdir ray-len projectile-group)
      camdist (if has-hit? dist ray-len)
      normal (if has-hit? normal [0 0 0])
      camdir (mapv (partial * camdist) (math/normalize camdir))
      campos (mapv + campiv camdir (mapv * normal (repeat 0.5)))

      state (global-effect (assoc state
        :action action
        :campos (mat/lerp (state :campos) campos (* 5 delta-time))
        :camrot (math/lerpv-angle (state :camrot) camrot (* 10 delta-time))
        :camrot-xy camrot-xy
        :fov fov
        :movement movement
        :player player
        :items items
        :non-players non-players
      ))
    ]
    state
  )
)
