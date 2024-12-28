(ns nine-clj.scenes.menu
  [:require
    [nine-clj.gui :as gui]
    [nine-clj.datum :as dat]
    [nine-clj.io :as io]
    [nine-clj.graph :as graph]
    [nine-clj.math :as math]
    [nine-clj.input :as input]
    [nine-clj.scenes.generic :as generic]
    [nine-clj.scenes.world :as world]
    [nine-clj.scenes.location :as location]
    [nine-clj.scripting :as scripting]
    [nine-clj.mac :refer [-->]]
  ]
)

(declare menu-loop)
(declare pause-menu-loop)

(defn menu-setup [dev res]
  {
    :loop menu-loop
    :gui-asset (res :gui-asset)
    :images [
      [(res :menu-image) gui/aspect-fit-layout [-1.5 -1 3 2]]
    ]
    :texts [
      [(str "nine-clj" (. System getProperty "nine-clj.version")) gui/aspect-fit-layout [0 1 0 1] [-0.5 -0.9 1 0.1]]
    ]
    :buttons (mapv [
      ["Начать игру" (fn [dev res state] ((res :world-setup) dev res))]
      ["Загрузить игру" (fn [dev res state] (io/load-game dev res "saves/main.save"))]
      ["Настройки" (fn [dev res state] state)]
      ["Выход" (constantly nil)]
    ] (cond
        (-> "saves/main.save" java.io.File. .exists) [0 1 2 3]
        :else [0 2 3]
      )
    )
  }
)
(defn menu-loop-base [dev res state]
  (let [
      { :keys [gui-asset images buttons texts] } state
      _ (doseq [[image layout [x y w h]] images] (gui/image gui-asset layout image x y w h [1 1 1 1]))
      _ (doseq [[label layout color [x y w h]] texts] (gui/text gui-asset layout label x y w h color))
      bs (mapv (fn [[label func] i]
          (vector
            (gui/button gui-asset gui/aspect-fit-layout label -0.5 (-> i (+ 0.4) (* -0.2)) 1 0.1)
            func
          )
        )
        buttons
        (range)
      )
    ]
    (nth (->> bs (filter (comp true? first)) (map (comp #(% dev res state) last))) 0 state)
  )
)
(defn menu-loop [dev res state]
  (doto (dev :gl)
    (.clearDepth)
    (.clearColor 0 0 0 0)
  )
  (menu-loop-base dev res state)
)

(defn pause-menu-setup [dev res images texts buttons render-loop resume-state]
  {
    :loop pause-menu-loop
    :gui-asset (res :gui-asset)
    :buttons buttons
    :images images
    :texts texts
    :resume-state resume-state
    :render-loop render-loop
    :escape-state :resume-state
  }
)

(defn pause-menu-loop [dev res state]
  ((state :render-loop) dev res (state :resume-state))
  (cond
    (-> dev :keyboard input/escape-up) (--> state :escape-state state)
    :else (menu-loop-base dev res state)
  )
)

(defn arena-pause-menu-setup [dev res resume-state]
  (pause-menu-setup dev res [] [] [
      ["Продолжить" (fn [dev res state] (state :resume-state))]
      ["Покинуть сражение" (fn [dev res state] (-> state :resume-state :exit-setup
        (apply [dev res (state :resume-state)])))]
      ["Выйти в меню" (fn [dev res state] (menu-setup dev res))]
    ]
    generic/generic-render-loop
    resume-state
  )
)

(defn world-pause-menu-setup [dev res resume-state]
  (pause-menu-setup dev res [] [] [
      ["Продолжить" (fn [dev res state] (state :resume-state))]
      ["Сохранить игру" (fn [dev res state] (doto
        (-> state :resume-state)
        (io/save-game "saves/main.save")
      ))]
      ["Выйти в меню" (fn [dev res state] (menu-setup dev res))]
    ]
    world/world-render-loop
    resume-state
  )
)

(defn location-pause-menu-setup [dev res resume-state]
  (pause-menu-setup dev res [] [] [
      ["Продолжить" (fn [dev res state] (state :resume-state))]
      ["Выйти из города" (fn [dev res state] (-> state :resume-state :world-state-setup (apply [(state :resume-state)])))]
      ["Выйти в меню" (fn [dev res state] (menu-setup dev res))]
    ]
    location/location-render-loop
    resume-state
  )
)

(declare army-menu-setup)

(defn location-enter-menu-setup [dev res location-id location-state-setup arena-state-setup exit-state-setup resume-state]
  (assoc (pause-menu-setup dev res []
    [
      ["Вы приблизились к городу" gui/aspect-fit-layout [1 1 1 1] [-0.5 0.2 1 0.1]]
    ]
    [
      ["Зайти" (fn [dev res state] (location-state-setup))]
      ["Нанять воинов" (fn [dev res state] (->> state exit-state-setup (army-menu-setup dev res location-id)))]
      ["Напасть" (fn [dev res state] (arena-state-setup))]
      ["Уйти" (fn [dev res state] (exit-state-setup state))]
    ]
    world/world-render-loop
    resume-state
  ) :escape-state exit-state-setup)
)

(defn game-over-menu-setup [dev res]
  {
    :gui-asset (res :gui-asset)
    :loop menu-loop
    :texts [
      ["Погиб последний воин из вашего отряда." gui/aspect-fit-layout [1 1 1 1] [-0.5 0.2 1 0.1]]
      ["Игра окончена." gui/aspect-fit-layout [2/3 1/4 1/4 1] [-0.5 0.1 1 0.1]]
    ]
    :buttons [
      ["Выйти в меню" (fn [dev res state] (menu-setup dev res))]
    ]
  }
)

(declare loading-menu-loop)

(defn loading-menu-setup [dev res-atom setup]
  (let [
      { :keys [gl storage mouse width height] } dev
      gui-asset (gui/gui-asset (assoc dev :mouse (input/viewport-mouse mouse width height)))
      menu-image (graph/load-image gl storage "res/images/menu.png")
      [load-funcs res-func]  (-> "res/scripts/resources.clj" scripting/read-file (apply [dev]))
    ]
    (swap! res-atom #(assoc % :gui-asset gui-asset :menu-image menu-image))
    {
      :loop loading-menu-loop
      :images [
        [menu-image gui/aspect-fit-layout [-1.5 -1 3 2]]
      ]
      :texts []
      :buttons []
      :gui-asset gui-asset
      :res-atom res-atom
      :load-atom (atom {})
      :load-funcs load-funcs
      :res-func res-func
      :progress 0
      :max-progress (count load-funcs)
    }
  )
)

(defn loading-menu-loop [dev res state]
  (let [
      state (menu-loop dev res state)
      { :keys [gui-asset] } res
      { :keys [res-atom load-atom res-func load-funcs load-atom progress max-progress] } state
      prog (/ progress max-progress)
      prog-col (math/lerpv [0 0 1 1] [0 1 0 1] prog)
      prog-back-col [1/2 1/2 1/2 1]
      load-title (-> load-funcs second
        (get :name "...")
        name
        (.replace "-" " ")
      )
      load-title (str "Загрузка : " load-title)
    ]
    (gui/text gui-asset gui/aspect-fit-layout load-title -0.5 -0.1 1 0.1 [1 1 1 1])
    (gui/status-bar gui-asset gui/aspect-fit-layout prog prog-back-col prog-col -0.5 -0.2 1 0.05)
    (cond
      (empty? load-funcs) (menu-setup dev (reset! res-atom ((res-func :func) @load-atom)))
      :else (do
        (swap! load-atom (-> load-funcs first :func))
        (assoc state
          :progress (inc progress)
          :load-funcs (rest load-funcs)
        )
      )
    )
  )
)

(defmacro load-resources-let [pairs expr]
  (let [
      pairs (partition 2 pairs)
      names (mapv first pairs)
      pair (fn [n f] { :name n :func f } )
      func-template (fn [[n e]] (pair (keyword n)
        `(fn [lets#] (let [
            { :keys [~@names] } lets#
          ]
          (assoc lets# (keyword (name '~n)) ~e)
        )))
      )
      funcs (mapv func-template pairs)
      expr-func (pair :final `(fn [lets#]
        (let [
            { :keys [~@names] } lets#
          ]
          ~expr
        )
      ))
    ]
    (list list funcs expr-func)
  )
)

(declare army-menu-loop)

(defn army-menu-setup [dev res location-id world-state]
  (let [
      { :keys [gui-asset] } res
      player (world-state :player)
      color (player :color)
      army (-> player world/get-unit-army rest)
      recr (-> world-state :locations location-id :recruits)
      presets (-> res :arena :presets)
      materials (update-vals presets #(dat/load-char-materials % color))
    ]
    {
      :presets presets
      :materials materials
      :army army
      :recr recr
      :gui-asset gui-asset
      :world-state world-state
      :loop army-menu-loop
      :location-id location-id
    }
  )
)

(defn army-menu-loop [dev res state]
  (doto (dev :gl)
    (.clearDepth)
    (.clearColor 1/2 1/2 3/4 1)
  )
  (graph/projection
    (math/perspective
      (--> dev :width ())
      (--> dev :height ())
      (math/radians 60) 0.3 1000
    )
  )
  (graph/camera (math/first-person-camera [0 2 -4] [(/ Math/PI 8) 0 0]))
  (graph/world-light [0 -1 0])
  (->> res :arena :models (map graph/model) dorun)
  (let [
      player (-> state :world-state :player)
      asset (state :gui-asset)
      { :keys [army recr location-id] } state
      { :keys [color side] } player
      { :keys [width height] } dev
      nums-fn #(merge { :fighter 0 :archer 0 :ninja 0 :mage 0 } (frequencies %))
      nums (nums-fn army)
      recr-nums (nums-fn recr)
      layout gui/aspect-fit-layout
      bsx 0.5
      bsy 0.1
      xs -1
      ys -0.5
      ws 0.5
      hs 1
      _ (gui/blank asset layout -1 -0.9 2 0.7 [1/4 1/4 1/4 1])
      _ (gui/text asset layout "Доступно для найма :" -0.5 (- ys bsy) 1 bsy [1 1 1 1])
      paint-kind (fn [index kind num x y w h]
        (graph/push-matrix)
        (graph/translate (* 1.3 (- index 1.5)) 0 1)
        (graph/rotate 0 Math/PI 0)
        (dat/render-preset
          (-> state :presets kind)
          (-> state :materials kind)
          "idle_pass"
          (--> dev :get-time ())
        )
        (graph/pop-matrix)
        (let [
            rec (recr-nums kind)
            minus (when (> num 0) (gui/button asset layout "Отпустить" x y bsx bsy))
            plus (when (> rec 0) (gui/button asset layout "Нанять" x (+ y bsy bsy) bsx bsy))
            preset (-> state :presets kind)
            materials (-> state :materials kind)
          ]
          (gui/text asset layout (str num) (- x 1/4) (+ y bsy) 1 bsy [1 1 1 1])
          (gui/text asset layout (str rec) (- x 1/4) (- y bsy bsy (/ bsy 2)) 1 bsy [1 1 1 1])
          (cond
            minus -1
            plus 1
            :else 0
          )
        )
      )
      diff (mapv
        (fn [[kind num] i]
          [kind (paint-kind i kind num (-> i (* ws) (+ xs)) ys ws hs)]
        )
        nums (range)
      )
      diff (into {} diff)
      apply-diff (fn [nums diff]
        (->> nums
          (reduce-kv (fn [m k v] (assoc m k (+ v (diff k)))) {})
          (map (fn [[k v]] (repeat v k)))
          (apply concat)
        )
      )
      army (apply-diff nums diff)
      recr (apply-diff recr-nums (update-vals diff -))
      cancel (gui/button asset layout "Отмена" -1 -0.9 1 0.1)
      ok (gui/button asset layout "Принять" 0 -0.9 1 0.1)
      state (assoc state :army army :recr recr)
    ]
    (cond
      ok (-> state :world-state
        (update :player #(assoc % :army army))
        (update :locations #(update % location-id (fn [l] (assoc l :recruits recr))))
      )
      cancel (state :world-state)
      :else state
    )
  )
)

(defn win-menu-setup [dev res exit-state]
  {
    :gui-asset (res :gui-asset)
    :loop menu-loop
    :buttons [
      ["Далее" (fn [dev res state] exit-state)]
    ]
    :texts [
      ["Вы захватили город!" gui/aspect-fit-layout [1 1 1 1] [-0.5 0.2 1 0.1]]
    ]
  }
)
