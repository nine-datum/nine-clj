(require
  '[nine-clj.datum :as dat]
  '[nine-clj.core :as core]
  '[nine-clj.graph :as graph]
  '[nine-clj.geom :as geom]
  '[nine-clj.gui :as gui]
  '[nine-clj.input :as input]
  '[nine-clj.phys :as phys]
  '[nine-clj.prof :as prof]
  '[nine-clj.math :as math]
  '[nine-clj.scripting :as scripting]
  '[nine-clj.scenes.arena :as arena]
  '[nine-clj.scenes.world :as world]
  '[nine-clj.scenes.menu :as menu]
  '[nine-clj.scenes.location :as location]
)
(use 'nine-clj.core)

(fn [dev]
  (menu/load-resources-let [
      gl (dev :gl)
      storage (dev :storage)
      mouse (dev :mouse)
      skin-shader (graph/load-shader gl storage "res/shaders/diffuse_skin_vertex.glsl" "res/shaders/diffuse_fragment.glsl")
      diffuse-shader (graph/load-shader gl storage "res/shaders/diffuse_vertex.glsl" "res/shaders/diffuse_fragment.glsl")
      blood-shader (graph/load-shader gl storage "res/shaders/particle_vertex.glsl" "res/shaders/particle_fragment.glsl")
      rain-shader (graph/load-shader gl storage "res/shaders/rain_vertex.glsl" "res/shaders/rain_fragment.glsl")
      blood-particles (graph/load-particles gl (graph/load-image gl storage "res/images/blood.png") blood-shader 5)
      rain-particles (graph/load-particles gl (graph/load-image gl storage "res/images/noise.png") rain-shader 1000)
      graphics (graph/load-graphics gl storage diffuse-shader skin-shader)
      arena-presets (dat/load-presets gl storage diffuse-shader skin-shader)
      world-presets (world/load-presets dev diffuse-shader skin-shader)
      all-presets (merge arena-presets world-presets)
      load-scene (fn [load-model-fn file]
        (hash-map
          :models (-> file load-model-fn vector)
          :shapes (->> file
            (geom/read-geom storage)
            (map #(map % [:vertex :root]))
            (mapv (partial apply phys/geom-shape))
          )
        )
      )
      script-load-func (fn [file]
        (fn [& args] (-> file scripting/read-file (apply args)))
      )
      arena (load-scene (partial graph/load-model graphics) "res/datum/scene/arena.dae")
      world (load-scene (partial world/load-world-model dev) "res/datum/scene/world/world.dae")
      world-markers (geom/geom-markers-map storage "res/datum/scene/world/world.dae")
      world-locations (-> "res/scripts/locations.clj" script-load-func (apply [dev world-markers all-presets]))
      world-water (graph/load-model graphics "res/datum/scene/world/water.dae")
      world-water-effect (world/load-water-model dev "res/datum/scene/world/water_effect.dae")
      arena-spawn (script-load-func "res/scripts/arena_spawn.clj")
      world-spawn (script-load-func "res/scripts/world_spawn.clj")
      gui-asset (gui/gui-asset (assoc dev :mouse (input/viewport-mouse mouse width height)))
      menu-image (graph/load-image gl storage "res/images/menu.png")
    ]
    {
      :blood-particles blood-particles
      :rain-particles rain-particles
      :skin-shader skin-shader
      :diffuse-shader diffuse-shader
      :graphics graphics
      :arena (assoc arena
        :presets arena-presets
        :spawn arena-spawn
        :update-state dat/update-game-state
        :update-phys phys/update-world
        :next-state dat/next-game-state
        :pos [0 0 0]
        :rot [0 0 0]
      )
      :world (assoc world
        :presets all-presets
        :models (->> world :models (cons world-water))
        :shapes (->> world-locations vals
          (map :shapes)
          (apply concat (world :shapes))
        )
        :spawn world-spawn
        :update-state (constantly ())
        :update-phys phys/update-world
        :next-state world/next-world-state
        :pos [0 0 0]
        :rot [0 0 0]
      )
      :world-locations world-locations
      :world-water-effect world-water-effect
      :gui-asset gui-asset
      :menu-image menu-image
      :arena-setup arena/arena-setup
      :arena-level arena/arena-level
      :arena-spawn arena/arena-spawn
      :world-setup world/world-setup
      :menu-setup menu/menu-setup
      :arena-pause-menu-setup menu/arena-pause-menu-setup
      :world-pause-menu-setup menu/world-pause-menu-setup
      :location-pause-menu-setup menu/location-pause-menu-setup
      :location-setup location/location-setup
      :location-enter-menu-setup menu/location-enter-menu-setup
      :base-enter-menu-setup menu/base-enter-menu-setup
      :game-over-menu-setup menu/game-over-menu-setup
      :win-menu-setup menu/win-menu-setup
    }
  )
)
