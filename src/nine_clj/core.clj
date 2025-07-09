(ns nine-clj.core
  (:gen-class)
  (:require
    [nine-clj.graph :as graph]
    [nine-clj.geom :as geom]
    [nine-clj.math :as math]
    [nine-clj.text :as text]
    [nine-clj.phys :as phys]
    [nine-clj.input :as input]
    [nine-clj.datum :as dat]
    [nine-clj.prof :as prof]
    [nine-clj.gui :as gui]
    [nine-clj.scenes.menu :as menu]
  )
  (:import
    [nine.lwjgl
      LWJGL_Window
    ]
    [nine.function
      UpdateRefreshStatus
    ]
    [nine.opengl
      WindowStartAction
      WindowLoopAction
    ]
    [nine.io
      FileStorage
    ]
    [nine.game
      Graphics
    ]
    [nine.geometry.procedural
      Geometry
    ]
    [org.lwjgl.glfw
      GLFW
    ]
  )
)

(defn new-status [] (UpdateRefreshStatus.))
(defn update-status [status] (.update status))

(def proc-refresh-status (new-status))

(defn get-time [] (org.lwjgl.glfw.GLFW/glfwGetTime))

(def window-width (atom 0))
(def window-height (atom 0))

(defn width [] @window-width)
(defn height [] @window-height)

(def state (atom {}))
(def resources (atom {}))

(defn windowLoop [win id dev res-atom]
  (proxy [WindowLoopAction] []
    (call [w h]
      (cond
        (= (. GLFW GLFW_TRUE) (. GLFW glfwGetWindowAttrib id GLFW/GLFW_FOCUSED))
        (do
          (update-status proc-refresh-status)
          (graph/reset-matrix-stack)
          (graph/reset-camera)
          (graph/reset-light)
          (graph/reset-projection)
          (reset! window-width w)
          (reset! window-height h)
          (prof/reset)
          (prof/profile :main-loop (swap! state (partial (@state :loop) dev @res-atom)))
        )
      )
      ((dev :mouse) :update)
      (-> dev :keyboard input/keyboard-update)
      (cond (nil? @state) (.close win))
    )
  )
)

(defn window-start [win setup]
  (proxy [WindowStartAction] []
    (start [id]
      (let [
          decoder (nine.io.encoding.ResourcesDecoder.)
          _ (-> "res.txt" nine.io.FileStorageResource. nine.io.encoding.FileDecoder. (.decode decoder nil))
          dev {
            :storage (.storage decoder)
            :gl (graph/new-gl)
            :keyboard (input/keyboard id)
            :mouse (input/mouse id proc-refresh-status)
            :get-time get-time
            :width width
            :height height
          }
          res-atom resources
        ]
        ((dev :mouse) :update)
        (-> dev :keyboard input/keyboard-update)
        (reset! state (menu/loading-menu-setup dev res-atom setup))
        ;(org.lwjgl.glfw.GLFW/glfwSwapInterval 0) ; fps unlocker
        (windowLoop win id dev res-atom)
      )
    )
  )
)

(defn window [w h setup params]
  (let [win (LWJGL_Window.)]
    (.run win w h
      (window-start win setup)
    )
  )
)

(defn -main [& args]
  (window 800 600 menu/menu-setup (apply hash-map args))
)
