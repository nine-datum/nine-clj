(ns nine-clj.graph
  [:require
    [nine-clj.math :as math]
    [nine-clj.text :as text]
    [nine-clj.prof :as prof]
    [nine-clj.mac :as mac]
    [nine-clj.part :as part]
  ]
  [:import
    [nine.lwjgl
      LWJGL_OpenGL
    ]
    [nine.drawing
      Color
    ]
    [nine.main
      TransformedDrawing
    ]
    [nine.math
      Matrix4f
    ]
    [nine.opengl
      OpenGL
      Shader
      ShaderPlayer
      Uniforms
      Uniform
      Drawing
      Texture
      Profiler
    ]
    [nine.function
      Condition
      RefreshStatus
    ]
    [nine.game
      Graphics
      AnimatedDrawing
    ]
    [nine.buffer
      Buffer
      CachedBuffer
    ]
    [nine.geometry
      Skeleton
      AnimatedSkeleton
      AnimatedSkeletonSource
      Animation
      Animator
      KeyFrameAnimation
      Material
      MaterialProvider
    ]
    [nine.geometry.collada
      NoAnimationParser
      ColladaBasicAnimationParser
      ColladaBasicSkeletonParser
      ColladaBasicSkinParser
      ColladaBasicGeometryParser
      ColladaBasicMaterialParser
      ColladaAnimationReader
      ColladaAnimationParser
      ColladaGeometryParser
      ColladaNode
      BuffersReader
      BufferMapping
    ]
  ]
)

(def nine-profiler
  (proxy [Profiler][]
    (profile [name func] (prof/profile name (.call func)))
  )
)

(defn new-gl [] (LWJGL_OpenGL.))

(def matrix-stack (atom (list (math/mat-identity))))
(def projection-matrix (atom (math/mat-identity)))
(def camera-matrix (atom (math/mat-identity)))
(def world-light-vec (atom (math/vec3f 0 0 1)))

(defn projection [p]
  (reset! projection-matrix p)
)

(defn get-projection [] @projection-matrix)

(defn camera [c]
  (reset! camera-matrix c)
)

(defn get-camera [] @camera-matrix)

(defn world-light [l]
  (reset! world-light-vec (apply math/vec3f l))
)

(defn get-world-light [] @world-light-vec)

(defn push-matrix [] (swap! matrix-stack #(cons (first %) %)))
(defn pop-matrix [] (swap! matrix-stack rest))
(defn peek-matrix [] (first @matrix-stack))
(defn swap-matrix [f] (swap! matrix-stack #(cons (->> % first f) (rest %))))
(defn apply-matrix "applies matrix m transformation after the head of the stack" [^Matrix4f m] (swap-matrix #(.mul % m)))
(defn reset-matrix-stack [] (reset! matrix-stack (list (math/mat-identity))))
(defn reset-camera [] (camera (math/mat-identity)))
(defn reset-light [] (world-light [0 0 1]))
(defn reset-projection [] (projection (math/mat-identity)))

(defn translate [x y z]
  (apply-matrix (math/translation x y z))
)

(defn rotate [x y z]
  (apply-matrix (math/rotation x y z))
)

(defn scale [x y z]
  (apply-matrix (math/scale x y z))
)

(defn load-shader [gl storage vert frag] (.load (. Shader loader storage gl) vert frag))

(defn load-graphics
  ([gl storage diffuse-shader skin-shader]
    (. Graphics collada gl diffuse-shader skin-shader storage)
  )
  ([gl storage diffuse-shader skin-shader geom-parser skin-parser anim-parser material-parser]
    (. Graphics collada gl diffuse-shader skin-shader storage geom-parser skin-parser anim-parser material-parser)
  )
)

(defn load-image-tex [gl tex]
  (let
    [
      bl [0 0 0]
      br [1 0 0]
      tl [0 1 0]
      tr [1 1 0]
      bl-uv [0 0]
      br-uv [1 0]
      tl-uv [0 1]
      tr-uv [1 1]
      to-float (partial map float)
      buf (comp vec to-float concat)
      geom (-> gl
        (.vao (. Buffer range 6))
        (.attribute 3 (. Buffer of (buf tl bl br br tr tl)))
        (.attribute 2 (. Buffer of (buf tl-uv bl-uv br-uv br-uv tr-uv tl-uv)))
        (.attribute 3 (. Buffer of (apply buf (repeat 6 [0 0 -1]))))
        (.drawing)
      )
      res { :gl gl :geom geom :tex tex :drawing (.apply tex geom) :disposed (atom false) }
    ]
    res
  )
)

(defn load-image [gl storage file]
  (load-image-tex gl
    (.texture gl (.open storage file) false)
  )
)

(defn unload-image [img]
  (->> img :tex .dispose)
  (->> img :geom .dispose)
  (-> img :disposed (reset! true))
  ()
)

(defn load-uniform [^Shader shader name kind val & args]
  (let [
      ^ShaderPlayer p (.player shader)
      ^Uniforms us (.uniforms p)
      ^Uniform u (case kind
        :color (.uniformColor us name)
        :vec3 (.uniformVector us name)
        :mat4 (.uniformMatrix us name)
        :texture (.uniformTexture us name (first args))
      )
      ^Drawing ud (mac/impl Drawing draw [] (.load u val))
      ^Drawing pd (.play p ud)
    ]
    (.draw pd)
  )
)

(defn load-uniform-color [shader name [r g b a]]
  (load-uniform shader name :color (Color/floats r g b a))
)

(defn load-uniform-vec3 [shader name [x y z]]
  (load-uniform shader name :vec3 (math/vec3f x y z))
)

(defn load-uniform-mat4 [shader name mat]
  (load-uniform shader name :mat4 mat)
)

(defn load-uniform-texture [shader name tex index]
  (load-uniform shader name :texture tex index)
)

(defn image [img ^Shader shader x y w h color]
  (let
    [
      ^ShaderPlayer player (.player shader)
      ^Uniforms uniforms (.uniforms player)
      ^Uniform trans (.uniformMatrix uniforms "transform")
      ^Uniform col (.uniformColor uniforms "color")
      [cr cg cb ca] color
      imgd (proxy [Drawing] []
        (draw []
          (.load trans (math/transform [x y 0] [0 0 0] [w h 1]))
          (.load col (. Color floats cr cg cb ca))
          (cond
            (-> img :disposed deref true?) (throw (RuntimeException. "Texture cannot be used, it was disposed"))
            :else (-> img :drawing .draw)
          )
        )
      )
      pd (.play player imgd)
    ]
    (.draw pd)
  )
)

(defn load-text-asset [gl storage file-name]
  (let
    [
      { :keys [img rects] } (text/load-text storage file-name)
      tex (load-image gl storage img)
    ]
    (assoc tex :rects rects)
  )
)

(defn unload-text-asset [asset] (unload-image asset))

(defn text [asset shader text x y w h color]
  (let [
      ^Drawing geom (text/text-geom (asset :gl) [w h] (asset :rects) text)
      ^Texture tex (asset :tex)
      drawing (.apply tex geom)
    ]
    (image (assoc asset :geom geom :drawing drawing) shader x y w h color)
    (.dispose geom)
  )
)

(defn load-particles [gl image shader num]
  { :image image :shader shader :gl gl :geom (part/part-geom gl num) }
)

(defn play-particles [particles pos rot scale time]
  (let [
      { :keys [^Drawing geom image ^Shader shader ^OpenGL gl] } particles
      t [time time time]
      ^Texture tex (image :tex)
      ^ShaderPlayer p (.player shader)
      m (math/transform pos rot scale)
      ^Matrix4f proj (get-projection)
      proj (.mul
        proj
        (get-camera)
      )
    ]
    (load-uniform-vec3 shader "time" t)
    (load-uniform-mat4 shader "transform" m)
    (load-uniform-mat4 shader "projection" proj)
    (load-uniform-color shader "color" [1 1 1 1])
    (load-uniform shader "worldLight" :vec3 (get-world-light))
    (->> geom (.depthOn gl) (.apply tex) (.play p) .draw)
  )
)

(defn unload-particles [geom] (.dispose geom))

(defn condition-func [func] (proxy [Condition] [] (match [t] (func t))))
(defn condition-equality [item] (. Condition equality item))

(defn load-materials [graphics file]
  (.materials graphics file)
)

(defn material-provider-colors [gl map]
  (proxy [MaterialProvider][]
    (material [name]
      (let [
          [cr cg cb ca] (get map name [1 0 1 1])
        ]
        (. Material textureAndColor (.blankTexture gl) (. Color floats cr cg cb ca))
      )
    )
  )
)

(defn material-provider-combine [a a-keys b]
  (let [
      a-keys (set a-keys)
      mat-fn (fn [name]
        (cond
          (contains? a-keys name) (.material a name)
          :else (.material b name)
        )
      )
      mat-fn (memoize mat-fn)
    ]
    (proxy [MaterialProvider] []
      (material [name] (mat-fn name))
    )
  )
)

(defn replace-materials [gl model map]
  (as-> gl m
    (material-provider-colors m map)
    (material-provider-combine m (keys map) (model :materials))
    (assoc model :materials m)
  )
)

(defn load-model [graphics file]
  {
    :model (.model graphics file)
    :materials (load-materials graphics file)
  }
)

(declare instance-always)

(defn load-anim [graphics file] (instance-always (.animation graphics file (condition-equality "JOINT"))))
(defn load-obj-anim [graphics file] (instance-always (.animation graphics file (condition-equality "NODE"))))

(defn skeleton-func [func]
  (proxy [Skeleton] []
    (transform [b]
      (func b)
    )
  )
)

(defn anim-func [func]
  (proxy [AnimatedSkeleton] []
    (animate [t]
      (skeleton-func
        (fn [b] (func t b))
      )
    )
  )
)

(defn empty-skeleton [] (skeleton-func (constantly (math/mat-identity))))

(defn empty-anim [] (anim-func (constantly (empty-skeleton))))

(defn comp-anim [a b]
  (anim-func
    (fn [t k]
      (let [
          at (.transform (.animate a t) k)
          bt (.transform (.animate b t) k)
        ]
        (.mul at bt)
      )
    )
  )
)

(defn anim-parser-func [func bone-pred]
  (proxy [ColladaAnimationParser] []
    (read [node reader]
      (.read reader
        (proxy [Animator] []
          (animation [id name]
            (cond
              (bone-pred name)
              (proxy [Animation] []
                (animate [t]
                  (func t name)
                )
              )
              :else nil
            )
          )
        )
      )
    )
  )
)

(defn instance [source refresh-status] (.instance source refresh-status))
(defn instance-always [source] (.instance source (. RefreshStatus always)))

(defn load-anim-clj [storage bone-type anim-file model-file]
  (let [
      db (-> (.open storage anim-file) nine.io.TextFileReader. .readString read-string)
      bone-names ((comp set map) first (db :bones))
      resources (db :resources)
      len (db :length)
      len (if (zero? len) 1 len)
      flip (math/mat4f [-1 0 0 0    0 0 1 0   0 1 0 0    0 0 0 1])
      flip-mat (fn [m]
        (.mul (.mul flip (math/mat4f m)) flip)
      )
      anims (db :bones)
      anims (mapv
        (fn [[n v]]
          [n
            (let [
                [ks vs] (apply mapv vector v)
                vs (map #(mapv resources %) vs)
                mats (for [[c0 c1 c2 c3] vs]
                  (concat c0 [0] c1 [0] c2 [0] c3 [1])
                )
              ]
              (KeyFrameAnimation.
                (. Buffer of (mapv float ks))
                (. Buffer of (mapv flip-mat mats))
              )
            )
          ]
        )
        anims
      )
      anims (apply hash-map (apply concat anims))
      f (fn [t name]
        (.animate (anims name) t)
      )
      node (. ColladaNode fromFile (.open storage model-file))
      aparser (anim-parser-func f (partial contains? bone-names))
      sparser (ColladaBasicSkeletonParser. (condition-func bone-type))
      anim (. AnimatedSkeleton fromCollada node aparser sparser)
    ]
    {
      :anim (. AnimatedSkeleton cached (instance-always anim) bone-names 30 len)
      :length len
    }
  )
)

(defn load-animated-model [graphics file]
  {
    :model (.animatedModel graphics file)
    :materials (load-materials graphics file)
  }
)

(defn animated-model
  ([md anim obj-anim] (animated-model md anim obj-anim (md :materials)))
  ([md anim obj-anim mats]
    (let [
        ^Matrix4f proj (get-projection)
        ^AnimatedDrawing ad (md :model)
        ^Drawing d (.animate ad
          (.mul
            proj
            (get-camera)
          )
          (get-world-light)
          (peek-matrix)
          anim
          obj-anim
          mats
        )
      ]
      (.draw d)
    )
  )
)

(defn animate [^AnimatedSkeleton anim ^double t] (.animate anim t))

(defn model
  ([md] (model md (md :materials)))
  ([md mats]
    (let [
        ^Matrix4f proj (get-projection)
        ^TransformedDrawing td (md :model)
        ^Drawing d (.transform td
          (.mul
            proj
            (get-camera)
          )
          (get-world-light)
          (peek-matrix)
          mats
        )
      ]
      (.draw d)
    )
  )
)
