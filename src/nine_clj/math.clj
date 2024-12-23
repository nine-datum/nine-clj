(ns nine-clj.math
  [:require
    [clojure.core.matrix :as mat]
  ]
  [:import
    [nine.math
      Vector2f
      Vector3f
      Matrix4f
      FloatFunc
    ]
  ]
)

(defn escape-nan [f] (if (Double/isNaN f) 0 f))

(defn mat-identity [] (. Matrix4f identity))

(defn vec-identity [] [
    1 0 0 0
    0 1 0 0
    0 0 1 0
    0 0 0 1
  ]
)

(defn floats-from-mat4f [^Matrix4f m] ((comp vec map) #(.at m %) (range 16)))
(defn floats-from-vec2f [^Vector2f v] [(.x v) (.y v)])
(defn floats-from-vec3f [^Vector3f v] [(.x v) (.y v) (.z v)])

(defn vec2f [x y] (. Vector2f newXY x y))

(defn vec3f [x y z] (. Vector3f newXYZ x y z))

(defn mat4f [fs]
  (. Matrix4f fromArray (double-array fs))
)

(defn radians [d]
  (. FloatFunc toRadians d)
)

(defn perspective [w h fov near far]
  (. Matrix4f perspective (/ w h) fov near far)
)

(defn normalize [v]
  (cond
    (empty? (filter (complement zero?) v)) (mapv (constantly 0) v)
    :else (mat/normalise v)
  )
)

(defn normalize-checked [v]
  (let [n (mat/normalise v)]
    (doseq [f n] (assert (not (Double/isNaN f))))
    n
  )
)

(defn clock "returns an angle for [x y] if it would be a clock arrow" [x y]
  (let [
      len (mat/length [x y])
      [y x] (if (zero? len) [1 0] (mat/normalise [x y]))
    ]
    (* (Math/acos x) (if (> y 0) 1 -1))
  )
)

(defn clock-xy "rotates clockwise vector [0 1] by an angle a" [a]
  [(Math/sin a) (Math/cos a)]
)

(defn x0y [x y] [x 0 y])

(defn transform [pos rot scale]
  (. Matrix4f transform (apply vec3f pos) (apply vec3f rot) (apply vec3f scale))
)

(defn scale [x y z]
  (. Matrix4f scale (vec3f x y z))
)

(defn translation [x y z]
  (. Matrix4f translation (vec3f x y z))
)

(defn rotation [x y z]
  (. Matrix4f rotation (vec3f x y z))
)

(defn get-vec-column-3 [mat n]
  (subvec (vec mat) (* n 4) (+ 3 (* n 4)))
)

(defn extract-rotation [m]
  (vec (concat (
    (get-vec-column-3 m 0) [0]
    (get-vec-column-3 m 1) [0]
    (get-vec-column-3 m 2) [0]
    [0 0 0 1]
  )))
)

(defn get-column-3 [^Matrix4f mat n]
  (let [
      p (* n 4)
    ]
    [  
      (.at mat p)
      (.at mat (+ 1 p))
      (.at mat (+ 2 p))
    ]
  )
)

(defn orbital-camera [pos rot dist]
  (->
    (. Matrix4f translation (vec3f 0 0 dist))
    (.mul (. Matrix4f rotationX (- (.x rot))))
    (.mul (. Matrix4f rotationY (- (.y rot))))
    (.mul (. Matrix4f rotationZ (- (.z rot))))
    (.mul (. Matrix4f translation (.negative pos)))
  )
)

(defn first-person-camera [pos rot]
  (. Matrix4f firstPersonCamera
    (apply vec3f pos)
    (apply vec3f rot)
  )
)

(defn lerp [a b t]
  (+ a (* t (- b a)))
)

(defn angle- [b a]
  (let [
      p Math/PI
      dp (* p 2)
      a (mod a dp)
      b (mod b dp)
      d (- b a)
      d (cond
        (> d p) (-> dp (- d) -)
        (< d (- p)) (+ dp d)
        :else d
      )
    ]
    d
  )
)

(defn lerp-angle [a b t]
  (let [
      dp (* Math/PI 2)
      a (mod a dp)
      d (angle- b a)
    ]
    (-> d (* t) (+ a))
  )
)

(defn lerpv [a b t]
  (mapv lerp a b (repeat t))
)

(defn lerpv-angle [a b t]
  (mapv lerp-angle a b (repeat t))
)

(defn clampv [v vmin vmax]
  (->> v (map min vmax) (mapv max vmin))
)

(defn look-rot[dir]
  (let[
      [x y z] dir
      rx (Math/asin (- y))
      ry (clock x z)
    ]
    [rx ry 0]
  )
)

(defn zero-len? [v] (zero? (mat/length v)))

(defn mul [& ms]
  (reduce #(.mul %1 %2) ms)
)
