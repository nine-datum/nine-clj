(ns nine-clj.phys
  (:require
    [nine-clj.geom :as geom]
    [nine-clj.math :as math]
    [nine-clj.prof :as prof]
    [clojure.core.matrix :as mat]
  )
  (:import
    (com.bulletphysics.dynamics
      DiscreteDynamicsWorld
      RigidBody
      RigidBodyConstructionInfo
    )
    (com.bulletphysics.dynamics.constraintsolver
      SequentialImpulseConstraintSolver
    )
    (com.bulletphysics.collision.broadphase
      AxisSweep3
    )
    (com.bulletphysics.collision.dispatch
      CollisionWorld$ClosestConvexResultCallback
      CollisionWorld$ClosestRayResultCallback
      CollisionWorld$LocalConvexResult
      CollisionDispatcher
    )
    (com.bulletphysics.collision.dispatch
      DefaultCollisionConfiguration
    )
    (com.bulletphysics.linearmath
      Transform
      DefaultMotionState
    )
    (com.bulletphysics.collision.shapes
      BoxShape
      CapsuleShape
      SphereShape
      StaticPlaneShape
      BvhTriangleMeshShape
      TriangleIndexVertexArray
    )
    (javax.vecmath
      Vector3f
      Matrix4f
      Matrix3f
    )
    (java.nio
      ByteBuffer
      ByteOrder
    )
  )
)

(defn dynamics-world []
  (let [
      collision-config (DefaultCollisionConfiguration.)
      dispatcher (CollisionDispatcher. collision-config)
      broadphase (AxisSweep3. (Vector3f. -10000 -10000 -10000) (Vector3f. 10000 10000 10000))
      solver (SequentialImpulseConstraintSolver.)
      dynamics-world (DiscreteDynamicsWorld. dispatcher broadphase solver collision-config)
    ]
    (.setGravity dynamics-world (Vector3f. 0 -9.81 0))
    dynamics-world
  )
)

(declare set-matrix)
(declare rotmat)
(declare get-position)
(declare get-velocity)
(declare get-matrix)
(declare set-angular-velocity)

(defn add-rigid-body [dynamics-world shape pos rot mass]
  (let [
      [px py pz] pos
      [rx ry rz] rot
      mat3 (apply rotmat rot)
      mat4 (Matrix4f. mat3 (Vector3f. px py pz) (float 1))
      transform (Transform. mat4)
      motion-state (DefaultMotionState. transform)
      local-inertia (Vector3f. 0 0 0)
    ]
    (.calculateLocalInertia shape mass local-inertia)
    (let [
        rbci (RigidBodyConstructionInfo. mass motion-state shape local-inertia)
        body (RigidBody. rbci)
      ]
      (.addRigidBody dynamics-world body)
      (set-matrix body (math/floats-from-mat4f (math/transform pos rot [1 1 1])))
      body
    )
  )
)

(defn set-group [body world group mask]
  (.removeRigidBody world body)
  (.addRigidBody world body group mask)
  body
)

(defn remove-body [^DiscreteDynamicsWorld world body]
  (.removeRigidBody world body)
)

(defn extract-vec3 [^Vector3f v]
  [(.x v) (.y v) (.z v)]
)

(defn make-vec3 [^double x ^double y ^double z]
  (Vector3f. x y z)
)

(defn rotx [x]
  (doto (Matrix3f.) .setIdentity (.rotX x))
)

(defn roty [y]
  (doto (Matrix3f.) .setIdentity (.rotY y))
)

(defn rotz [z]
  (doto (Matrix3f.) .setIdentity (.rotZ z))
)

(defn rotmat [x y z]
  (doto (rotx x)
    (.mul (roty y))
    (.mul (rotz z))
  )
)

(defn transform-position [t pos]
  (let [
      ^Matrix4f m (.getMatrix t (Matrix4f.))
      [x y z] (mapv float pos)
      m (do (.setColumn m 3 x y z 1) m)
    ]
    (Transform. m)
  )
)

(defn box [world pos rot [scale-x scale-y scale-z] mass]
  (add-rigid-body world (BoxShape. (Vector3f. scale-x scale-y scale-z))
    pos rot mass
  )
)

(defn capsule [world pos rot radius height mass]
  (add-rigid-body world (CapsuleShape. radius height)
    pos rot mass
  )
)

(defn sphere [world pos rot radius mass]
  (add-rigid-body world (SphereShape. radius)
    pos rot mass
  )
)

(defn plane[world [nx ny nz] const]
  (add-rigid-body world
    (StaticPlaneShape. (Vector3f. nx ny nz) const)
    [0 0 0] [0 0 0] 0
  )
)

(defn set-gravity [^RigidBody body [gx gy gz]]
  (.setGravity body (Vector3f. gx gy gz))
  body
)

(defn set-velocity [^RigidBody body [vx vy vz]]
  (.activate body)
  (.setLinearVelocity body (Vector3f. vx vy vz))
  body
)

(defn update-velocity [^RigidBody body func]
  (->> body get-velocity func (set-velocity body))
)

(defn apply-world-force [^RigidBody body force at]
  (let [
      rel (mapv - at (get-position body))
      ang (mat/cross rel force)
    ]
    (.applyCentralForce body (apply make-vec3 force))
    (.applyTorque body (apply make-vec3 ang))
    body
  )
)

(defn rotate-by-relative-force [^RigidBody body force rel]
  (->> force (mat/cross rel) (set-angular-velocity body))
)

(defn apply-local-force [^RigidBody body force at]
  (let [
      [force at] (map
        #(-> body get-matrix math/mat4f
          (.transformVector (apply math/vec3f %))
          math/floats-from-vec3f
        )
        [force at]
      )
    ]
    (.applyCentralForce body (apply make-vec3 force))
    (rotate-by-relative-force body force at)
    body
  )
)

(defn apply-force [^RigidBody body force]
  (.applyCentralForce body (apply make-vec3 force))
)

(defn get-point-velocity [^RigidBody body point]
  (let [
      l (.getLinearVelocity body (Vector3f.))
      a (.getAngularVelocity body (Vector3f.))
      [px py pz] point
      o (.origin (.getCenterOfMassTransform body (Transform.)))
      s (Vector3f.)
      c (Vector3f.)
      r (Vector3f.)
    ]
    (.sub s (Vector3f. px py pz) o)
    (.cross c a s)
    (.add r c l)
    (extract-vec3 r)
  )
)

(defn get-velocity [^RigidBody body]
  (let [
      v (.getLinearVelocity body (Vector3f.))
    ]
    [(.x v) (.y v) (.z v)]
  )
)

(defn set-angular-velocity [^RigidBody body [vx vy vz]]
  (.activate body)
  (.setAngularVelocity body (Vector3f. vx vy vz))
  body
)

(defn get-angular-velocity [^RigidBody body]
  (let [
      v (.getAngularVelocity body (Vector3f.))
    ]
    [(.x v) (.y v) (.z v)]
  )
)

(defn set-matrix [^RigidBody body mat]
  (let [
      ^Matrix4f m (Matrix4f.)
      cs (partition 4 (map float mat))
      m (do (doseq [[i [x y z w]] (map vector (range) cs)]
        (.setColumn m i x y z w)
      ) m)
      t (Transform. m)
    ]
    (.setCenterOfMassTransform body t)
  )
  body
)

(defn set-position [^RigidBody body pos]
  (let [
      t (.getCenterOfMassTransform body (Transform.))
      t (transform-position t pos)
    ]
    (.setCenterOfMassTransform body t)
  )
  body
)

(defn set-rotation [^RigidBody body rot]
  (let [
      mat3 (apply rotmat rot)
      t (Transform.)
    ]
    (.getCenterOfMassTransform body t)
    (-> t .basis (.set mat3))
    (.setCenterOfMassTransform body t)
    body
  )
)

(defn rotate [^RigidBody body rot]
  (set-matrix body
    (-> body get-matrix math/mat4f
      (math/mul (apply math/rotation rot))
      math/floats-from-mat4f
    )
  )
)

(defn set-rotation-enabled [^RigidBody body v]
  (.setAngularFactor body (if v 1 0))
  body
)

(defn get-matrix [^RigidBody body]
  (let [
      t (.getCenterOfMassTransform body (Transform.))
      m (.getMatrix t (Matrix4f.))
      cs (mapv (partial make-array Float/TYPE) [4 4 4 4])
    ]
    (doseq [i (range 4)] (.getColumn m (int i) (cs i)))
    ((comp vec (partial apply concat)) cs)
  )
)

(defn get-position [^RigidBody body]
  (let [
      t (.getCenterOfMassTransform body (Transform.))
      m (.getMatrix t (Matrix4f.))
      fs (make-array Float/TYPE 4)
    ]
    (.getColumn m 3 fs)
    (vec (take 3 fs))
  )
)

(defn get-look [^RigidBody body]
  (-> body
    get-matrix
    math/mat4f
    (.transformVector (math/vec3f 0 0 1))
    math/floats-from-vec3f
    math/normalize
  )
)

(defn update-world [^DiscreteDynamicsWorld world time-step]
  (.stepSimulation world time-step 1/50)
  world
)

(defn byte-buffer [size]
  (.order (. ByteBuffer allocateDirect size) (. ByteOrder nativeOrder))
)

(defn float-byte-buffer [fs]
  (let [
      v (vec fs)
      c (count v)
      b (byte-buffer (* 4 c))
    ]
    (-> b .asFloatBuffer (.put (float-array v)))
    b
  )
)

(defn int-byte-buffer [is]
  (let [
      v (vec is)
      c (count v)
      b (byte-buffer (* 4 c))
    ]
    (-> b .asIntBuffer (.put (int-array v)))
    b
  )
)

(defn geom-shape [verts root]
  (let [
      v (vec verts)
      c (/ (count v) 3)
      i (vec (range c))
      t (/ (count i) 3)
      vbuf (float-byte-buffer v)
      fvbuf (.asFloatBuffer vbuf)
      _ (.applyToPointBuffer root fvbuf fvbuf)
      ibuf (int-byte-buffer i)
      arr (TriangleIndexVertexArray. t ibuf 12 c vbuf 12)
    ]
    { :pos [0 0 0] :rot [0 0 0] :shape (BvhTriangleMeshShape. arr true) }
  )
)

(defn move-char [body vel]
  (let [
      [vx vy vz] (get-velocity body)
      [mx my mz] vel
    ]
    (set-velocity body [mx vy mz])
  )
)

(defn get-all-contacts [^DiscreteDynamicsWorld world]
  (let [
      d (.getDispatcher world)
      n (.getNumManifolds d)
      ms (map #(.getManifoldByIndexInternal d %) (range n))
      ms (filter #(not= 0 (.getNumContacts %1)) ms)
      cs (mapv #(vector (.getBody0 %) (.getBody1 %)) ms)
      cs (apply concat cs)
    ]
    (apply hash-map (concat cs (reverse cs)))
  )
)

(defn make-mask [ignore-groups]
  (cond
    (empty? ignore-groups) -1
    (-> ignore-groups rest empty?) (-> ignore-groups first bit-not short)
    :else (-> (apply bit-or ignore-groups) bit-not short)
  )
)

(defn sphere-check [^DiscreteDynamicsWorld world from to radius & ignore-groups]
  (let [
      ss (SphereShape. radius)
      tf (fn [t c]
        (.setIdentity t)
        (transform-position t c)
      )
      tfrom (tf (Transform.) from)
      tto (tf (Transform.) to)
      res (atom ())
      vf (fn [x y z] (Vector3f. x y z))
      callback (proxy [CollisionWorld$ClosestConvexResultCallback] [(apply vf from) (apply vf to)]
        (addSingleResult [r n]
          (swap! res conj (.hitCollisionObject r))
          1.0
        )
      )
    ]
    (set! (.collisionFilterMask callback) (make-mask ignore-groups))
    (.convexSweepTest world ss tfrom tto callback)
    @res
  )
)

(defn sphere-cast [^DiscreteDynamicsWorld world from to radius & ignore-groups]
  (let [
      shape (doto (SphereShape. radius))
      [fx fy fz] from
      [tx ty tz] to
      ft (doto (Transform.) .setIdentity (-> .origin (.set fx fy fz)))
      tt (doto (Transform.) .setIdentity (-> .origin (.set tx ty tz)))
      callback (proxy [CollisionWorld$ClosestConvexResultCallback] [(.origin ft) (.origin tt)]
        (addSingleResult [^CollisionWorld$LocalConvexResult r n]
          (cond (< (.hitFraction r) (proxy-super closestHitFraction))
            (proxy-super addSingleResult r n)
            :else (proxy-super closestHitFraction)
          )
        )
      )
      _ (set! (.collisionFilterMask callback) (make-mask ignore-groups))
      _ (.convexSweepTest world shape ft tt callback)
      has-hit? (.hasHit callback)
      point (-> callback .hitPointWorld extract-vec3)
      normal (-> callback .hitNormalWorld extract-vec3)
      dist (mat/length (mapv - point from))
    ]
    {
      :has-hit? has-hit?
      :point point
      :normal normal
      :dist dist
    }
  )
)

(defn ray-cast [^DiscreteDynamicsWorld world origin dir dist & ignore-groups]
  (let [
      dir (math/normalize dir)
      [fx fy fz] origin
      [tx ty tz] (mapv + origin (mapv (partial * dist) dir))
      from (Vector3f. fx fy fz)
      to (Vector3f. tx ty tz)
      callback (CollisionWorld$ClosestRayResultCallback. from to)
    ]
    (set! (.collisionFilterMask callback) (make-mask ignore-groups))
    (.rayTest world from to callback)
    (let [
        has-hit? (.hasHit callback)
        normal (math/normalize (extract-vec3 (.hitNormalWorld callback)))
        point (extract-vec3 (.hitPointWorld callback))
        dist (mat/length (mapv - point origin))
      ]
      {
        :has-hit? has-hit?
        :normal normal
        :point point
        :dist dist
      }
    )
  )
)
