(defn mapl [func l] (list* (map func l)))

(defn to-cl [val]
  (cond
    (vector? val) (mapl to-cl val)
    (map? val) (mapl (fn [[k v]] (list (to-cl k) '. (to-cl v))) val)
    (set? val) (mapl to-cl val)
    :else val
  )
)

(defn file-to-cl [src dst] (->> src slurp read-string to-cl (spit dst)))

(defn folder-to-cl [src dst]
  (doseq [f (.listFiles (java.io.File. src))]
    (.mkdirs (java.io.File. dst))
    (apply file-to-cl (map #(str % "/" (.getName f)) [src dst]))
  )
)

(folder-to-cl "res/datum/anims/archer" "../ten/res/anims/archer")

(defn folders-to-cl [src-root dst-root & names]
  (doseq [n names]
    (folder-to-cl (str src-root "/" n) (str dst-root "/" n))
  )
)

(folders-to-cl "res/datum/anims" "../ten/res/anims"
  "archer"
  "fighter"
  "mage"
  "ninja"
)
