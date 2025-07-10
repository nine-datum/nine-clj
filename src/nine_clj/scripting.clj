(ns nine-clj.scripting)

(defn read-file [storage file]
  (let [
      p (-> (.open storage file) .inputStream java.io.InputStreamReader. java.io.PushbackReader.)
      ex (loop [ex []]
        (let [e (read p false nil)]
          (cond (nil? e) ex :else (recur (conj ex e)))
        )
      )
      res (binding [*ns* (-> file java.io.File. .getName symbol create-ns)]
        (clojure.core/refer-clojure)
        (last (mapv eval ex))
      )
    ]
    res
  )
)
