(import
  (java.io File FileInputStream FileOutputStream)
  (java.lang Byte String)
  (java.nio ByteBuffer)
  (java.nio.file Files Paths FileVisitOption)
  (java.nio.charset Charset)
  (java.util.function BiPredicate)
  (java.util.stream Collectors)
  (java.util ArrayList)
)

(def charset (Charset/forName "US-ASCII"))

(defn write-int [stream int]
  (.write stream (-> (ByteBuffer/allocate 4) (.putInt int) .array))
)

(defn encode-file [path stream]
  (println (str "encoding " path " ..."))
  (let [
      buffer (make-array Byte/TYPE 4096)
      vector (ArrayList.)
      source (FileInputStream. (File. path))
    ]
    (loop [i (.read source buffer)]
      (when (> i 0)
        (.addAll vector (take i buffer))
        (recur (.read source buffer))
      )
    )
    (let [
        text (.array (.encode charset path))
        data (byte-array vector)
      ]
      (write-int stream (count text))
      (.write stream text)
      (write-int stream (count data))
      (.write stream data)
    )
  )
)
(defn encode-files [paths stream]
  (write-int stream (count paths))
  (mapv #(encode-file % stream) paths)
)
(defn encode-root [root stream]
  (let [
      path (Paths/get root (make-array String 0))
      files (Files/find path 1024 (proxy [BiPredicate] [] (test [a b] (.isRegularFile b))) (make-array FileVisitOption 0))
      files (.collect files (Collectors/toList))
      files (mapv #(str path "/" (.relativize path %)) files)
    ]
    (encode-files files stream)
    (.flush stream)
  )
)
(encode-root "res" (-> "res.txt" File. FileOutputStream.))
