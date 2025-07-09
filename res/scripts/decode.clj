(import
  (java.io File FileInputStream FileOutputStream)
  (java.lang Byte String)
  (java.nio ByteBuffer)
  (java.nio.file Files Paths FileVisitOption)
  (java.nio.charset Charset)
  (java.util.function BiPredicate)
  (java.util.stream Collectors)
  (java.util ArrayList HashMap)
)

(def charset (Charset/forName "US-ASCII"))

(defn read-int [stream]
  (let [
      buf (make-array Byte/TYPE 4)
    ]
    (.read stream buf)
    (-> buf ByteBuffer/wrap .getInt)
  )
)

(defn decode-file [stream]
  (let [
      table (HashMap.)
      files-num (read-int stream)
    ]
    (doseq [_ (range files-num)]
      (let [
          name-len (read-int stream)
          name-buf (make-array Byte/TYPE name-len)
          name (do
            (.read stream name-buf)
            (String. name-buf charset)
          )
          file-size (read-int stream)
          buffer (make-array Byte/TYPE file-size)
        ]
        (.read stream buffer)
        (.put table name buffer)
      )
    )
    table
  )
)

(-> "res.txt" File. FileInputStream. decode-file)
