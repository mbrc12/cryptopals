(load-file "singlebytexor.clj")

(def message "Burning 'em, if you ain't quick and nimble\nI go crazy when I hear a cymbal")

(def bits-to-str (comp hex-to-str bits-to-hex))

(defn repeating-key-xor [s k]
  (let [sb  (str-to-bits s)
        kb  (str-to-bits k)
        res (map bit-xor sb (cycle kb))
        es  (bits-to-hex res)]
    es))

(defn hamming-distance-bits [s t]
  (apply + 
         (map bit-xor s t)))

(defn hamming-distance [s t]
  (hamming-distance-bits (str-to-bits s) (str-to-bits t)))

(def hex-data (base64-to-bits (filter #(not= % \newline) (slurp "base64.txt"))))

(defn edit-dist [ksz s]
  (let [byts  (* ksz 8)
        as    (take byts s)
        bs    (take byts (drop byts s))]
    (/ (hamming-distance-bits as bs) (float ksz))))

(def probables (take 10
                        (map second 
                          (sort-by first 
                             (map (fn [x] [(edit-dist x hex-data) x]) 
                                  (range 3 41))))))

(defn blocks [n data]
  (loop [es     (vec (take n (cycle [[]])))
         ix     0
         d      data]
    (if (empty? d)
        es
        (let [vi (rem ix n)
            v  (es vi)]
          (recur (assoc es vi (conj v (first d)))
              (+ 1 ix)
              (rest d))))))
 
(defn blocks-of-hex [ksz data]
  (let [dp    (partition 8 data)
        blks  (blocks ksz dp)
        hx    (->> blks   (map flatten)
                          (map bits-to-hex))]
    (vec hx)))

(def blk-sz 29)     ; found by experimentation using the above codes

(def h (blocks-of-hex blk-sz hex-data))
(def pieces (map #(first (has-candidate (h %))) (range 0 29)))

(println (apply str 
                (apply interleave pieces)))
