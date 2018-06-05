

(def zero (int \0))
(def nine (int \9))
(def a-char (int \a))
(def z-char (int \z))
(def A-char (int \A))
(def Z-char (int \Z))

(defn bit-pos [x pos] 
  (if (bit-test x pos)
    1 0))

(defn hex-char-to-dec [x] 
  (let [y (int x)]
    (if (and (>= y zero) (<= y nine))
      x
      (char (+ (+ zero 10) (- y a-char))))))

(defn char-to-int [x]
  (let [y (int x)]
    (- y zero)))

(defn base64-elem-to-bits [c]
  (map #(bit-pos c %) [5 4 3 2 1 0]))

(defn hex-to-bits [hex] 
  (let [ h2bs   (fn [c] 
                  (map #(bit-pos c %) [3 2 1 0])) 
        ]
    (flatten (map (comp h2bs char-to-int hex-char-to-dec)  hex))))

(defn bits-to-int [elems]
  ;; takes a collection of numbers x y z .. and produces x + y * 2 + z * 4 + ..
  (apply + (map (fn [x y] 
            (* x (bit-shift-left 1 y))) elems (range 0 10))))

(defn base64-to-char [z]
  (cond
    (and (>= z 0 ) (<= z 25)) (char (+ z A-char))
    (and (>= z 26) (<= z 51)) (char (+ (- z 26) a-char))
    (and (>= z 52) (<= z 61)) (char (+ (- z 52) zero))
    (= z 62)                    \+
    (= z 63)                    \/
   )) 

(def b64-map 
  (apply assoc {} \= 0
         (interleave 
           (map base64-to-char (range 0 64)) (range 0 64))))

(defn base64-to-bits [b]
  (->> b  (map #(b64-map %))
          (map base64-elem-to-bits)
          (flatten)))
          

(defn hex-to-char [z]
  (cond
    (and (>= z 0) (<= z 9)) (char (+ z zero))
    (>= z 10)               (char (+ (- z 10) a-char)))) 

(defn bits-to-base64 [bits]
  (->> bits (reverse)
            (partition-all 6)
            (map bits-to-int)
            (reverse)
            (map base64-to-char)
            (apply str)))

(defn bits-to-hex [bits]
  (->> bits (reverse)
            (partition-all 4)
            (map bits-to-int)
            (reverse)
            (map hex-to-char)
            (apply str)))

(defn hex-to-str [hex]
  (->> hex  (hex-to-bits)
            (reverse)
            (partition-all 8)
            (map bits-to-int)
            (reverse) 
            (map char)
            (apply str)))

(def hex-to-base64 (comp bits-to-base64 hex-to-bits))
