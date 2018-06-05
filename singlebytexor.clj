(load-file "base64.clj")
(require '[clojure.java.io :as io])
(require '[clojure.string :as string])
(import 'java.lang.Math)

(defn printv [x] 
  (println x)
  x)

(defn is-alpha [c]
  (let [c (int c)]
    (or (and (>= c a-char) (<= c z-char)) (and (>= c A-char) (<= c Z-char)) (= c 32) (= c 10))))

(defn is-cap [c]
  (let [c (int c)]
    (and (>= c A-char) (<= c Z-char))))

(defn c2bs [c] 
  (let [c (int c)]
    (map #(bit-pos c %) [7 6 5 4 3 2 1 0])))

(defn is-alpha-or-unprintable [c]
  (nil? (#{\formfeed \# \%  \* \+ \, \- \/ \: \; \< \= \> \& \) \( \[ \] \_ \' \0 \1 \2 \3 \4 \5 \6 \7 \8 \9 \$ \! \\ \~ \@ \return \± \² \³ \´ \µ \¶ \· \¸ \¹ \º \» \¼ \½ \¾ \¿ \À \Á \Â \Ã \Ä \Å \Æ \Ç \È \É \Ê \Ë \Ì \Í \Î \Ï \Ð \Ñ \Ò \Ó \Ô \Õ \Ö \× \Ø \Ù \Ú \Û \Ü \Ý \Þ \ß \à \á \â \ã \ä \å \æ \ç \è \é \ê \ë \ì \í \î \ï \ð \ñ \ò \ó \ô \õ \ö \÷ \ø \ù \ú \û \ü \ý \þ \ÿ}
                   c)))

(defn is-okay [s]
  true)
  ;(every? is-alpha-or-unprintable s))

(defn char-range [a b]  ; a .. b including b
  (map char (range (int a) (+ 1 (int b)))))


(defn how-many [p xs & oth]
  (count
    (filter #(= % true)
            (apply map p xs oth))))

(defn sigmoid [x] 
  (/ 2.0 
     (+ 1 (Math/exp (- 1 x))))) 

(defn score-by-evenness [xs]    ; = number of positions such that xs[i] >= xs[i + 1]
  (apply * (map #(sigmoid (* %3 (/ (+ 1 %1) (+ 1 %2))) )
                xs (rest xs) (range 1 100))))

(defn freq-analysis [s]
  ; determine the frequency analysis score
  (let [s     (string/lower-case (apply str (filter is-alpha s)))
        freq  (merge 
                (apply assoc {} 
                       (interleave (char-range \a \z) (cycle [0]))) 
                (frequencies s))                                        ; contains even zero frequencies
        mapr  (map #(freq %) [\e \a \r \i \o \t \n \s \l \c \u \d \p \m \h \g \b \f \y \w \k \v \x \z \j \q]) ]
    (score-by-evenness mapr)))
        
(defn string-xor-char [s c]
  (let [n   (.length s)
        bit (flatten (map c2bs s))
        cbs (take (* n 8) (cycle (c2bs c))) ]
   (apply str 
     (map (comp char bits-to-int reverse) 
          (partition-all 8 
                         (map bit-xor bit cbs))))))

(defn str-to-hex [s]
  (->> s  (map c2bs)
          (flatten)
          (bits-to-hex)))


(defn gross-violation [c]
  (not (nil? (#{\( \) \: \; \^ \+ \- \/ \*} c))))


(defn non-spam [s]
  (/ (- (float (how-many is-alpha s)) (how-many gross-violation s) (how-many is-cap s))
     (.length s)))


(defn is-okay-spam [s]
  (>= (non-spam s) 0.82))

(def str-to-bits (comp hex-to-bits str-to-hex))


(defn candidate [s] 
  (let  [oks (filter is-okay-spam
                   (map #(string-xor-char s %)
                        (char-range (char 0) (char 255)) ))] 
      oks))

(defn lines-of-file [f]
  (line-seq (io/reader f)))

(defn has-candidate [x] 
  (let [s     (hex-to-str x)
        cand  (candidate s)]
    (vec cand)))

(defn decode-file [f]
  (let [ls  (lines-of-file f)]
    (filter first
        (map (fn [x] 
               [(has-candidate x) x]) ls))))
