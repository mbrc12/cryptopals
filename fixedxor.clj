(load-file "base64.clj")

(defn fixed-xor [hex1 hex2] 
  (let [bit1 (hex-to-bits hex1)
        bit2 (hex-to-bits hex2)]
       (bits-to-hex (map bit-xor bit1 bit2))))
