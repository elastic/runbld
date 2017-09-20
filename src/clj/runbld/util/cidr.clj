(ns runbld.util.cidr
  "From:
   github.com/zentrope/match-expr/blob/master/src/match_expr/impl/cidr.clj"
  (:require
   [clojure.string]))

(defn expt
  [x pow]
  (apply * (repeat pow x)))

(defn shft
  [num to]
  (bit-shift-right (bit-and num (bit-shift-left 0xff to)) to))

(defn ip->num
  [[a b c d]]
  (+ (* a (expt 256 3))
     (* b (expt 256 2))
     (* c 256)
     d))

(defn num->ip
  [num]
  [(shft num 24)
   (shft num 16)
   (shft num 8)
   (bit-and num 0xFF)])

(defn cidr->ips
  [a b c d len]
  (let [nw (ip->num [a b c d])
        mask (bit-shift-left -1 (- 32 len))
        low (bit-and nw mask)
        high (+ low (bit-not mask))]
    [low high]))

(defn in-cidr-range?
  "In range? [cider] [ip]."
  [[a b c d len] [w x y z]]
  (let [[low high] (cidr->ips a b c d len)
        val (ip->num [w x y z])]
    (and (>= val low)
         (<= val high))))

(defn str->ip
  [s]
  (->> (clojure.string/split s #"[.]")
       (mapv #(Integer/parseInt %))))

(defn str->cidr
  [s]
  (let [parts (clojure.string/split s #"/")
        quads (clojure.string/split (first parts) #"[.]")
        quads (take 4 (apply conj quads ["0" "0" "0" "0"]))]
    (->> (conj (vec quads) (second parts))
         (mapv #(Integer/parseInt %)))))

(defn in-range?
  [ip-str cidr-str]
  (let [ip (str->ip ip-str)
        cidr (str->cidr cidr-str)]
    (in-cidr-range? cidr ip)))
