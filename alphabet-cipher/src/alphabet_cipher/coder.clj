(ns alphabet-cipher.coder)

(defn- index-alpha [index]
  (char (+ 97 index)))

(defn- alpha-index [letter]
  (- (int letter) 97))

(defn- compute-letter [a b f]
  (-> (f (alpha-index a) (alpha-index b))
      (mod 26)
      (index-alpha)))

(defn- encode-letter [[k-letter m-letter]]
  (compute-letter k-letter m-letter +))

(defn- decode-letter [[k-letter m-letter]]
  (compute-letter m-letter k-letter -))

(defn- decipher-letter [[e-letter m-letter]]
  (compute-letter e-letter m-letter -))

(defn- map-paired-letters [f keyword message]
  (->> (map vector (cycle keyword) message)
       (map f)
       (apply str)))

(defn- halt-cycle [keyword-cycle pred]
  (loop [i 1]
    (let [possible-keyword (apply str (take i keyword-cycle))]
      (if (pred possible-keyword)
        possible-keyword
        (recur (inc i))))))

(defn encode [keyword message]
  (map-paired-letters encode-letter keyword message))

(defn decode [keyword message]
  (map-paired-letters decode-letter keyword message))

(defn decipher [encrypted-message message]
  (-> (map-paired-letters decipher-letter encrypted-message message)
      (halt-cycle (fn [possible-keyword]
                      (= message (decode possible-keyword encrypted-message))))))
