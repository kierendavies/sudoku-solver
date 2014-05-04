(require 'clojure.pprint)

(defn no-dups? [line]
  (let [non-zeros (filter (complement zero?) line)]
    (= (count non-zeros)
       (count (distinct non-zeros)))))

(defn rows [grid]
  grid)

(defn cols [grid]
  (map (fn [c] (map #(nth % c) grid))
       (range 9)))

(def box-corners
  (for [r (range 0 9 3)
        c (range 0 9 3)]
    [r c]))

(def box-deltas
  (for [dr (range 3)
        dc (range 3)]
    [dr dc]))

(defn boxs [grid]
  (map (fn [[r c]]
         (map (fn [[dr dc]]
                (nth (nth grid (+ r dr)) (+ c dc)))
              box-deltas))
       box-corners))

(defn valid? [grid]
  (every? no-dups? (lazy-cat (rows grid)
                             (cols grid)
                             (boxs grid))))

(defn full? [grid]
  (not-any? (partial some zero?) grid))

(defn first-empty [grid]
  (let [r (.indexOf (map (partial some zero?) grid) true)
        c (.indexOf (nth grid r) 0)]
    [r c]))

(defn insert [grid pos value]
  (let [[ir ic] pos]
    (vec (map (fn [r]
           (if (= r ir)
             (vec (map (fn [c]
                    (if (= c ic)
                      value
                      (nth (nth grid r) c)))
                  (range 9)))
             (nth grid r)))
         (range 9)))))

(defn solution [grid]
  (cond
   (not (valid? grid)) nil
   (full? grid) grid
   :else (let [pos (first-empty grid)]
           (some #(solution (insert grid pos %)) (range 1 10)))))

(def _ 0)

(clojure.pprint/pprint
 (solution [[ 5 3 _   _ 7 _   _ _ _ ]
            [ 6 _ _   1 9 5   _ _ _ ]
            [ _ 9 8   _ _ _   _ 6 _ ]

            [ 8 _ _   _ 6 _   _ _ 3 ]
            [ 4 _ _   8 _ 3   _ _ 1 ]
            [ 7 _ _   _ 2 _   _ _ 6 ]

            [ _ 6 _   _ _ _   2 8 _ ]
            [ _ _ _   4 1 9   _ _ 5 ]
            [ _ _ _   _ 8 _   _ 7 9 ]]))
