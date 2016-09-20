(ns dog-and-gopher.core
  (:require [clojure.string :as str])
  (:gen-class))

(defn get-input
  "Returns input from a file"
  [file-name]
  (with-open [reader (clojure.java.io/reader file-name)]
    (map #(str/split % #" ") (reduce conj [] (line-seq reader)))))

(defn make-hole
  [x y]
  {:x (Double. x)
   :y (Double. y)})

(defn dist
  [pt1 pt2]
  (let [{x1 :x y1 :y} pt1
        {x2 :x y2 :y} pt2
        dist-x  (- x1 x2)
        dist-y  (- y1 y2)]
   (Math/sqrt (+ (* dist-x dist-x) (* dist-y dist-y)))))

(defn parse-data
  "num-holes gopherX gopherY dogX dogY
   hole1X hole1Y
   hole2X hole2Y
   ... etc ...
   [blank line]
   [repeat for each set]"
  [lines]
  (->>
    lines
    (partition-by #(= % [""]))
    (remove #(= % '([""])))
    (map (fn [lines]
           (let [[num-holes gopher-x gopher-y dog-x dog-y] (first lines)
                 curr-data-set  (->>
                                  lines
                                  (drop 1)
                                  (take (Integer. num-holes)))
                 holes          (map #(make-hole (first %) (second %)) curr-data-set)]
             {:gopher (make-hole gopher-x gopher-y)
              :dog    (make-hole dog-x dog-y)
              :holes  holes})))))

(defn str-exit
  [hole]
  (let [str-x (format "%.3f" (:x hole))
        str-y (format "%.3f" (:y hole))]
    (str "The gopher can escape at hole " str-x ", " str-y)))

(defn str-no-exit
  []
  (str "The gopher has no escape D:"))

(defn calc-algorithm
  [data-sets]
  (map
    (fn [data]
      (let [{gopher     :gopher
             dog        :dog
             holes      :holes} data
             exit-hole  (first (filter #(> (dist dog %) (* 2 (dist gopher %))) holes))]
        (if exit-hole
          (str-exit exit-hole)
          (str-no-exit))))
    data-sets))

(defn -main
  "Dog and Gopher"
  [& args]
  (let [lines  (get-input "input.txt")
        data   (parse-data lines)
        answer (calc-algorithm data)]
    (map println answer)))
