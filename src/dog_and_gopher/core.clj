(ns dog-and-gopher.core
  (require '[clojure.string :as str])
  (:gen-class))

(defn get-input
  "Returns input from a file"
  [file-name]
  (with-open [reader (clojure.java.io/reader file-name)]
    (reduce conj '() (line-seq reader))))

(defn parse-data
  "Data is formatted like this:
  num-holes gopherX gopherY dogX dogY
  hole1X hole1Y
  hole2X hole2Y
  ... etc ...
  [blank line]
  [repeat for each set]"
  [lines]
  (->> 
    (partition-by #(= % ""))
    (remove #(= % ""))
    (map (fn [lines] (let [[num-holes gopher-x gopher-y dog-x dog-y] (str/split (first lines) #" ")
                           curr-data-set  (->>
                                            lines
                                            (drop 1)
                                            (take (Integer. num-holes)))
                           holes          (map make-holes curr-data-set)]
      {:gopher (make-hole gopher-x gopher-y)
       :dog    (make-hole dog-x dog-y)
       :holes  holes}))))

(defn calc-algorithm
  []
  "")

(defn -main
  "Dog and Gopher"
  [& args]
  (let [lines  (get-input "input.txt")
        data   (parse-data lines)
        answer (calc-algorithm data)]
    (map println answer)))
