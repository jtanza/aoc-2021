(ns aoc.core
  (:require [clojure.java.io :as io]
            [clojure.string :as string]))

(defn -main
  "Saves christmas"
  [& args]
  (println "merry xmas"))

(defn read-input
  "Splits `file` on newlines, optionally applying `f`` to each element in
  the resulting collection if it has been supplied."
  ([file]
   (read-input file #(map identity %)))
  ([file f]
   (map f (string/split-lines (slurp (io/resource file))))))

;; day one

(defn day-one
  [input]
  (->> (partition 2 1 input)
       (filter #(> (second %) (first %)))
       (count)))

(def day-one-input
  (read-input "one.txt" #(Integer/parseInt %)))

(day-one day-one-input)

(defn day-one-part-two
  [input]
  (->> (partition 3 1 input)
       (map #(reduce + %))
       (day-one)))

(day-one-part-two day-one-input)

;; day two

(def commands
  {:forward #(vector (first %1) (+ %2 (second %1)))
   :down #(vector (+ %2 (first %1)) (second %1))
   :up  #(vector (- (first %1) %2) (second %1))})

(defn day-two
  [commands seed input]
   (reduce (fn [coord command]
             (((first command) commands) coord (second command)))
           seed input))

(def day-two-input
  (read-input "two.txt" #(let [split (string/split % #" ")]
                           [(keyword (first split)) (Integer/parseInt (second split))])))

(reduce * (day-two commands [0 0] day-two-input))

(def commands-part-two
  {:forward #(vector (first %1) (+ (* %2 (first %1)) (second %1)) (+ %2 (nth %1 2)))
   :down #(vector (+ %2 (first %1)) (second %1) (nth %1 2))
   :up  #(vector (- (first %1) %2) (second %1) (nth %1 2))})

(->> (day-two commands-part-two [0 0 0] day-two-input)
     (rest)
     (reduce *))
