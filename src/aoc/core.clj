(ns aoc.core
  (:require [clojure.java.io :as io]))

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
   (map f (clojure.string/split-lines (slurp (io/resource file))))))

;; day one

(defn day-one
  [input]
  (->> (partition 2 1 input)
       (filter #(> (second %) (first %)))
       (count)))

(day-one day-one-input)

(defn day-one-part-two
  [input]
  (->> (partition 3 1 input)
       (map #(reduce + %))
       (day-one)))

(def day-one-input
  (read-input "one.txt" #(Integer/parseInt %)))

(day-one-part-two day-one-input)

;; day two

(def commands
  {:forward #(vector (first %1) (+ %2 (second %1)))
   :down #(vector (+ %2 (first %1)) (second %1))
   :up  #(vector (- (first %1) %2) (second %1))})

(defn day-two
  [input]
  (reduce *
          (reduce
           (fn [coord command]
             (((first command) commands) coord (second command)))
           '(0 0) input)))

(def day-two-input
  (read-input "two.txt" #(let [split (clojure.string/split % #" ")]
                           [(keyword (first split)) (Integer/parseInt (second split))])))

(day-two day-two-input)
