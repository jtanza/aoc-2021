(ns aoc.core
  (:require [clojure.java.io :as io]
            [clojure.string :as string]
            [clojure.string :as str]))

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


;; day three

(defn most-freq-bits
  "Finds the most frequent bit in a collection of bits at each 0...nth position,
  resulting in an n length bit sequence."
  [bits]
  (let [partitioned-bits (partition (count bits) (apply interleave bits))]
    (map (fn [partitioned] (first (apply max-key val (frequencies partitioned))))
         partitioned-bits)))

(defn day-three
  [input]
  (let [bits (most-freq-bits input)
        gamma (Integer/parseInt (string/join bits) 2)
        epsilon (bit-and-not 0xFFF gamma)] ;; complement and keep 12 least significant bits
    (* gamma epsilon)))

(day-three (read-input "three.txt"))

