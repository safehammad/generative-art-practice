(ns com.safehammad.generative-art-practice.utils
  (:require [clojure.math :refer [sin cos] :as math]
            [quil.core :as q]))

(defn append-first
  "Lazily append the first element of a collection into the collection.

  For example:

  (append-first [1 2 3]) => (1 2 3 1)"
  [coll]
  (concat coll [(first coll)]))

(defn halton-at
  "Calculate the number in the Halton sequence at the given `index` for the given `base`.

  Implementation taken from https://observablehq.com/@jrus/halton and converted to clojure:

  halton = function halton (index, base) {
    let fraction = 1;
    let result = 0;
    while (index > 0) {
      fraction /= base;
      result += fraction * (index % base);
      index = ~~(index / base); // floor division
    }
    return result;
  }"
  [index base]
  (loop [fraction 1
         result   0
         index    index]
    (if (pos? index)
      (let [fraction (/ fraction base)]
        (recur fraction
               (+ result (* fraction (mod index base)))
               (quot index base)))
      result)))

(defn halton-seq
  "Generate the Halton sequence for the given `base` starting at index 1."
  [base]
  (map #(halton-at % base) (iterate inc 1)))

(defn halton-coord-seq
  "Generate halton coords for the given `bases`.

  Each coord generated is a vector of length n where n is the number of `bases` with each
  element in the vector corresponding to the Halton seqence for each base respectively."
  [& bases]
  (apply map vector (map halton-seq bases)))

(defn polar->cartesian
  "Translate polar coordinates (angle in radians, dist from centre) to cartesian (x, y) coordinates."
  [angle dist]
  [(* (cos angle) dist) (* (sin angle) dist)])

(defn create-quad
  "Create quad as sequence of 2-vector coords centered on `x`, `y` with width `w` and height `h`."
  [x y w h]
  (let [half-w (* w 0.5)
        half-h (* h 0.5)
        offsets [[-1 -1] [1 -1] [1 1] [-1 1]]]
    (mapv (fn [[m n]] [(+ x (* m half-w)) (+ y (* n half-h))]) offsets)))

(defn filtered-random-gaussian
  "Return q/random-gaussian returning only values for which `(pred value)` returns logical `true`."
  [pred]
  (loop [result (q/random-gaussian)]
    (if (pred result)
      result
      (recur (q/random-gaussian)))))

(defn capped-random-gaussian
  "Return q/random-gaussian throwing away any values where (abs value)` is greated than `cap`."
  [cap]
  (filtered-random-gaussian #(<= (abs %) cap)))

(defn jitter
  "Shift x and y in collection of coords by `dist` pixels * random gaussian number capped at +/- 3."
  [dist coll]
  (mapv (fn [coord] (mapv #(+ (* dist (capped-random-gaussian 3)) %) coord)) coll))

(defn line-coords
  "Convert a collection of vertices to a collection of vertices suitable for creating a set of lines.

  Note that the final line closes the shape by default or if `close?` is true. For example:

  (line-coords [p1 p2 p3 p4]) => [p1 p2 p2 p3 p3 p4 p4 p1]"
  ([coll]
   (line-coords coll true))
  ([coll close?]
   (let [points (if close? (append-first coll) coll)]
     (sequence cat (partition 2 1 points)))))
