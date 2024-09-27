(ns com.safehammad.generative-art-studies.utils
  (:require [clojure.math :refer [sin cos] :as math]))

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
