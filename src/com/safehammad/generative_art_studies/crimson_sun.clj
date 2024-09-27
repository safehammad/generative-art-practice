(ns com.safehammad.generative-art-studies.crimson-sun
  (:require [clojure.math :as math :refer [PI sin cos]]
            [clojure.math.combinatorics :as combo]
            [com.safehammad.generative-art-studies.utils :as utils]
            [quil.core :as q]
            [quil.middleware :as m]))

;; Inspect state used to generate image
(def debug (atom nil))

(defn setup []
  ;; Redraw every second to show result of re-evaluated changes
  (q/frame-rate 1)
  ;; Use hue, saturation, brightness and alpha to define colours
  (q/color-mode :hsb 360 100 100 1.0))

(defn sign-combos
  "Return set of sign combinations (-1, 0, 1) of a given length.

  For example:
  => (sign-combos 2) ;; ((-1 -1) (-1 0) (-1 1) (0 -1) (0 0) (0 1) (1 -1) (1 0) (1 1))
  => (sign-combos 3) ;; ((-1 -1 -1) (-1 -1 0) (-1 -1 1) ..."
  [len]
  (apply combo/cartesian-product (repeat len [-1 0 1])))

(defn halton-jitter
  "Return a sequence of vectors of length `len` that jitter around [0 0] with a halton sequence."
  [len halton-base]
  (map (fn [haltons signs] (mapv #(* %1 %2) haltons signs))
       (apply utils/halton-coord-seq (take len (iterate inc halton-base)))
       (cycle (sign-combos len))))

(defn jitter-triangle
  [halton-base jitter-amount {coords :coords [hue sat bri _alpha] :colour dist :dist}]
  (take 12 (for [factors (halton-jitter 6 halton-base)
                 :let [shifts (map (partial * jitter-amount) factors)]]
             {:coords (map + coords shifts)
              :dist dist
              :colour [hue sat bri (+ (/ 1 3) (abs (/ (first factors) 3)))]})))

(defn make-triangle [x y base angle dist]
  (let [arm-angle-1 (+ angle (/ PI 6) dist)
        arm-angle-2 (- angle (/ PI 6) dist)]
    [x y
     (+ x (* (cos arm-angle-1) base)) (+ y (* (sin arm-angle-1) base))
     (+ x (* (cos arm-angle-2) base)) (+ y (* (sin arm-angle-2) base))]))

(defn triangles []
  (for [[h1 h2] (take 5000 (utils/halton-coord-seq 8 19))
        :let [angle (* h1 2 PI)
              dist (+ 250 (* h2 3250))  ; dist from centre
              [x y] (utils/polar->cartesian angle dist)
              triangle (make-triangle x y (- 135 (/ 40000 dist)) (- angle PI) dist)]
        :when (> 650 (mod dist 950))]
    {:coords triangle :dist dist}))

(defn colour-triangles
  "Apply colour to triangles based on index and distance from centre."
  [triangles]
  (let [props [[0 750 (fn [dist] [0 100 (+ 30 (int (* (/ dist 4000) 100))) 1])]
               [750 750 (fn [dist] [210 100 (+ 40 (int (* (/ dist 8000) 100))) 1])]
               [1500 750 (fn [dist] [0 100 (- 90 (int (* (/ dist 4000) 100))) 1])]
               [2250 750 (fn [dist] [40 20 (+ 40 (int (* (/ dist 6000) 100))) 1])]]]
    (sequence cat (for [[from to colour-fn] props]
                    (sequence
                     (comp
                      (drop from)
                      (take to)
                      (map #(assoc % :colour (colour-fn (:dist %)))))
                     triangles)))))

(defn make-triangles []
  (mapcat #(jitter-triangle 3 %1 %2) (cycle [0 2 4]) (colour-triangles (triangles))))

(defn jitter-circle
  "Explode a single circle to multiple circles, jittering location, size and alpha."
  [halton-base jitter-amount {[x y width height] :coords [hue sat bri _alpha] :colour}]
  (take 12 (for [[x-factor y-factor] (halton-jitter 2 halton-base)
                 :let [x-shift (* jitter-amount x-factor)
                       y-shift (* jitter-amount y-factor)]]
             {:coords [(+ x x-shift) (+ y y-shift) width height]
              :colour [hue sat bri (abs (/ x-factor 3))]})))

(defn make-dots []
  (let [props [[28 790 1 2 [0 0 90 1]]  ; dot-count, dist, start angle, rotation colour
               [56 1750 3 (/ PI 4) [210 100 90 1]]
               [48 2695 4 (/ PI 4) [0 100 90 1]]]]
    (sequence cat (for [[dot-count dist start-angle rotation colour] props
                        angle (range start-angle (+ start-angle rotation) (/ (* 2 PI) dot-count))
                        :let [angle2 (+ angle PI)
                              x (* dist (cos angle))
                              y (* dist (sin angle))
                              x2 (* dist (cos angle2))
                              y2 (* dist (sin angle2))
                              width (+ 40 (* angle 15))
                              height (+ 40 (* angle 15))]]
                    (sequence cat [(jitter-circle (int (* angle 4)) 10 {:coords [x y width height]
                                                                        :colour colour})
                                   (jitter-circle (int (* angle 3)) 10 {:coords [x2 y2 width height]
                                                                        :colour colour})])))))

(defn make-circles []
  (let [props [[4 200 [0 0 7250 7250] [40 100 70 1]]   ; halton-base, jitter, coords, colour
               [3 150 [0 0 7050 7050] [15 100 60 1]]
               [3 150 [0 0 6450 6450] [0 100 50 1]]]]
    (sequence cat (for [[halton-base jitter coords colour] props]
                    (jitter-circle halton-base jitter {:coords coords :colour colour})))))

(defn update-state [_state]
  (let [triangles (make-triangles)
        circles (make-dots)
        large-circles (make-circles)]
    (reset! debug {:triangles triangles
                   :circles circles
                   :large-circles large-circles})))

;; Drawing

(defn draw [{:keys [triangles circles large-circles]}]
  ;; Conventional coords -5000 to +5000
  (let [factor (/ (min (q/width) (q/height)) 10000)]
    (q/scale factor (- factor))
    (q/translate (/ (q/width) 2 factor) (/ (q/height) -2 factor)))

  ;; Clear
  (q/background 0 90 30 1)
  (q/no-stroke)

  (doseq [{:keys [coords colour]} large-circles]
    (apply q/fill colour)
    (apply q/ellipse coords))

  (doseq [{:keys [coords colour]} triangles]
    (apply q/fill colour)
    (apply q/triangle coords))

  (doseq [{:keys [coords colour]} circles]
    (apply q/fill colour)
    (apply q/ellipse coords))

  ;; Uncomment the following form to generate PNG. Consider increasing size first (see below).
  #_(q/save-frame "images/crimson-sun.png"))

#_{:clj-kondo/ignore [:clojure-lsp/unused-public-var]}
(q/defsketch crimson-sun
  :title "Crimson Sun"
  :settings #(q/smooth 8)
  :setup setup
  :draw draw
  :update update-state
  :middleware [m/fun-mode]
  :size [1000 1000]
  ;:size [10000 10000]  ; Use this size when uncommenting "save-frame" (see above) to generate high res image
  )
