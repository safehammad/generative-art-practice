(ns com.safehammad.generative-art-practice.square-wave
  (:require [quil.core :as q]
            [quil.helpers.tools :as qtools]
            [quil.middleware :as m]
            [clojure.math :as math :refer [PI sin]]
            [com.safehammad.generative-art-practice.utils :as utils]))

(comment
  (require '[portal.api :as p])
  (require '[portal.console :as log])
  (def p (p/open)) ; Open a new inspector
  (add-tap #'p/submit) ; Add portal as a tap> target
  (log/trace {:foo [1 2 3]}))

;; Inspect state used to generate image
(def debug (atom nil))
;; Height between rows
(def k 136)
;; Square dist from centre to corner
(def max-dist-from-center (* 2 4000 4000))

(defn within-bounds
  "Keep coords within 4000 pixel square with ragged edges to the right."
  [{[x y] :coord}]
  (and (< x (+ 3875 (q/random 300))) (< y 4000)))

(defn z-ify
  "Draw z shape.

      ___
     |
  ___|


  h = left and right horizontal width
  v = vertical height"
  [x y horiz vert]
  (utils/line-coords [[x y]
                      [x (- y vert)]
                      [(+ x horiz) (- y vert)]
                      [(+ x horiz) y]
                      [(+ x horiz horiz) y]]
                     false))

(defn line-color
  "Given a line location, determine colour.

  Return [hue sat bri alpha]."
  [[x y]]
  (let [dist-from-center (+ (* x x) (* y y))
        dist-ratio (/ dist-from-center max-dist-from-center)
        hue (- 155 (* 180 dist-ratio))
        sat (+ 50 (* 50 (abs (- 0.5 dist-ratio))))
        bri (+ 55 (abs (- 60 (* 275 dist-ratio))))]
    [hue sat bri 1]))

(defn next-z
  "Given the previous z coords, determine the coords for the next z connected to the right."
  [coords]
  (let [[x y] (:coord (last coords))
        mag (/ (abs x) 8000)
        period (* 2 (sin (* PI (/ (abs y) 4000))))
        horiz (+ 48 (* 96 (abs (sin (* period PI mag)))))]
    (map #(hash-map :coord %
                    :colour (line-color %)) (z-ify x y horiz k))))

(defn create-state
  "Main function to create the state of the drawing."
  [_state]
  (for [row-y (range -4000 4000 (* 1.75 k))]
    (take-while within-bounds
                (sequence cat  ; combine z's, vector per row
                          (rest (iterate next-z [{:coord [-4000 row-y]}]))))))  ; vectors of z's

(defn setup []
  ;; Visuals
  (q/color-mode :hsb 360 100 100 1.0)
  (q/frame-rate 1)

  ;; State
  (create-state []))

(defn update-state [state]
  (reset! debug (create-state state)))

;; Drawing
(defn draw [state]
  ;; Conventional coords
  (let [factor (/ (min (q/width) (q/height)) 10000)]
    (q/scale factor (- factor))
    (q/translate (/ (q/width) 2 factor) (/ (q/height) -2 factor)))

  (q/random-seed 10)

  (q/background 260 100 50 1)
  (q/stroke-weight 24)
  (q/no-fill)

  ;; Start at the bottom and work up row by row
  (doseq [row state]
    (qtools/with-shape :lines
      (doseq [{:keys [coord colour]} row]
        (apply q/stroke colour)
        (apply q/vertex coord))))

  ;; Uncomment the following form to generate PNG. Consider increasing size first (see below).
  #_(q/save-frame "images/square-wave.png"))

#_{:clj-kondo/ignore [:unresolved-symbol]}
(q/defsketch square-wave
  :title "Square Wave"
  :settings #(q/smooth 8)
  :setup setup
  :draw draw
  :update update-state
  :middleware [m/fun-mode]
  :size [1000 1000]
  ;:size [3900, 3900]  ; Use this size when uncommenting "save-frame" (see above) to generate high res image
  )
