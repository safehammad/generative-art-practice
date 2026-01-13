(ns com.safehammad.generative-art-practice.spawn
  (:require [quil.core :as q]
            [quil.middleware :as m]
            [clojure.math :as math :refer [cos pow sin sqrt]]
            [com.safehammad.generative-art-practice.utils :as utils]))

(defn setup []
  (q/color-mode :hsb 360 100 100 1.0)
  (q/frame-rate 1)
  (q/ellipse-mode :radius)

  {:row-step 2.75
   :col-step 2.75})

(defn update-state [_state]
  {:row-step 2.75
   :col-step 2.75})

(defn angle
  "Determine the angle of a shape given its `row` and `col` and based on total `rows` and `cols`."
  [row col rows cols]
  (let [col-score (q/map-range col 0 cols -20 20)
        row-score (q/map-range row 0 rows -20 20)
        angle (* (- (q/noise col-score row-score) 0.5) 0.2)]
    angle))

(defn side-len [row col rows cols]
  (let [col-shift 40
        row-shift -40
        col-score (q/map-range col col-shift (+ cols col-shift) 0 8)
        row-score (q/map-range row row-shift (+ rows row-shift) 0 11)]
    (* (pow (q/noise col-score row-score) 1.6) 5)))

(defn etch [row col side a]
  (let [row2 (+ row (* (sin a) side))
        col2 (+ col (* (cos a) side))
        row3 (+ row2 (* (sin (+ a q/HALF-PI)) side))
        col3 (+ col2 (* (cos (+ a q/HALF-PI)) side))
        row4 (+ row (* (sin (+ a q/HALF-PI)) side))
        col4 (+ col (* (cos (+ a q/HALF-PI)) side))]
    [[row col]
     [row2 col2]
     [row3 col3]
     [row4 col4]]))

(defn draw [state]

  (q/random-seed 5)
  (q/background 220 80 30 1)
  (q/no-fill)

  ;; Conventional coords
  (q/scale 1, -1)
  (q/translate (/ (q/width) 2) (/ (q/height) -2))

  ;; Flow pins
  (let [[rows cols] [100 100]
        col-height (/ (q/width) cols)
        row-width (/ (q/height) rows)]

    (doseq [row (range -50 50 (:row-step state))
            col (range -50 50 (:col-step state))]
      (let [side (side-len row col rows cols)
            half-side (/ side 2)
            quarter-side (/ side 4)
            a (angle row col rows cols)
            ;; Shift from regular location
            row (+ row (q/random -0.7 0.7))
            col (+ col (q/random -0.7 0.7))
            coords (etch row col side a)
            half-coords (etch (+ row quarter-side) (+ col quarter-side) half-side a)]

        (q/stroke-weight (* side 0.5 (sqrt side)))

        (doseq [[color jittered-coords] (map vector
                                             (apply concat (repeatedly #(utils/q-rand-nth [(repeat 35 :graded) (repeat 45 :random)])))
                                             (map #(utils/jitter (* side %) 1 (utils/q-rand-nth [coords half-coords half-coords]))
                                                  (range 0.01 0.15 0.002)))
                :let [stroke (case color
                               :graded [(- (* 270 (- side 0.7)) 200) (+ 40 (* 35 (- side 0.5))) 100 1]
                               :random [(q/random 0 360) (utils/q-rand-nth [0 0 100]) 100 1])]]

          ;; Draw squares
          (apply q/stroke stroke)
          (q/begin-shape :quads)
          (doseq [[row col] jittered-coords]
            (q/vertex (* col col-height) (* row row-width)))
          (q/end-shape)))))

  #_(q/save-frame "images/spawn.png"))

#_{:clj-kondo/ignore [:unresolved-symbol]}
(q/defsketch Spawn
  :title "Spawn"
  :settings #(q/smooth 8)
  :setup setup
  :draw draw
  :update update-state
  :middleware [m/fun-mode]
  :size [3900, 3900])  ; Required size, as (ashamedly) the piece changes according to size :(
