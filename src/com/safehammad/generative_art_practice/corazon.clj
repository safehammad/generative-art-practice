(ns com.safehammad.generative-art-practice.corazon
  (:require [quil.core :as q]
            [quil.middleware :as m]
            [com.safehammad.generative-art-practice.utils :as utils]))

(comment
  (require '[portal.api :as p])
  (def p (p/open)) ; Open a new inspector
  (add-tap #'p/submit) ; Add portal as a tap> target
  )

;; Inspect state used to generate image
(def debug (atom nil))

(defn setup []
  ;; Visuals
  (q/color-mode :hsb 360 100 100 1.0)
  (q/frame-rate 10)
  [])

(defn fill-by-prob
  "Put an increasing number of each item in the bag i.e 1 of first, 2 of second etc."
  [coll]
  (mapcat #(repeat %1 %2) (iterate inc 1) coll))

(defn take-fraction
  "Given a collection `coll` take the first `1/denom` of all elements."
  [denom coll]
  (take (quot (count coll) denom) coll))

(defn next-visit [points [x y :as point]]
  (let [other-points (remove #{point} points)
        points-by-dist (sort-by (fn [[x' y']]
                                  (q/dist x y x' y'))
                                other-points)]
    (utils/q-rand-nth (fill-by-prob (reverse (take-fraction 4 points-by-dist))))))

(defn update-state [_state]
  (q/random-seed 26)
  (let [points (repeatedly 45 #(vector (q/random -3000 3000) (q/random -3000 3000)))
        visited (take 12000 (iterate (partial next-visit points) (first points)))
        state {:visited-points visited}]
    (reset! debug state)))

(defn shift
  "Shift the given value `m` by a random gaussian amount with std dev `sd`."
  [sd m]
  (+ m (utils/gaussian-range (/ sd 3))))

(defn diff [a b]
  (abs (- a b)))

;; Drawing
(defn draw [{:keys [visited-points] :as _state}]
  ;; Conventional coords with each axis -5000 to 5000
  (let [factor (/ (min (q/width) (q/height)) 10000)]
    (q/scale factor (- factor))
    (q/translate (/ (q/width) 2 factor) (/ (q/height) -2 factor)))
  (q/background 120 50 30 1)

  ;; Dots
  (q/stroke 120 30 60 1)
  (q/stroke-weight 2)

  ;; Lines
  (q/no-fill)
  (doseq [[[x1 y1] [x2 y2]] (partition 2 1 visited-points)
          :let [dist-x (diff x1 x2)
                dist-y (diff y1 y2)
                dist (q/dist x1 y1 x2 y2)]]
    (q/stroke-weight (+ 0 (/ (* dist dist dist) 2500000000)))
    (q/stroke (+ 60 (utils/gaussian-range 30)) (/ (* dist dist dist) 95000000) 100 1)
    (q/bezier x1 y1  ; First anchor
              (shift dist-y x1) (shift dist-x y1)  ; First control
              (shift dist-y x2) (shift dist-x y2)  ; Second control
              x2 y2))  ; Second anchor

  ;; Uncomment the following form to generate PNG. Consider increasing size first (see below).
  #_(q/save-frame "images/corazon.png"))

#_{:clj-kondo/ignore [:unresolved-symbol]}
(q/defsketch corazon
  :title "Corazon"
  :settings #(q/smooth 8)
  :setup setup
  :draw draw
  :update update-state
  :middleware [m/fun-mode]
  :size [1000, 1000])
