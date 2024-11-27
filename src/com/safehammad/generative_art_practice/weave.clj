(ns com.safehammad.generative-art-practice.weave
  (:require [quil.core :as q]
            [quil.middleware :as m]
            [clojure.math :as math]))

;; Inspect state used to generate image
(def debug (atom nil))

(def directions (range 8))  ; A direction is a value from 0 to 7 following 45 degree increments around the compass.

(defn slant
  "Return 0 if direction is horizontal or vertical, and 1 if direction is diagonal."
  [direction]
  (mod direction 2))

(defn same-slant?
  "Return true if both directions are horizontal/vertical, or both directions are diagonal."
  [direction other-direction]
  (= (slant direction) (slant other-direction)))

(defn diagonal? [direction]
  (= 1 (slant direction)))

(defn direction->angle
  "Convert a direction (point of compass) from 0-7 to an angle in radians."
  [direction]
  (* q/QUARTER-PI direction))

(defn opposite-direction
  "Remove opposite direction."
  [direction]
  (mod (+ direction 4) 8))

(defn setup []
  ;; Visuals
  (q/color-mode :hsb 360 100 100 1.0)
  (q/frame-rate 10)
  [])

(defn at-edge?
  [x y]
  (let [limit 3300]
    (not (and (< (- limit) x limit)
              (< (- limit) y limit)))))

(defn in-bounds? [x y direction]
  (if (at-edge? x y)
    (let [limit (+ (q/random -100 100)
                   (if (diagonal? direction)
                     3400
                     3800))]
      (and
       (< (- limit) x limit)
       (< (- limit) y limit)))
    (and
     (even? (q/round (/ x 350)))
     (even? (q/round (/ y 350))))))

(defn q-rand-nth
  "A rand-nth implementation that uses quil random under the covers to respects the random seed."
  [coll]
  (nth coll (int (q/random (count coll)))))

(defn gaussian-range
  "Return a random gaussian float where the standard deviation is sd rather than the default 1."
  [sd]
  (* (q/random-gaussian) sd))

(defn next-direction
  "Given the previous position and direction, determine the next direction."
  [x y direction]
  (if (at-edge? x y)
    (opposite-direction direction)
    (let [possible-directions (->> directions
                                   (remove #{direction})
                                   (remove #{(opposite-direction direction)}))
          new-direction (q-rand-nth (apply concat
                                           possible-directions
                                           (repeat 2 (filter (partial same-slant? direction) possible-directions))))]
      new-direction)))

(defn next-line
  "Main function to create the state of the drawing."
  [{:keys [direction length line] :as current-line}]
  (let [[_ _ x y] line  ; end of previous line
        new-direction (next-direction x y direction)
        new-length (if (same-slant? direction new-direction)
                     length
                     (+ 50 (abs (gaussian-range 350))))
        angle (+ (direction->angle new-direction) (gaussian-range (/ 50)))
        new-x (+ (* (math/cos angle) new-length) x)
        new-y (+ (* (math/sin angle) new-length) y)
        stroke [(+ (/ new-length 14) (gaussian-range 15))
                90
                100
                0.7]
        stroke-weight (max 4 (+ 10 (q/random-gaussian) (if (diagonal? new-direction) -1 0)))]
    (if (in-bounds? new-x new-y new-direction)
      {:line [x y new-x new-y]
       :direction new-direction
       :length new-length
       :stroke stroke
       :stroke-weight stroke-weight}
      (recur current-line))))

(defn line-sort
  "Sort line based on direction prioritising horizontal/vertical."
  [{:keys [direction]}]
  [(mod direction -2)])  ; prioritise horizontal/vertical

(defn update-state [_state]
  (q/random-seed 10)
  (let [state (sort-by line-sort (take 95000 (iterate next-line {:line [0 0 0 0]
                                                                 :direction 0
                                                                 :length 0
                                                                 :stroke [0 0 0 0]
                                                                 :stroke-weight 0})))]
    (reset! debug state)))

(defn random-purple
  "Return a random hue of purple."
  []
  [280 90 (+ 35 (gaussian-range 25)) 1])

(defn equilaterals
  "Draw up/down equilateral triangles."
  [base-x base-y]
  (doseq [[x y] [[base-x base-y] [(+ base-x 50) (- base-y 100)]]]
    (q/fill (random-purple))
    (q/triangle x y (+ x 50) (+ y 100) (+ x 100) y)  ; up
    (q/fill (random-purple))
    (q/triangle x y (+ x 50) (+ y 100) (- x 50) (+ y 100))))  ; down

;; Drawing
(defn draw [state]
  ;; Conventional coords with each axis -5000 to 5000
  (let [factor (/ (min (q/width) (q/height)) 10000)]
    (q/scale factor (- factor))
    (q/translate (/ (q/width) 2 factor) (/ (q/height) -2 factor)))

  ;; Patterned background
  (q/background 280 90 100 1)
  (q/fill 0 100 100 1)
  (q/no-stroke)
  (doseq [y (range -5000 5100 200)
          x (range -5100 5050 100)]
    (equilaterals x y))

  ;; Weave
  (doseq [{:keys [line stroke stroke-weight]} state
          :let [[x1 y1 x2 y2] line]]
    (q/stroke stroke)
    (q/stroke-weight stroke-weight)
    (q/line x1 y1 x2 y2))

;; Uncomment the following form to generate PNG. Consider increasing size first (see below).
  #_(q/save-frame "images/weave.png"))

#_{:clj-kondo/ignore [:unresolved-symbol]}
(q/defsketch weave
  :title "Weave"
  :settings #(q/smooth 8)
  :setup setup
  :draw draw
  :update update-state
  :middleware [m/fun-mode]
  :size [1000, 1000])
