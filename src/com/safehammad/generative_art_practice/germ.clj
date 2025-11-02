(ns com.safehammad.generative-art-practice.germ
  (:require [clojure.math :refer [sqrt cos] :as math]
            [quil.core :as q]
            [quil.middleware :as m]
            [com.safehammad.generative-art-practice.utils :refer [q-rand-nth]]))

;; Inspect state used to generate image
(def debug (atom nil))
(def factor 75)  ; Scale of hexagons
(def frames 190)  ; Number of frames to draw

(def sin-30 0.5)
(def cos-30 (cos (/ math/PI 6)))

(defn virtual->physical [{:keys [x y] :as point}]
  (assoc point
         :x (* x factor (sqrt 2) cos-30 0.5)
         :y (* y factor)))

(defn setup []
  ;; Visuals
  (q/color-mode :hsb 360 100 100 1.0)
  (q/frame-rate 30)
  (q/random-seed 2)
  (let [points {{:x 0 :y 0} {:level 40 :status :infected :direction 0}}]
    {:points points}))

(def directions
  "Directions from one hexagon to another together with associated vector

              5 0
             4 C 1
              3 2"
  {0 [1 1]
   1 [2 0]
   2 [1 -1]
   3 [-1 -1]
   4 [-2 0]
   5 [-1 1]})

(defn direction->coord [{:keys [x y]} direction]
  (let [[dx dy] (directions direction)]
    {:x (+ x dx), :y (+ y dy)}))

(defn random-direction! []
  (q-rand-nth (keys directions)))

(defn infect [points infector-level neighbour-coord new-direction]
  (let [{:keys [status]} (get points neighbour-coord)]
    (when (or (not status)
              (= status :infected))
      {neighbour-coord {:level (* (inc (quot infector-level 10)) 1) :status :infected :direction new-direction}})))

(defn process [points [coord {:keys [level status direction]}]]
  (case status
    :infected (if (< level frames)
                (let [new-direction (random-direction!)
                      neighbour-coord (direction->coord coord new-direction)]
                  [{coord {:level -0.5 :status :infected :direction new-direction}}
                   (infect points level neighbour-coord new-direction)])
                [{coord {:level -3 :status :recovering :direction direction}}])

    :recovering [{coord (if (> level 10)
                          {:level -6 :status :recovering :direction direction}
                          {:level 0 :status :recovered :direction direction})}]
    :recovered nil))

(defn update-status [{l-level :level} {r-level :level r-status :status direction :direction}]
  {:level (+ l-level r-level)
   :status r-status
   :direction direction})

(defn update-state [{:keys [points] :as state}]
  (if (> (q/frame-count) frames)
    state
    (let [updates (mapcat (partial process points) points)
          new-points (apply merge-with update-status points updates)
          state (assoc state :points new-points)]
      (reset! debug state))))

(defn hexagon [x y r]
  (q/begin-shape)
  (q/vertex x (+ y r))
  (q/vertex (+ x (* r cos-30)) (+ y (* r sin-30)))
  (q/vertex (+ x (* r cos-30)) (- y (* r sin-30)))

  (q/vertex x (- y r))
  (q/vertex (- x (* r cos-30)) (- y (* r sin-30)))
  (q/vertex (- x (* r cos-30)) (+ y (* r sin-30)))

  (q/vertex x (+ y r))
  (q/end-shape))

;; Drawing
(defn draw [{:keys [points] :as _state}]
  ;; Conventional coords with each axis -5000 to 5000
  (let [factor (/ (min (q/width) (q/height)) 10000)]
    (q/scale factor (- factor))
    (q/translate (/ (q/width) 2 factor) (/ (q/height) -2 factor)))

  (if (<= (q/frame-count) 85)
    (do
      (q/background 120 0 10 1)

      (q/stroke-weight 10)
      (let [radius (* 0.5 factor)]
        (doseq [[coord {:keys [level status]}] points
                :let [{:keys [x y]} (virtual->physical coord)]]
          (if (< (q/random 100) 10)
            (do
              (q/stroke 0 0 (+ 70 level) (if (= status :recovered)
                                           1
                                           (+ (/ level 5) 0.2)))
              (q/fill 0 0 0 1))
            (do
              (q/no-stroke)
              (if (= status :recovered)
                (q/fill 0 100 90 1)
                (q/fill (+ (* 2.2 level) 20) (+ 50 level) (+ 70 level) (/ level 20)))))

          (hexagon x y radius))))
    ;; Uncomment the following form to generate PNG. Consider increasing size first (see below).
    #_(q/save-frame "images/germ.png")))

#_{:clj-kondo/ignore [:unresolved-symbol]}
(q/defsketch transmission
  :title "Germ"
  :settings #(q/smooth 8)
  :setup setup
  :draw draw
  :update update-state
  :middleware [m/fun-mode]
  :size [1000, 1000])

