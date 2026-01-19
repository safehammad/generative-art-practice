(ns com.safehammad.generative-art-practice.nest-egg
  (:require [quil.core :as q]
            [quil.helpers.tools :as qtools]
            [quil.middleware :as m]
            [com.safehammad.generative-art-practice.utils :as utils]))

(comment
  (require '[portal.api :as p])
  (def p (p/open)) ; Open a new inspector
  (add-tap #'p/submit) ; Add portal as a tap> target
  )

;; Inspect state used to generate image
(def debug (atom nil))

(def focus 2250)  ; Distance of focus from origin

(defn diff
  "Difference between two values."
  [a b]
  (abs (- a b)))

(defn ->coord-data [[focus-1-x focus-1-y] [focus-2-x focus-2-y]]
  (fn [coord]
    ;; Expect arm-dist to be between 2x and 4x factor
    (let [arm-dist (+
                    (apply q/dist focus-1-x focus-1-y coord)
                    (apply q/dist focus-2-x focus-2-y coord))
          degree (- 100 (* (/ (diff (* 3 focus) arm-dist) focus) 100))]
      {:coord coord
       :arm-dist arm-dist
       :saturation degree})))

(defn create-state
  "Main function to create the state of the drawing."
  [_state]
  (let [shape :red-nest  ; For alternatives, change to :nest, :red-nest, :star-burst, :streak
        focus-1 [(- focus) 0]
        focus-2 [focus 0]]
    {:shape shape
     :points (->> (utils/halton-coord-seq 8 13) ;(repeatedly (juxt (partial q/random 1) (partial q/random 1)) )
                  (into [] (comp
                            (map (partial mapv #(- (* % 10000) 5000)))
                            (map (->coord-data focus-1 focus-2))
                            (take (case shape
                                    (:nest :streak) 10000
                                    :red-nest 8000
                                    :star-burst 15000))))
                  (sort-by (case shape
                             :streak (comp first :coord)
                             :arm-dist) >))}))

(defn setup []
  ;; Visuals
  (q/color-mode :hsb 360 100 100 1.0)
  (q/frame-rate 1)

  ;; State
  (create-state nil))

(defn update-state [state]
  (reset! debug (create-state state)))

;; Drawing
(defn draw [{:keys [points shape]}]
  ;; Conventional coords with each axis -5000 to 5000
  (let [factor (/ (min (q/width) (q/height)) 10000)]
    (q/scale factor (- factor))
    (q/translate (/ (q/width) 2 factor) (/ (q/height) -2 factor)))

  (q/random-seed 10)

  (q/background 160 90 50 1)
  (q/no-fill)

  (qtools/with-shape :lines
    (case shape
      (:nest :streak)
      (doseq [point-pairs (partition 2 1 points)
              point point-pairs]
        (let [{:keys [coord saturation]} point]
          (q/stroke-weight (+ 3 (abs (/ saturation 15))))
          (q/stroke (- (* 1.85 saturation) 140) saturation 100 (/ saturation 100))
          ;(apply q/vertex [0 0])
          (apply q/vertex coord)))

      :red-nest
      (doseq [point-pairs (partition 2 1 points)
              point point-pairs]
        (let [{:keys [coord saturation]} point]
          (q/stroke-weight (+ 3 (abs (/ saturation 15))))
          (q/stroke (abs (/ saturation 30)) saturation 100 (+ (/ saturation 100) 0.05))
          ;(apply q/vertex [0 0])
          (apply q/vertex coord)))

      :star-burst
      (doseq [{:keys [coord saturation]} points]
        (q/stroke-weight (+ 3 (abs (/ saturation 15))))
        (q/stroke 0 saturation 100 (+ (/ saturation 100) 0.1))
        (apply q/vertex [0 0])
        (apply q/vertex coord))))

;; Start at the bottom and work up row by row

  ;; Uncomment the following form to generate PNG. Consider increasing size first (see below).
  #_(q/save-frame "images/nest-egg.png"))

#_{:clj-kondo/ignore [:unresolved-symbol]}
(q/defsketch nest-egg
  :title "Nest Egg"
  :settings #(q/smooth 8)
  :setup setup
  :draw draw
  :update update-state
  :middleware [m/fun-mode]
  :size [1000 1000]
  ;:size [3900, 3900]  ; Use this size when uncommenting "save-frame" (see above) to generate high res image
  )
