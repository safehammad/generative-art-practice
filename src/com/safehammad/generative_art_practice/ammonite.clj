(ns com.safehammad.generative-art-practice.ammonite
  (:require [quil.core :as q]
            [quil.helpers.tools :as qtools]
            [quil.middleware :as m]
            [clojure.math :as math :refer [sin cos]]))

(comment
  (require '[portal.api :as p])
  (def p (p/open)) ; Open a new inspector
  (add-tap #'p/submit) ; Add portal as a tap> target
  )

;; Inspect state used to generate image
(def debug (atom nil))

(defn increment-len [len expansion-factor]
  (let [random-factor (q/random-gaussian)]
    (+ (inc len) (cond
                   (> random-factor 1)  (- 9 expansion-factor)
                   (< random-factor -1) (- expansion-factor 5)
                   :else 0))))

(defn random-shift [value]
  (+ value (* (q/random-gaussian) 2)))

(defn make-swirl [expansion-factor max-angle hue]
  (loop [angle 0
         len 50
         expand-by expansion-factor
         sat 100
         coords []]
    (if (> angle max-angle)
      coords
      (let [r (* len expand-by)
            x (* (cos angle) r)
            y (* (sin angle) r)
            r2 (* (+ len 10) expand-by)
            x2 (random-shift (* (cos angle) r2))
            y2 (random-shift (* (sin angle) r2))
            angle-increment (/ 70 r2)
            below-angle-increment (* 0.75 angle-increment)
            x3 (random-shift (* (cos (+ angle below-angle-increment)) r2))
            y3 (random-shift (* (sin (+ angle below-angle-increment)) r2))
            x4 (random-shift (* (cos (+ angle below-angle-increment)) r))
            y4 (random-shift (* (sin (+ angle below-angle-increment)) r))
            random-hue (+ hue (* 10 (q/random-gaussian)))]
        (if (> y (+ 4250 (q/random -250 250)))
          coords
          (recur (+ angle angle-increment)
                 (increment-len len expansion-factor)
                 (* expand-by 0.9996)
                 (* sat 0.999)
                 (conj coords
                       {:coord [x y]
                        :color [random-hue 100 90 1]}
                       {:coord [x2 y2]
                        :color [random-hue 100 90 1]}
                       {:coord [x3 y3]
                        :color [random-hue 100 90 1]}
                       {:coord [x4 y4]
                        :color [random-hue 100 90 1]})))))))

(defn create-state
  "Main function to create the state of the drawing."
  [_state]
  (let [lines (map make-swirl
                   (filter #(< (- (math/ceil %) %) 0.4) (range 3.5 7.5 0.017))
                   (range (* 1.2 q/TWO-PI) (* 3.4 q/TWO-PI) 0.15)
                   (cycle (mapcat repeat [25 41 12] [220 60 10])))]
    {:lines lines}))

(defn setup []
  ;; Visuals
  (q/color-mode :hsb 360 100 100 1.0)
  (q/frame-rate 1)

  ;; State
  (create-state []))

(defn update-state [state]
  (reset! debug (create-state state)))

(defn darken [[h s l a]]
  [h s (* 0.9 l) a])

;; Drawing
(defn draw [{:keys [lines]}]
  ;; Conventional coords
  (let [factor (/ (min (q/width) (q/height)) 11850)]
    (q/scale factor (- factor))
    (q/translate (/ (q/width) 2 factor) (/ (q/height) -2 factor))
    (q/translate -700 375)) ; Shift up and left to fit

  (q/random-seed 10)

  (q/background 260 90 30 1)
  (q/no-fill)

  (q/stroke-weight 2)

  ;; Start at the bottom and work up row by row
  (doseq [line lines]
    (qtools/with-shape :quads
      (doseq [{:keys [coord color]} line]
        (q/stroke (darken color))
        (q/fill color)
        (apply q/vertex coord))))

  ;; Uncomment the following form to generate PNG. Consider increasing size first (see below).
  #_(q/save-frame "images/ammonite.png"))

#_{:clj-kondo/ignore [:unresolved-symbol]}
(q/defsketch ammonite
  :title "Ammonite"
  :settings #(q/smooth 8)
  :setup setup
  :draw draw
  :update update-state
  :middleware [m/fun-mode]
  :size [1000 1000]
  ;:size [3900, 3900]  ; Use this size when uncommenting "save-frame" (see above) to generate high res image
  )
