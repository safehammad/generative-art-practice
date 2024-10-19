(ns com.safehammad.generative-art-practice.quad-crystals
  (:require [quil.core :as q]
            [quil.helpers.tools :as qtools]
            [quil.middleware :as m]
            [clojure.math :as math :refer [round]]
            [com.safehammad.generative-art-practice.utils :as utils]))

(def debug (atom nil))

(defn quadrant-color
  "Return [hue max-sat] based on quadrant of x, y coord."
  [x y]
  (case (map math/signum [x y])
    [-1.0 1.0] [0 85]    ; NW
    [1.0 1.0] [120 85]   ; NE
    [1.0 -1.0] [240 85]  ; SE
    [-1.0 -1.0] [0 0]    ; SW
    [0 0]))              ; Default: hide those at origin with no area

(defn create-quads
  "Create `iterations` quads where each quad is shifted by `jitter` * random-gaussian value."
  [x y width height jitter iterations]
  (for [_ (range iterations)]
    (utils/jitter jitter (utils/create-quad x y width height))))

(defn next-state []
  (q/random-seed 19)
  (for [[m n] (take 95 (utils/halton-coord-seq 2 3))
        :let [[x y] (map #(- (* % 5500) 2750) [m n])  ; Centre about origin
              [width height] (map abs [x y])
              [x y] (map #(+ % (* (math/signum %) 150)) [x y])  ; Shift out from centre
              area (* width height)
              [hue sat] (quadrant-color x y)]
        :when (and (pos? area) (< area 3000000))]  ; Discard largest squares hogging 45 degree diagonals
    {:stroke [0 0 85 1]
     :fill [(+ hue (round (* (q/random-gaussian) 35)))  ; hue
            (* (max m n) sat)                           ; sat
            (- 100 (* 20 (/ area 4500000)))             ; bri
            0.35]                                        ; alpha
     :coords (create-quads x y width height (+ 5 (/ area 150000)) 2)}))

(defn setup []
  (q/color-mode :hsb 360 100 100 1.0)
  (q/frame-rate 1)
  (next-state))

(defn update-state [_state]
  (reset! debug (next-state)))

;; Drawing

(defn draw [state]
  ;; Conventional coords
  (let [factor (/ (min (q/width) (q/height)) 10000)]
    (q/scale factor (- factor))
    (q/translate (/ (q/width) 2 factor) (/ (q/height) -2 factor)))

  ;; Clear
  (q/background 240 100 50 1)
  (q/stroke-weight 14)

  ;; Draw quads
  (doseq [{:keys [stroke fill coords]} state]
    (apply q/stroke stroke)
    (apply q/fill fill)
    (doseq [quad coords]
      (qtools/with-shape :quads
        (doseq [[x y] quad]
          (q/vertex x y)))))

  ;; Uncomment the following form to generate PNG. Consider increasing size first (see below).
  #_(q/save-frame "images/quad-crystals.png"))

#_{:clj-kondo/ignore [:unresolved-symbol]}
(q/defsketch quad-crystals
  :title "Quad Crystals"
  :settings #(q/smooth 8)
  :setup setup
  :draw draw
  :update update-state
  :middleware [m/fun-mode]
  :size [1000, 1000]
  ;:size [3900 3900]  ; Use this size when uncommenting "save-frame" (see above) to generate high res image
  )
