(ns com.safehammad.generative-art-practice.nett
  (:require [clojure.math :as math]
            [quil.core :as q]
            [quil.middleware :as m]))

(def debug (atom nil))

(def dot-width 105)  ; Number of dots wide
(def extent 4200)    ; Number of virtual pixels wide
(def v-limit 5)      ; Maximum velocity of dot, it's actually the speed, but will refer to it as v!

(defn dot-grid [width]
  (for [row (range 0 width)  ; rows x cols
        col (range 0 width)]
    {:row row
     :col col
     :a (/ col width)      ; Rebase to value between 0 and 1
     :b (/ row width)}))   ; Rebase to value between 0 and 1

(defn create-dots [n]
  (for [{:keys [a b row col]} (take n (dot-grid dot-width))
        ;; Translate to virtual coords
        :let [x (* (* 2 (- 0.5 a)) extent)
              y (* (* 2 (- 0.5 b)) extent)]]
    {:row row
     :col col
     :x x
     :y y
     :v 0      ; velocity ()
     :vx 0     ; velocity in x direction
     :vy 0}))  ; velocity in y direction

(defn resize-vector
  "Resize vector `vx`, `vy` to vector in same direction with magnitude `mag`."
  [mag vx vy]
  (let [mag' (math/sqrt (+ (* vx vx) (* vy vy)))
        mag-ratio (/ mag mag')
        new-vx (* mag-ratio vx)
        new-vy (* mag-ratio vy)]
    [new-vx new-vy]))

(defn accelerate
  "Adjust velocity (accelerate) dots based on the force applied to it.."
  [dots [mx my :as _magnet]]
  (for [{:keys [x y vx vy] :as dot} dots
        :let [dx (- mx x)
              dy (- my y)
              d (q/mag dx dy)
              f (/ 5000000 (inc (* d d)))
              [fx fy] (resize-vector f dx dy)
              new-vx (+ vx fx)
              new-vy (+ vy fy)
              new-v (q/constrain (q/mag new-vx new-vy) 0 v-limit)  ; maximum v is 5
              [newVx newVy] (resize-vector new-v new-vx new-vy)]]
    (-> dot
        (assoc :v new-v)
        (assoc :vx newVx)
        (assoc :vy newVy))))

(defn drag
  "Apply a fixed drag (deceleration) to the dots."
  [dots]
  (for [dot dots]
    (-> dot
        (update :vx (partial * 0.85))
        (update :vy (partial * 0.85)))))

(defn move
  "Move dots based on velocity."
  [dots]
  (for [{:keys [vx vy] :as dot} dots]
    (-> dot
        (update :x (partial + vx))
        (update :y (partial + vy)))))

(defn next-move [state magnet]
  (move (drag (accelerate state magnet))))

(defn random-point []
  (vector (q/random -3600 4000) (q/random -4000 4000)))

;; Define a set of fixed locations (magnets) to which the dots attract
(let [magnets (->> (repeatedly random-point)
                   (take 28))]
  (defn next-state [state]
    (reduce next-move state magnets)))

(defn setup []
  (q/color-mode :hsb 360 100 100 1.0)
  (q/frame-rate 3)
  (q/random-seed 15)
  (create-dots 40000))

(defn update-state [state]
  (if (< (q/frame-count) 21)
    (reset! debug (next-state state))
    state))

;; Drawing

(defn draw [state]
  ;; Conventional virtual coords -5000 to 5000 on x and y axes
  (let [factor (/ (min (q/width) (q/height)) 10000)]
    (q/scale factor (- factor))
    (q/translate (/ (q/width) 2 factor) (/ (q/height) -2 factor)))

  ;; Clear
  (q/background 185 2 100 1)

  ;; Draw quads
  (q/stroke-weight 8)
  (let [rows (partition dot-width state)
        row-pairs (map vector rows (rest rows))]
    (q/begin-shape :quads)
    (doseq [[upper-row lower-row] row-pairs
            dots (map vector upper-row lower-row (rest upper-row) (rest lower-row))  ; corders of quad
            :let [[{:keys [row col]}] dots
                  max-v (apply max (map :v dots))
                  colour [(+ (mod (+ 345 (* (/ max-v v-limit) 40)) 360)
                             (- 10 (* 1.5 (min (abs (- 50 row)) (abs (- 50 col))))))
                          (/ (q/mag row col) 0.6)
                          (- 125 (* 2.4 (min (abs (- 50 row)) (abs (- 50 col)))))
                          1]]]
      (q/fill colour)
      (q/stroke colour)
      (doseq [{:keys [x y]} (replace dots [0 1 3 2])]  ; Access corners in right order
        (q/vertex x y)))
    (q/end-shape)

    ;; Points
    (q/stroke 185 2 100 1)
    (doseq [line rows]
      (doseq [{:keys [x y v row col]} line
              :when (and (< 0 row (dec dot-width)) (< 0 col (dec dot-width)))]
        (q/stroke-weight (+ 6 (* 10 (/ v-limit (inc v)))))
        (q/point x y))))

  ;; Uncomment the following form to generate a series of PNGs from which a video can be created:
  ;; $ ffmpeg -framerate 5 -pattern_type glob -i 'nett-*.png' -c:v libx264 -pix_fmt yuv420p nett.mp4
  #_(q/save-frame (format "nett-%s.png" (q/frame-count)))

  ;; Uncomment the following form to generate final PNG
  #_(when (>= (q/frame-count) 21)
      (q/save-frame "nett.png")))

#_{:clj-kondo/ignore [:unresolved-symbol]}
(q/defsketch nett
  :title "Nett"
  :settings #(q/smooth 8)
  :setup setup
  :draw draw
  :update update-state
  :middleware [m/fun-mode]
  :size [1000, 1000]
  ;:size [3600 3600]  ; Use this size when uncommenting "save-frame" (see above) to generate high res image
  )
