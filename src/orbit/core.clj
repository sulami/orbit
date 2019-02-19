(ns orbit.core
  (:require [quil.core :as q]
            [quil.middleware :as m]))

(def gravitational-constant 9.81)

(defn setup []
  (q/frame-rate 30)
  {:objects [{:position [250 250]
              :speed [0 0]
              :size 30
              :mass 100}
             {:position [100 250]
              :speed [0 -2.5]
              :size 10
              :mass 8}
             {:position [400 250]
              :speed [0 2.5]
              :size 10
              :mass 7.8}]})

(defn draw [obj]
  (let [[x y] (:position obj)
        size (:size obj)]
    (q/ellipse x y size size)))

(defn add-vectors [[a-x a-y] [b-x b-y]]
  [(+ a-x b-x) (+ a-y b-y)])

(defn distance [a b]
  (let [[a-x a-y] (:position a)
        [b-x b-y] (:position b)]
    (Math/sqrt (+ (Math/pow (- a-x b-x) 2)
                  (Math/pow (- a-y b-y) 2)))))

(defn gravity-for [this other]
  (let [r (distance this other)
        gravity (* gravitational-constant
                   (/ (* (:mass this) (:mass other))
                      (* r r)))
        mass-mod (/ 1 (:mass this))
        [this-x this-y] (:position this)
        [other-x other-y] (:position other)
        x-dist (- this-x other-x)
        y-dist (- this-y other-y)
        combined-dist (+ (Math/abs x-dist) (Math/abs y-dist))
        x-mod (Math/sin (* Math/PI -1/2 (/ x-dist combined-dist)))
        y-mod (Math/sin (* Math/PI -1/2 (/ y-dist combined-dist)))]
    [(* gravity mass-mod x-mod)
     (* gravity mass-mod y-mod)]))

(defn update-speed [this others]
  (assoc-in this
            [:speed]
            (reduce add-vectors
                    (:speed this)
                    (map (partial gravity-for this) others))))

(defn update-position [this]
  (update-in this [:position] (fn [[x y]]
                                [(+ x (first (:speed this)))
                                 (+ y (second (:speed this)))])))

(defn update-object [this others]
  (-> this
      (update-speed others)
      update-position))

(defn update-objects [objects]
  (for [this objects]
    (let [others (filter (partial not= this) objects)]
      (update-object this others))))

(defn update-state [state]
  (q/ellipse-mode :center)
  (update-in state [:objects] update-objects))

(defn draw-state [state]
  (q/background 30)
  (doseq [obj (:objects state)]
    (draw obj)))

(q/defsketch orbit
  :title "Orbit"
  :size [500 500]
  :setup setup
  :update update-state
  :draw draw-state
  :middleware [m/fun-mode])

(defn -main [])
