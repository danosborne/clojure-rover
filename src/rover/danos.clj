(ns rover.danos)

;; We are given Rover's starting coords x,y and facing direction N,S,W,E
;; Rover is given a sequence of characters representing instructions
;;
;; Implement:
;;
;;  Move forwards/backwards
;;  Rotate left/right
;;  Grid wrapping
;;  Obstacles & collision detection

;; Grid representation ::
;;
;; Nice rendered like this:
;; [[0 0 1 1]
;;  [0 0 0 0]
;;  [1 0 0 0]
;;  [0 0 0 0]]
;;
;; ... 1's are Obstacles.
;;
;; Perhaps better defined as data like this though:
;; { :dimensions [10 10] :obstacles [[0 4] [8 7]] }
;;
;; ... [0,0] is top, left; like SVG.

;; Rover representation ::
;; { :coords [0 0]
;;   :facing :north }
;;

;; Command representation ::
;; "ffrfflfffrbb"
;;

(defrecord Rover [coords facing])

(def movements
  {:north 
   {:forward -
    :backward +}
   :south 
   {:forward +
    :backward -}
   :east
   {:forward +
     :backward -}
   :west
   {:forward -
    :backward +}})

(defn- move [coords facing direction-to-move]
  (let [move-fun (get-in movements [facing direction-to-move])]
    (cond
      (= facing :north) [(first coords) (move-fun (second coords) 1)]
      (= facing :south) [(first coords) (move-fun (second coords) 1)]
      (= facing :east) [(move-fun (first coords) 1) (second coords)]
      (= facing :west) [(move-fun (first coords) 1) (second coords)])))

(defn make-move [direction]
  (fn [{:keys [coords facing]}]
    {:coords (move coords facing direction) :facing facing}))

(defn move-f 
  "Takes a rover, gives a new rover having moved it forwards"
  [rover]
  ((make-move :forward) rover))

(defn move-b 
  "Takes a rover, gives a new rover"
  [rover]
  ((make-move :backward) rover))

(defonce facings [:north :east :south :west])

(defn- rotates [facing direction]
  (get facings (mod (direction (.indexOf facings facing) 1) (count facings))))

(defn rotate-l [facing]
  (rotates facing -))

(defn rotate-r [facing]
  (rotates facing +))

(defn rotate 
  "Returns a freshly rotated rover"
  [{facing :facing :as rover} rotator]
  (assoc rover :facing (rotator facing)))


;; -- Command dispatch...
;;
(defmulti  execute (fn [command _ _] (identity command)))
(defmethod execute \l [_ rover grid] (rotate rover rotate-l))
(defmethod execute \r [_ rover grid] (rotate rover rotate-r))
(defmethod execute \f [_ rover grid] (move-f rover))
(defmethod execute \b [_ rover grid] (move-b rover))

;; -- The mainline!
;;
(defn go 
  "Returns a sequence of rover states following each command execution" 
  [rover grid commands]
  (reduce #(conj %1 (execute %2 (last %1) grid)) [rover] commands))


;; -- Helpful for building and viewing our data structures at the repl...
;;
(defn- make-render-row [cols] (vec (repeat cols 0)))

(defn- make-render-grid [rows cols] (vec (repeat rows (make-render-row cols))))

(defn- update-cell [rendered-grid x y val]
  (assoc rendered-grid y (assoc (nth rendered-grid y) x val)))

(defn- render-grid 
  "Turns grid into a nested vector of zeros, places 1s where there are obstacles"
  [{[cols rows] :dimensions obstacles :obstacles :as grid-def}]
  (let [rendered (make-render-grid rows cols)]
    (reduce (fn [current [x y]] (update-cell current x y 1)) rendered obstacles)))

(defn- add-rover [rendered-grid {[x y] :coords facing :facing :as rover}]
  (update-cell rendered-grid x y (facing {:north 8 :east 6 :south 2 :west 4})))

;; -- For eyeballing...
;;
(defn render [rover grid]
  (print "\n")
  (doseq [row (-> (render-grid grid) (add-rover rover))]
    (clojure.pprint/pprint row)))

(comment 
  ;; rotation
  (let [grid {:dimensions [4 4] :obstacles []}
        rover (map->Rover {:coords [1 2] :facing :north})
        commands "lr"
        rover-states (go rover grid commands)]
    (clojure.pprint/pprint rover-states)
    (doseq [rover-state rover-states]
      (render rover-state grid)))

  ;; movement
  (let [grid {:dimensions [4 4] :obstacles []}
        rover {:coords [3 3] :facing :north}
        commands "ffb"
        rover-states (go rover grid commands)]
    (clojure.pprint/pprint rover-states)
    (doseq [rover-state rover-states]
      (render rover-state grid)))

  ;; movement and rotation
  (let [grid {:dimensions [10 10] :obstacles []} 
        rover-states 
        (go {:coords [0 0] :facing :north} 
            grid
            "rrffflfflbb")]
    (clojure.pprint/pprint rover-states)
    (doseq [rover-state rover-states]
      (render rover-state grid)))
)

