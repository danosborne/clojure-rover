(ns rover.danos
  (:require [clojure.test :refer :all]))

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
;; [[0 0 1 1]
;;  [0 0 0 0]
;;  [1 0 0 0]
;;  [0 0 0 0]]
;;
;; 1's are Obstacles.
;;
;; Could just as easily be { :dimensions [10 10] :obstacles [[0 4] [8 7]] }
;; The nested vectors pretty prints easier though.

;; Rover representation ::
;; { :coords [0 0]
;;   :facing :north }
;;

;; Command representation ::
;; [ffrfflfffrbb]
;;



(defn move [rover grid direction]
  )
(deftest move-forwards
  )
(deftest can-move-backwards
  )

(defonce facings [:north :east :south :west])

(defn- rotates [facing direction]
  (get facings (mod (direction (.indexOf facings facing) 1) (count facings))))

(defn rotate-l [facing]
  (rotates facing -))

(defn rotate-r [facing] 
  (rotates facing +))

(deftest rotates-left
  (are [in out] (= out (rotate-l in))
       :west :south
       :south :east
       :east :north
       :north :west))

(deftest rotates-right 
  (are [in out] (= out (rotate-r in))
       :west :north
       :north :east
       :east :south
       :south :west))

(defn rotate 
  "Returns a freshly rotated rover"
  [{facing :facing :as rover} rotator]
  (assoc rover :facing (rotator facing)))

(deftest rotates-rover-left
  (is (= :west (:facing (rotate {:facing :north } rotate-l)))))

(deftest rotates-rover-right
  (is (= :east (:facing (rotate {:facing :north } rotate-r)))))

;; -- Command dispatch...
;;
(defmulti  execute (fn [command _ _] (identity command)))
(defmethod execute \l [_ rover grid] (rotate rover rotate-l))
(defmethod execute \r [_ rover grid] (rotate rover rotate-r))
(defmethod execute \f [_ rover grid] (identity rover))
(defmethod execute \b [_ rover grid] (identity rover))

;; -- Helpful for building our data structures...
;;
(defn- make-row [cols] (vec (repeat cols 0)))
(defn- make-grid [rows cols] (vec (repeat rows (make-row cols))))
(defn- update-cell [grid x y val]
  (assoc grid y (assoc (nth grid y) x val)))

;; -- For eyeballing...
;;
(defn render [rover grid]
  (clojure.pprint/pprint 
   (update-cell grid 
                (-> rover :coords first) 
                (-> rover :coords second) 
                ((:facing rover) {:north 8 :east 6 :south 2 :west 4}))))

(comment 
  ;; Original version for comparison
  (defn apply-command 
  "Returns the new state of the rover following application of the command"
  [command rover grid]
  (cond
    (= command \l) (rotate rover rotate-l)
    (= command \r) (rotate rover rotate-r)
    ;; (= command "f") (move rover grid forwards)
    ;; (= command "b") (move rover grid backwards)
    :else (prn "FAIL")))
)

;; -- The mainline!
;;
(defn go 
  "Returns a sequence of rover states following each command execution" 
  [rover grid commands]
  (reduce #(conj %1 (execute %2 (last %1) grid)) [rover] commands))

(comment 
  (clojure.pprint/pprint (go {:coords [1 2] :facing :north} (make-grid 4 4) "rr"))
  (go {:coords [1 2] :facing :north} (make-grid 10 10) "rrffflfflbb")
)

(deftest test-go 
  (let [results (go {:coords [1 2] :facing :north} (make-grid 4 4) "rr")]
    (is (= :north (:facing (first results))))
    (is (= :east (:facing (second results))))
    (is (= :south (:facing (last results))))))

(test-go)


