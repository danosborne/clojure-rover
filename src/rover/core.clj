(ns rover.core
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

(defonce facings [:north :east :south :west])

(defn move [rover grid direction]
  )
(deftest move-forwards
  )
(deftest can-move-backwards
  )

(defn- rotates [facing direction]
  (get facings (mod (direction (.indexOf facings facing) 1) (count facings))))

(defn rotate-l [facing]
  (rotates facing -))

(defn rotate-r [facing] 
  (rotates facing +))

(deftest rotate-left
  (are [in out] (= out (rotate-l in))
       :west :south
       :south :east
       :east :north
       :north :west))

(deftest rotate-right 
  (are [in out] (= out (rotate-r in))
       :west :north
       :north :east
       :east :south
       :south :west))

(defn rotate 
  "Returns a freshly rotated rover"
  [rover rotator]
  (assoc rover :facing (rotator (:facing rover))))

(deftest can-rotate-rover-left
  (is (= :west (:facing (rotate {:facing :north } rotate-l)))))

(deftest can-rotate-rover-right
  (is (= :east (:facing (rotate {:facing :north } rotate-r)))))

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

;; -- Command dispatch...
;;
(defn apply-command 
  "Returns the new state of the rover following application of the command"
  [command rover grid]
  (cond
    (= command \l) (rotate rover rotate-l)
    (= command \r) (rotate rover rotate-r)
    ;; (= command "f") (move rover grid forwards)
    ;; (= command "f") (move rover grid backwards)
    :else (prn "FAIL")))

;; -- The mainline!
;;
(defn go 
  "Returns a sequence of rover states following each command execution" 
  [rover grid commands]
  (reduce #(conj %1 (apply-command %2 (last %1) grid)) [rover] commands))

(comment 
  (clojure.pprint/pprint (go {:coords [1 2] :facing :north} (make-grid 4 4) "rr"))
  (go {:coords [1 2] :facing :north} (make-grid 10 10) "rrffflfflbb")
)

(deftest test-go 
  (let [results (go {:coords [1 2] :facing :north} (make-grid 4 4) "rr")]
    (is (= :north (:facing (first results))))
    (is (= :east (:facing (second results))))
    (is (= :south (:facing (last results))))))

3
