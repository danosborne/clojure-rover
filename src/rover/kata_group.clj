(ns rover.kata-group
  (:require [clojure.test :refer :all]))

;; Rover expressed as map of coords and facing direction
;; {:coords [0 1] :facing :south} 
;;

;; World/Grid expressed as dimensions
;; {:x 4 :y 6}

(deftest rover-location 
  (let [a-rover {:coords [1 1]}]
    (is (=  [1 1] (:coords a-rover)))))

(deftest rover-direction 
  (let [a-rover {:facing :north}]
    (is (= :north (:facing a-rover)))))

(deftest grid
  (let [a-grid {:x 5 :y 6}]
    (is (= 5 (:x a-grid)))
    (is (= 6 (:y a-grid)))))

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

(defn- move 
  "keyword, keyword"
  [coords facing direction-to-move]
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
  "Takes a rover, gives a new rover having moved in forwards"
  [rover]
  ((make-move :forward) rover))

(deftest move-forward
  (are [facing in out] 
    (= 
     {:coords out :facing facing}
     (move-f {:coords in :facing facing}))
    :north [2 2] [2 1]
    :south [2 2] [2 3]
    :east [2 2] [3 2]
    :west [2 2] [1 2]))

(defn move-b 
  "Takes a rover, gives a new rover"
  [rover]
  ((make-move :backward) rover))

(deftest move-backward
  (are [facing in out] 
    (= 
     {:coords out :facing facing}
     (move-b {:coords in :facing facing}))
    :north [2 2] [2 3]
    :south [2 2] [2 1]
    :east [2 2] [1 2]
    :west [2 2] [3 2]))

;; This runs all tests in all namespace mathcing regex
;; Would typically develop with this and remove when done.
(run-all-tests #"rover\\.group")
