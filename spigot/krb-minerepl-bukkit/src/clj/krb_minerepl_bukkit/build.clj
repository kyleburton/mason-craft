(ns krb-minerepl-bukkit.build
  (:require
   [clojure.tools.logging    :as log]
   [krb-minerepl-bukkit.core :as core]))

(defn get-blocks [location direction length]
  (let [world (.getWorld location)]
    (mapv
     (fn [ii]
       (let [loc (core/loc+direction location direction ii)]
         [loc (.. loc getBlock getType)]))
     (range length))))

(comment
  (get-blocks
   (core/loc+ (.getLocation (.getTargetBlockExact (core/get-player-named "DominusSermonis") Integer/MAX_VALUE))
              [0 1 0])
   (.getFacing (core/get-player-named "DominusSermonis"))
   10)

  )

(defn set-blocks [locations-and-types]
  (doall
   (reduce
    (fn [acc [loc type]]
      (let [block     (.getBlock loc)
            orig-type (.getType block)]
        (.setType block type)
        (conj acc [loc orig-type])))
    []
    locations-and-types)))

(comment

  (def old-blocks
    (get-blocks
     (core/loc+ (.getLocation (.getTargetBlockExact (core/get-player-named "DominusSermonis") Integer/MAX_VALUE))
                [0 1 0])
     (.getFacing (core/get-player-named "DominusSermonis"))
     10))

  (core/schedule!
   (set-blocks
    (mapv
     (fn [[loc _type]]
       [loc org.bukkit.Material/SMOOTH_STONE_SLAB])
     old-blocks)))

  (core/schedule! (set-blocks old-blocks))

  )

(defn build-road [loc direction num-segments step pave-fn]
  (loop [loc          loc
         num-segments num-segments
         acc          []]
    (if (zero? num-segments)
      acc
      (do
        (pave-fn acc loc direction num-segments step)
        (recur
         (core/loc+direction loc direction)
         (dec num-segments)
         (conj acc [loc (.getType (.getBlock loc))]))))))

(comment
  (core/all-players)
  (core/get-player-named "DominusSermonis")
  (.getCompassTarget (core/get-player-named "DominusSermonis"))
  (.getSpectatorTarget (core/get-player-named "DominusSermonis"))

  (def player (core/get-player-named "DominusSermonis"))
  (.getPlayer player)
  (.getItemOnCursor player) ;; ItemStack AIR

  (.getEyeHeight player)
  (.getEyeLocation player)
  ;; .getLineOfSight (java.util.Set, int)
  (.getLineOfSight (core/get-player-named "DominusSermonis") nil 0)
  (do
    (Thread/sleep 3000)
    (.getLineOfSight (core/get-player-named "DominusSermonis") nil 0))
  ;; .getTargetBlock (java.util.Set, int)
  (.getTargetBlock (core/get-player-named "DominusSermonis") nil 0)
  (.getTargetBlockExact (core/get-player-named "DominusSermonis") 9999)
  ;; .getTargetBlockExact int
  ;; .getLocation
  (.getLocation (core/get-player-named "DominusSermonis"))
  ;; pitch: positive is down
  ;; pitch: negative is up
  ;; yaw:   0 is south
  ;; yaw:  90 is west
  ;; yaw: 180/-180 is north
  ;; yaw: -90 is east
  (.getFacing (core/get-player-named "DominusSermonis"))

  (.getWorld (core/get-player-named "DominusSermonis"))

  (core/schedule!
   (build-road
    (.getLocation (.getTargetBlockExact (core/get-player-named "DominusSermonis") 9999))
    (.getFacing (core/get-player-named "DominusSermonis"))
    1024
    1
    (fn [acc location direction _length _step]
      ;; make a two wide road of MATERIAL (lower half), as a "trough"
      ;; G G G G  <--- if any of these are falling blocks (water/lava/gravel/sand), make them glass
      ;; - - - -
      ;; - - - -
      ;; X - - X
      ;; X X X X
      ;;     ^--- player points to this block to start
      ;; if the fifth block overhead is water, lava, sand or gravel, replace it with a glass block
      ;; ? accumulate the location and type of each block changed ?
      (let [material    org.bukkit.Material/BLUE_ICE
            ;; offset is "next to" the block, think 90 degrees
            offset      (-> direction
                            core/cardinal-directions
                            {:north [-1  0  0]
                             :south [ 1  0  0]
                             :east  [ 0  0  1]
                             :west  [ 0  0 -1]})
            air-offsets [(core/loc+ location offset [0 1 0])
                         (core/loc+ location        [0 1 0])

                         (core/loc+ location offset offset [0 2 0])
                         (core/loc+ location offset        [0 2 0])
                         (core/loc+ location               [0 2 0])
                         (core/loc- location offset        [0 2 0])

                         (core/loc+ location offset offset [0 3 0])
                         (core/loc+ location offset        [0 3 0])
                         (core/loc+ location               [0 3 0])
                         (core/loc- location offset        [0 3 0])]

            material-offsets [(core/loc+ location offset offset [0 1 0])
                              (core/loc+ location offset offset)
                              (core/loc+ location offset)
                              (core/loc+ location)
                              (core/loc- location offset)
                              (core/loc- location offset [0 -1 0])]

            glass-offsets [(core/loc+ location offset offset [0 4 0])
                           (core/loc+ location offset        [0 4 0])
                           (core/loc+ location               [0 4 0])
                           (core/loc- location offset)       [0 4 0]]]
        (doseq [loc material-offsets]
          (.setType (core/get-block-at loc) material))
        (doseq [loc air-offsets]
          (.setType (core/get-block-at loc) (core/materials :air)))
        (doseq [loc glass-offsets]
          (let [block      (core/get-block-at loc)]
            (when (core/materials-affected-by-gravity (.getType block))
              (.setType block org.bukkit.Material/GLASS))))))))

  (core/schedule!
   (doseq [block (core/find-blocks-of-material-around-player "DominusSermonis" org.bukkit.Material/BLUE_ICE 18)]
     (.setType block org.bukkit.Material/DIRT)))
  )


(comment

  (def blocks (mapv core/loc->loc-and-type (core/shell-coords-around-location "DominusSermonis" 3)))

  (core/schedule!
   (set-blocks
    (mapv
     (fn [[loc _type]]
       [loc org.bukkit.Material/GLASS])
     blocks)))

  (core/schedule! (set-blocks blocks))

  (core/schedule!
   (doseq [block (core/find-blocks-of-material-around-player "DominusSermonis" org.bukkit.Material/BLUE_ICE 18)]
     (.setType block org.bukkit.Material/DIRT)))


  )

(defn clear-to-bedrock [loc dist]
  (def loc loc)
  (def dist dist)
  (let [direction      (.getFacing (core/get-player-named "DominusSermonis"))
        forward-offset (-> direction
                           core/cardinal-directions
                           {:north [ 0  0 -1]
                            :south [ 0  0  1]
                            :east  [ 1  0  0]
                            :west  [-1  0  0]})
        left-offset    (-> direction
                           core/cardinal-directions
                           {:north [-1  0  0]
                            :south [ 1  0  0]
                            :east  [ 0  0 -1]
                            :west  [ 0  0 1]})]
    (doseq [xoff (range 0 dist)
            zoff (range 0 dist)
            :let [loc1 (core/loc+
                        loc
                        (core/loc* forward-offset xoff)
                        (core/loc* left-offset    zoff))
                  [xx yy zz] (core/location-to-xyz loc1)]]
      (doseq [yy   (range (.getHighestBlockYAt (core/overworld) xx zz) -64 -1)
              :let [block      (core/get-block-at [xx yy zz])
                    block-type (.getType block)]]
        (cond
          (= (core/materials :bedrock) block-type)
          nil
          :else
          (.setType block (core/materials :air)))))))


(comment
  (defn krbtmp [off]
    (let [loc (.getLocation (.getTargetBlockExact (core/get-player-named "DominusSermonis") 9999))
          loc (core/loc+ loc [(* off -32) 0 0])]
      (core/schedule!
       (time
        (do
          (clear-to-bedrock
           loc
           ;; NB: 256 is too much, it crashes the server
           32)
          (log/infof "krbtmp: completed off=%s" off))))))

  (krbtmp 8)
  (krbtmp 7)
  (krbtmp 6)
  (krbtmp 5)
	(krbtmp 4)
  (krbtmp 3)
  (krbtmp 2)
  (krbtmp 1)
  (krbtmp 0)


  (let [loc (.getLocation (.getTargetBlockExact (core/get-player-named "DominusSermonis") 9999))]
    (doseq [off (range 8 -1 -1)]
      (let [loc (core/loc+ loc [(* off -32) 0 0])]
        (core/schedule!
         (time
          (do
            (clear-to-bedrock
             loc
             ;; NB: 256 is too much, it crashes the server
             32)
            (log/infof "doseq: completed off=%s" off)))))
      (Thread/sleep 10000)))

  )


(comment
  ;; facing a "wall", iterate to the left & right; fill in any
  ;; openings (lower than the getHighestBlockYAt) or if there is water

  (.getType (.getBlock (.getLocation (.getTargetBlockExact player 9999))))
  (.isAir (core/materials :seagrass))
  (.isSolid (core/materials :seagrass))
  (class (core/materials :seagrass))
  (let [player          (core/get-player-named "DominusSermonis")
        loc             (.getLocation (.getTargetBlockExact player 9999))
        axis            (core/player-perpendicular-axis player)
        iterations      16
        horiz-locations (if (= :x axis)
                          (core/loc+-x loc iterations)
                          (core/loc+-z loc iterations))
        xx              (.getX loc)
        zz              (.getZ loc)]
    (def player player)
    (def loc loc)
    (doseq [yy   (range -64 (inc (core/get-highest-block-y-at loc)))
            loc2 horiz-locations
            :let [[xx _yy zz] loc2
                  block       (core/get-block-at xx yy zz)]]
      ;; TODO: any watter logged/loggable
      (when (or (core/=material? block
                                 :water
                                 :air
                                 :lava
                                 :cave-vines
                                 :big-dripleaf
                                 :clay
                                 ;; NB: :pointed-dripstone doens't seem to get replaced?
                                 :pointed-dripstone)
                (not (.isSolid (.getType block))))
        ;; (core/materials :pointed-dripstone)
        (log/infof "comment: set to glass: %s isa %s" [xx yy zz] (.getType block))
        (core/schedule!
         (.setType block (core/->material :glass)))))
    ;; range from left to right, from highest block down to bedrock,
    ;; replace air, water or lava with glass

    ;; facing direction to "axis"
    ;; east/west   => Z
    ;; north/south => X
    ;; (core/get-highest-block-at loc)
    (core/set-world-time! :morning)
    [axis (inc (core/get-highest-block-y-at loc)) horiz-locations])

  (krbtmp 0)

  )

;; north is -z
;; south is +z
;; west  is -x
;; east  is +x
(defn find-next-block-at-height [loc direction max-dist at-height]
  (let [loc       (core/->loc loc)
        xx        (.getX loc)
        zz        (.getZ loc)
        range-seq (cond
                    (= :north direction) (core/range-down (.getZ loc) (- (.getZ loc) max-dist))
                    (= :south direction) (core/range-up   (.getZ loc) (+ (.getZ loc) max-dist))
                    (= :west  direction) (core/range-down (.getX loc) (- (.getX loc) max-dist))
                    (= :east  direction) (core/range-up   (.getX loc) (+ (.getX loc) max-dist)))
        coord-fn  (cond
                    (= :north direction) (fn [%] [xx at-height  %])
                    (= :south direction) (fn [%] [xx at-height  %])
                    (= :west  direction) (fn [%] [%  at-height zz])
                    (= :east  direction) (fn [%] [%  at-height zz]))]
    (->>
     range-seq
     (map coord-fn)
     (filter
      (fn [coord]
        (not (core/=material? coord :air))))
     first
     core/->loc)))

(defn fill-layer [c1 c2 c3 c4 height material pred-fn]
  (let [material (core/->material material)
        coords   [c1 c2 c3 c4]
        x-coords (->> coords (map core/->loc) (map #(.getX %)))
        z-coords (->> coords (map core/->loc) (map #(.getZ %)))
        x-min    (apply min x-coords)
        x-max    (apply max x-coords)
        z-min    (apply min z-coords)
        z-max    (apply max z-coords)]
    (doseq [xx   (range x-min (inc x-max))
            zz   (range z-min (inc z-max))
            :let [block (core/get-block-at [xx height zz])]]
      (if (pred-fn block)
        (.setType block material)))))


(comment
  (let [find-height -59
        ;; find-height -58
        fill-height -60]
    (core/schedule!
     (fill-layer
      (find-next-block-at-height (core/->loc "DominusSermonis") :north 512 find-height)
      (find-next-block-at-height (core/->loc "DominusSermonis") :south 512 find-height)
      (find-next-block-at-height (core/->loc "DominusSermonis") :west  512 find-height)
      (find-next-block-at-height (core/->loc "DominusSermonis") :east  512 find-height)
      fill-height
      :glass
      (fn [block]
        (if (< (* 10000 (rand)) 10) (log/infof "checking if glass block=%s" block))
        (not (core/=material? block :bedrock))))))



  ;; find the "walls" of the large open area - the
  ;; north/south/east/west extents just above the bedrock, level -59

  ;; east
  (let [loc  (core/->loc "DominusSermonis")
        dist 1024]
    (->>
     (core/range-up (.getX loc) (+ (.getX loc) dist))
     (filter
      (fn [xx]
        (not (core/=material? [xx -59 (.getZ loc)] :air))))
     (map (fn [xx]
            [xx -59 (.getZ loc)]))
     first))
  [468.30000001192093 -59 1433.703285991409]

  ;; west
  (let [loc  (core/->loc "DominusSermonis")
        dist 1024]
    (->>
     (core/range-down (.getX loc) (- (.getX loc) dist))
     (filter
      (fn [xx]
        (not (core/=material? [xx -59 (.getZ loc)] :air))))
     (map (fn [xx]
            [xx -59 (.getZ loc)]))
     first))
  [112.8122704476587 -59 1432.587004373802]

  ;; south
  (let [loc  (core/->loc "DominusSermonis")
        dist 1024]
    (->>
     (core/range-up (.getZ loc) (+ (.getZ loc) dist))
     (filter
      (fn [zz]
        (not (core/=material? [(.getX loc) -59 zz] :air))))
     (map (fn [zz]
            [(.getX loc) -59 zz]))
     first))
  [300.30000001192093 -59 1604.1939984143335]

  ;; north
  (let [loc  (core/->loc "DominusSermonis")
        dist 1024]
    (->>
     (core/range-down (.getZ loc) (- (.getZ loc) dist))
     (filter
      (fn [zz]
        (not (core/=material? [(.getX loc) -59 zz] :air))))
     (map (fn [zz]
            [(.getX loc) -59 zz]))
     first))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  (let [loc (.getLocation (.getTargetBlockExact (core/get-player-named "DominusSermonis") 9999))
        loc (core/loc+ loc [(* off -32) 0 0])]
    (core/schedule!
     (time
      (do
        ;; fill in the non-bedrock from Y=-64 up to -60
        ))))

  )
