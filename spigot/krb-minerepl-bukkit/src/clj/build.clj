(ns krb-minerepl-bukkit.build
  (:require
   [clojure.tools.logging :as log]
   [krb-minerepl-bukkit.core :as core]))

(defn get-blocks [location direction length]
  (let [world (.getWorld location)]
    (mapv
     (fn [ii]
       (let [block (.getBlockAt world (core/inc-loc-dir location direction))]
         [(.getLocation block) (.getType block)]))
     (range length))))

(comment
  (get-blocks
   (core/loc+ (.getLocation (.getTargetBlockExact (core/get-player-named "DominusSermonis") Integer/MAX_VALUE))
              [0 1 0])
   (.name (.getFacing (core/get-player-named "DominusSermonis")))
   10)

  )

(defn set-blocks [locations-and-types]
  (doall
   (reduce
    (fn [acc [loc type]]
      (let [block     (.getBlockAt (.getWorld loc) loc)
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
     (.name (.getFacing (core/get-player-named "DominusSermonis")))
     10))

  (core/schedule!
   (set-blocks
    (mapv
     (fn [[loc _type]]
       [loc org.bukkit.Material/SMOOTH_STONE_SLAB])
     old-blocks)))

  (core/schedule! (set-blocks old-blocks))

  )

(defn build-road [loc direction length step pave-fn]
  )

(comment
  (core/all-players)
  (core/get-player-named "DominusSermonis")
  (.getCompassTarget (core/get-player-named "DominusSermonis"))
  ;; #object[org.bukkit.Location 0x25b0a681 "Location{world=CraftWorld{name=world},x=-256.0,y=72.0,z=-80.0,pitch=0.0,yaw=0.0}"]
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

  (build-road
   (.getLocation (.getTargetBlockExact (core/get-player-named "DominusSermonis") 9999))
   (.getFacing (core/get-player-named "DominusSermonis"))
   10
   1
   (fn [acc location _direction _length _step]
     ;; make a road of half slabs (lower half)
     ;; have four air blocks cleared overhead
     ;; if the fifth block overhead is water, lava, sand or gravel, replace it with a glass block
     ;; accumulate the location and type of each block changed
     (let [block (.getBlockAt (.getWorld location) location)]
       (.setType block org.bukkit.Material/SMOOTH_STONE_SLAB))))
  ;; COBBLED_DEEPSLATE_SLAB
  ;; POLISHED_DEEPSLATE_SLAB

  (.getBlockAt
   (.getWorld (core/get-player-named "DominusSermonis"))
   (.getLocation (.getTargetBlockExact (core/get-player-named "DominusSermonis") 9999)))
  )
