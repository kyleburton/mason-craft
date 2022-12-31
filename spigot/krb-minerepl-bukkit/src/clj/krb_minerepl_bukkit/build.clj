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
  ;; TODO: remove the hard-coded player, take direction from loc as the center
  (let [direction      (let [player (core/get-player-named "DominusSermonis")]
                         (if player
                           (.getFacing player)
                           :north))
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

  ;; glass wall ^^^

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
    ;; (find-next-block-at-height loc :north 512 find-height)
    (def coord-fn coord-fn)
    (def range-seq range-seq)
    ;; (->> range-seq (map coord-fn) (take 3))
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
    (doseq [xx   (range (inc x-min) x-max)
            zz   (range (inc z-min) z-max)
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
  )

(defn find-center-of-corners [c1 c2 c3 c4]
  (let [coords   [c1 c2 c3 c4]
        x-coords (->> coords (map core/->loc) (map #(.getX %)))
        y-coords (->> coords (map core/->loc) (map #(.getY %)))
        z-coords (->> coords (map core/->loc) (map #(.getZ %)))
        x-min    (apply min x-coords)
        x-max    (apply max x-coords)
        y-min    (apply min y-coords)
        y-max    (apply max y-coords)
        z-min    (apply min z-coords)
        z-max    (apply max z-coords)]
    (core/->loc [(/ (+ x-min x-max) 2)
                 (/ (+ y-min y-max) 2)
                 (/ (+ z-min z-max) 2)])))

(comment
  ;; find the center of 4 coords
  (let [loc         (core/->loc "DominusSermonis")
        find-height -59]
    (def find-height find-height)
    (find-center-of-corners
     (find-next-block-at-height loc :north 512 find-height)
     (find-next-block-at-height loc :south 512 find-height)
     (find-next-block-at-height loc :west  512 find-height)
     (find-next-block-at-height loc :east  512 find-height)))

  (let [loc (core/->loc [455 71 1320])
        loc (core/loc+ loc [-256 0 0])]
    (core/schedule!
     (.teleport (core/get-player-named "DominusSermonis") loc))
    loc)

  (core/->loc "DominusSermonis")
  )

(comment
  (def state (atom {:stop       false
                    :count      0
                    :batch-size 128
                    :delay      100
                    :xx-start   217}))

  (let [;; loc     (core/->loc "DominusSermonis")
        loc     (core/->loc [199 100 1320])
        dist    256
        ;; dist    64
        xx      (.getX loc)
        ;; xx-min  (- xx dist)
        ;; (- 199 256)
        ;; xx-min  -57
        ;; xx-min  186
        xx-min  (-> state deref :xx-start)
        xx-max  (+ xx dist)  ;; (+ 199 256) 455
        yy      (.getY loc)
        zz      (.getZ loc)
        zz-min  (- zz dist)
        zz-max  (+ zz dist)
        xx-seq  (range xx-min xx-max)
        zz-seq  (range zz-min zz-max)
        loc-seq (for [xx xx-seq zz zz-seq]
                  [xx yy zz])
        recfunc (fn recfunc [locs]
                  (let [batch-size (-> state deref :batch-size)
                        some-locs (take batch-size locs)
                        locs      (drop batch-size locs)]
                    (cond
                      (empty? some-locs)
                      :done

                      (:stop @state)
                      (do
                        (log/infof "recfuncstopping")
                        :stopped)

                      :else
                      (core/schedule!
                       (log/infof "recfunc: processing block of %s first=%s" (count some-locs) (first some-locs))
                       (doseq [loc some-locs]
                         (clear-to-bedrock loc 1))
                       (swap! state merge {:count (inc (-> state deref :count))})
                       (future
                         (Thread/sleep (:delay @state))
                         (recfunc locs))))))]
    (recfunc loc-seq))

  (swap! state merge {:stop false, :delay 1000 :xx-start -54 :batch-size 256})
  (swap! state merge {:stop true})

  (krbtmp 0)
  (core/set-world-time! :morning)

  ;; kill the thread
  (->>
   (Thread/getAllStackTraces)
   (filter
    (fn [[thread stack-trace-elements]]
      (some
       (fn [elt]
         (def elt elt)
         (.contains (str elt) "recfunc"))
       stack-trace-elements)))
   first
   #_.getKey
   #_.stop)
  )

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(declare automata)

(defn automata:forward [state & args]
  (update-in state [:loc] core/loc-forward 1))

(comment
  (core/loc-forward (core/->loc [199 -59 1319 10.049989 -178.95055]) 1)
  (automata:forward
   {:loc (core/->loc [199 -59 1319 10.049989 -178.95055])})
  )

(defn automata:backward [state & args]
  (update-in state [:loc] core/loc-forward -1))

(defn automata:repeat [state times & args]
  (dissoc
   (reduce
    (fn [state ii]
      (log/infof "automata:repeat: times=%s; ii=%s" times ii)
      (automata (assoc state :ii ii) args))
    state
    (range times))
   :ii))

(defn automata:branch [state & args]
  (automata state args)
  state)

(defn attr-relative-to-absolute [loc attr]
  (def loc loc)
  (def attr attr)
  (let [direction (core/->direction loc)]
    (def direction direction)
    (cond
      (and (= :north direction) (= :facing-away attr))    attr
      (and (= :north direction) (= :facing-toward attr))  :south
      (and (= :north direction) (= :facing-left attr))    :west
      (and (= :north direction) (= :facing-right attr))   :east

      (and (= :south direction) (= :facing-away attr))    attr
      (and (= :south direction) (= :facing-toward attr))  :north
      (and (= :south direction) (= :facing-left attr))    :east
      (and (= :south direction) (= :facing-right attr))   :west

      (and (= :east direction) (= :facing-away attr))     attr
      (and (= :east direction) (= :facing-toward attr))   :west
      (and (= :east direction) (= :facing-left attr))     :north
      (and (= :east direction) (= :facing-right attr))    :south

      (and (= :west direction) (= :facing-away attr))     attr
      (and (= :west direction) (= :facing-toward attr))   :east
      (and (= :west direction) (= :facing-left attr))     :south
      (and (= :west direction) (= :facing-right attr))    :north

      :else
      attr)))

(defn automata:place [state material0 & [default-material]]
  (let [block             (.getBlockAt (:world state) (:loc state))
        [material1 attrs] (core/parse-material (or material0 default-material :smooth-stone))]
    (log/infof "automata:place: placing %s at %s (material1=%s; default-material=%s)" material1 (:loc state) material0 default-material)
    (.setType block (core/->material material1))
    (when-not (empty? attrs)
      (doseq [attr (map #(attr-relative-to-absolute (:loc state) %) attrs)]
        (core/apply-block-attr block attr))))
  state)

(comment
  (def block1 (.getBlock (.getLocation (.getTargetBlockExact (core/->player "DominusSermonis") 9999))))
  block1
  (.getData block1)
  (class (.getType block1))
  org.bukkit.Material

  (.getFacing (.getBlockData block1))
  ;; org.bukkit.block.BlockFace
  (.getType (.getBlockData block1))

  org.bukkit.block.data.type.Slab$Type/TOP

  (.setType
   (.getBlockData block1)
   org.bukkit.block.data.type.Slab$Type/BOTTOM)

  (core/schedule!
   (let [block-data (.getBlockData block1)]
     (.setType block-data org.bukkit.block.data.type.Slab$Type/TOP)
     (.setBlockData block1 block-data)))


  ;; https://www.spigotmc.org/threads/get-if-slab-is-positioned-bottom-or-top.451406/
  (.isTagged org.bukkit.Tag/SLABS (.getType block1))


  )

(defn automata:clear [state]
  (let [block (.getBlockAt (:world state) (:loc state))]
    (.setType block (core/->material :air)))
  state)

(defn automata:up-one [state]
  (update-in state [:loc] core/loc-up 1))

(defn automata:down-one [state]
  (update-in state [:loc] core/loc-up -1))

(defn automata:turn-left [state & args]
  (update-in state [:loc] core/loc-turn-left))

(defn automata:left [state & args]
  (update-in state [:loc] core/loc-left (first args)))

(defn automata:turn-right [state & args]
  (update-in state [:loc] core/loc-turn-right))

(defn automata:right [state & args]
  (update-in state [:loc] core/loc-right (first args)))

(defn automata:turn-around [state & args]
  (->
   state
   (update-in [:loc] core/loc-turn-left)
   (update-in [:loc] core/loc-turn-left)))

(defn automata:define-pattern [state name & args]
  (log/infof "automata:define-pattern: name=%s || %s" name args)
  (assoc-in state [:patterns name] args))

(defn automata:apply-pattern [state pattern & args]
  (log/infof "automata:apply-pattern: pattern=%s || %s" pattern (-> state :patterns (get pattern)))
  (dissoc
   (automata
    (assoc state :pattern pattern)
    (-> state :patterns (get pattern)))
   :pattern))

(defn automata:set-var [state vname value & args]
  (assoc-in state [:vars vname] value))

(defn automata:push-loc [state & args]
  (update-in state [:loc-stack] conj (:loc state)))

(defn automata:peek-loc [state & args]
  (assoc state
         :loc       (or (-> state :loc-stack first) (:loc state))))

(defn automata:pop-loc [state & args]
  (assoc state
         :loc       (or (-> state :loc-stack first) (:loc state))
         :loc-stack (-> state :loc-stack rest)))

(defn automata:inventory-add-item [state material & args]
  (let [block       (.getBlockAt (:world state) (:loc state))
        block-state (.getState block)
        inventory   (.getInventory block-state)]
    (def block block)
    (def block-state block-state)
    (def inventory inventory)
    (.addItem
     inventory
     (into-array
      org.bukkit.inventory.ItemStack
      [(org.bukkit.inventory.ItemStack.
        (core/->material material))])))
  state)

(def automata-functions
  {:forward            #'automata:forward
   :backward           #'automata:backward
   :repeat             #'automata:repeat
   :branch             #'automata:branch
   :place              #'automata:place
   :clear              #'automata:clear
   :up-one             #'automata:up-one
   :down-one           #'automata:down-one
   :turn-left          #'automata:turn-left
   :left               #'automata:left
   :turn-right         #'automata:turn-right
   :right              #'automata:right
   :define             #'automata:define-pattern
   :pattern            #'automata:apply-pattern
   :set                #'automata:set-var
   :push-loc           #'automata:push-loc
   :pop-loc            #'automata:pop-loc
   :peek-loc           #'automata:peek-loc
   :inventory-add-item #'automata:inventory-add-item})

(defn automata-compound? [action]
  (and (vector? action)
       (-> action first keyword?)))

(defn automata-lookup-fn [action]
  (def action action)
  (cond
    (keyword? action)
    [(automata-functions action)]

    (and (vector? action)
         (-> action first keyword?))
    (let [[action-fn-name & args] action
          action-fn               (automata-functions action-fn-name)]
      (when action-fn
        [action-fn args]))

    :else
    (throw (RuntimeException. (format "automata-lookup-fn: unrecognized action=%s" action)))))

(defn automata-resolve-vars [state form]
  (cond
    (and
     (vector? form)
     (-> form first (= :get)))
    (-> state :vars (get (second form)))

    :else
    form))

(defn automata [state actions]
  (def state state)
  (def actions actions)
  ;; todo: convert this to a reduce / trampoline
  ;; - convert (map) actions into a seq of [:action action-fn args]
  ;; - then reduce it
  (loop [[action & actions] actions
         state              state]
    (cond
      (not action)
      state

      :else
      (let [[action-fn args] (automata-lookup-fn action)]
        (log/infof "automata: action=%s; loc=%s" action (:loc state))
        (when-not action-fn
          (throw (RuntimeException. (format "automata: error: unrecognized action=%s" action))))
        (recur actions (apply action-fn state (map #(automata-resolve-vars state %) args)))))))

(defn clear-contiguous-blocks [loc limit]
  (loop [locs (->> (core/adjacent-locations loc)
                   (filter #(not (core/=material? % :air))))
         ii   0]
    (cond
      (>= ii limit)
      :hit-limit

      (not locs)
      :done

      :else
      (do
        (doseq [loc locs]
          (log/infof "clear-contiguous-blocks: loc=%s material=%s" loc (core/->material loc))
          (.setType (core/->block loc) (core/->material :air)))
        (recur
         (->>
          locs
          (mapcat core/adjacent-locations)
          (filter #(not (core/=material? % :air))))
         (inc ii))))))

(defn clear-structure-at-crosshirs!
  ([player]
   (clear-structure-at-crosshirs! player 1024 9999))
  ([player max-size]
   (clear-structure-at-crosshirs! player max-size 9999))
  ([player max-size max-dist]
   (core/schedule!
    (clear-contiguous-blocks
     (.getLocation (.getTargetBlockExact (core/->player player) max-dist))
     max-size))))

(comment
  (core/schedule!
   (.teleport (core/get-player-named "DominusSermonis")
              (core/->loc [199 -59 1319 10.049989 -178.95055])))

  (clear-structure-at-crosshirs "DominusSermonis" 1024 9999)

  (core/schedule!
   (automata
    ;; (-> "DominusSermonis" core/->loc core/location-to-xyzpy)[199 -59 1319 10.049989 -178.95055]
    ;; state
    ;; (.getDirection (core/->loc [199 -59 1319 10.049989 -178.95055]))
    {:world    (core/overworld)
     :loc      (core/->loc [199 -59 1319 10.049989 -178.95055])
     :patterns {}
     :vars     {}}
    ;; instructions
    [ ;; ...
     ;; - side wall 3 high
     ;; - spawn platform / channel, 7 then 8, 2 wide
     ;; - side wall 3 high
     [:define
      :spawn-floor-wall
      [:repeat 3
       [:branch
        [:pattern :spawn-floor-line]]
       :up-one]
      [:set :material :smooth-stone-slab]
      [:branch
       ;; NB: for the "roof"
       [:pattern :spawn-floor-line]]]
     [:define
      :spawn-floor-line
      [:repeat 3
       [:branch
        [:repeat 7
         [:place [:get :material] :smooth-stone]
         :forward]
        :backward
        :up-one
        [:repeat 10
         [:place [:get :material] :smooth-stone]
         :forward]]]]

     :forward
     :forward
     [:repeat 8 :up-one]

     ;; start
     [:branch
      [:pattern :spawn-floor-wall]]
     [:repeat 4
      [:branch
       [:pattern :spawn-floor-line]]
      :left]]))


  (core/schedule!
   (automata
    ;; (-> "DominusSermonis" core/->loc core/location-to-xyzpy)[199 -59 1319 10.049989 -178.95055]
    ;; state
    ;; (.getDirection (core/->loc [199 -59 1319 10.049989 -178.95055]))
    {:world    (core/overworld)
     :loc      (core/->loc [199 -59 1319 10.049989 -178.95055])
     :patterns {}
     :vars     {}}
    ;; instructions
    [
     [:define :wall
      [:branch
       [:repeat 3
        [:branch
         [:repeat 7
          [:place :smooth-stone]
          :forward]
         :up-one
         [:repeat 8
          [:place :smooth-stone]
          :forward]]
        :up-one]
       [:branch
        [:repeat 5
         [:place :smooth-stone-slab]
         :forward]
        [:place :smooth-stone]
        :forward
        [:place :smooth-stone]
        :up-one
        [:repeat 9
         [:place :smooth-stone-slab]
         :forward]]]]

     ;; floor
     [:define :floor
      [:repeat 2
       :left
       [:branch
        [:repeat 7
         [:place :smooth-stone]
         :forward]
        :up-one
        :backward
        [:place :smooth-stone-slab]
        :forward
        [:repeat 10
         [:place :smooth-stone]
         :forward]
        :backward
        :backward
        :up-one
        [:place :dispenser.facing-toward]
        [:inventory-add-item :water-bucket]
        :up-one
        [:place :smooth-stone]]]]

     ;; start position
     :forward
     :forward
     [:repeat 8 :up-one]
     :push-loc

     ;; left-wall
     [:pattern :wall]
     [:pattern :floor]

     ;; ceiling
     :peek-loc
     :left
     [:repeat 3 :up-one]
     [:repeat 2
      [:branch
       [:repeat 5 [:place :smooth-stone-slab] :forward]
       [:place :smooth-stone-slab.top]
       :forward
       :up-one
       [:repeat 9 [:place :smooth-stone-slab] :forward]]
      :left]
     ;; left-wall
     :peek-loc
     :left
     :left
     :left
     [:pattern :wall]]))

  (do
    (core/set-world-time! :morning)
    (clear-structure-at-crosshirs! "DominusSermonis" 1024 9999))

  )
