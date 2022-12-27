(ns krb-minerepl-bukkit.core
  (:require
   [clojure.tools.logging  :as log]
   [camel-snake-kebab.core :as csk]))

(comment

  (.getSeed (overworld))
  ;; -8944630491960636560

  (.getWorldType (overworld))
  (.getDifficulty (overworld))
  (.getPlayers (overworld))

  )


;; M-x cider-connect 4123
;; https://hub.spigotmc.org/javadocs/bukkit/index.html?org/bukkit/entity/package-summary.html

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defn logback-configure! [xml-config-file-path]
  (let [logger-factory     (org.slf4j.LoggerFactory/getILoggerFactory)
        joran-configurator (ch.qos.logback.classic.joran.JoranConfigurator.)]
    (.setContext joran-configurator logger-factory)
    (.doConfigure joran-configurator xml-config-file-path)))

(comment
  (logback-configure! "/Users/kburton/code/github.com/kyleburton/mason-craft/spigot/krb-minerepl-bukkit/resources/logback.xml")
  (logback-configure! "/home/kyle/code/github.com/kyleburton/mason-craft/spigot/krb-minerepl-bukkit/resources/logback.xml")
  )

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(def plugin-instance (atom nil))

(defn set-plugin! [p]
  (reset! plugin-instance p))

(defn get-minerepl-plugin []
  (->>
   (org.bukkit.Bukkit/getServer)
   .getPluginManager
   .getPlugins
   (filter #(= "krbminerepl" (.getName %)))
   first))

(defn plugin []
  (when (nil? @plugin-instance)
    (set-plugin! (get-minerepl-plugin)))
  @plugin-instance)

(comment
  org.bukkit.plugin.Plugin
  (.getServer (plugin))

  (.getDefaultGameMode (.getServer (plugin)))

  (.getPluginManager (org.bukkit.Bukkit/getServer))

  ;; registerEvent​(@NotNull Class<? extends Event> event, @NotNull Listener listener, @NotNull EventPriority priority, @NotNull EventExecutor executor, @NotNull Plugin plugin)
  (org.bukkit.event.player.PlayerInteractEvent.)
  (.registerEvent
   (.getPluginManager (org.bukkit.Bukkit/getServer))
   )


  )

(def overworld-instance (atom nil))
(defn overworld []
  (when (nil? @overworld-instance)
    (reset! overworld-instance
            (-> (org.bukkit.Bukkit/getWorlds) first)))
  @overworld-instance)


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defmacro after [stime & body]
  `(.run
    (Thread.
     (fn []
       (Thread/sleep ~stime)
       ((fn [] ~@body))))))

(defn all-players []
  (org.bukkit.Bukkit/getOnlinePlayers))

(defn get-player-named [name]
  (org.bukkit.Bukkit/getPlayer name))

(defn player? [thing]
  (and thing (isa? (class thing) org.bukkit.entity.Player))  )

(defn location? [thing]
  (isa? (class thing) org.bukkit.Location))

(defn block? [thing]
  (isa? (class thing) org.bukkit.block.Block))

(defn material? [thing]
  (isa? (class thing) org.bukkit.Material))

(defn ->player [name-or-player]
  (cond
    (player? name-or-player)
    name-or-player

    (string? name-or-player)
    (get-player-named name-or-player)

    :else
    (throw (RuntimeException. (format "->player: unrecognized name-or-player=%s/%s" (class name-or-player) name-or-player)))))

(defn ->loc [player-or-loc]
  (cond
    (nil? player-or-loc)
    nil

    (player? player-or-loc)
    (.getLocation player-or-loc)

    (string? player-or-loc)
    (-> player-or-loc ->player .getLocation)

    (location? player-or-loc)
    player-or-loc

    (vector? player-or-loc)
    (let [[xx yy zz] player-or-loc]
      (org.bukkit.Location.
       (overworld) ;; nb: this is an assumption
       xx
       yy
       zz))

    :else
    (throw (RuntimeException. (format "->loc: unrecognized player-or-loc=%s/%s" (class player-or-loc) player-or-loc)))))


;; X is east (pos) / west (neg)
;; Y is height
;; Z is north (neg) / south (pos)
(defn location-to-xyzpy [loc]
  [(.getBlockX loc)
   (.getBlockY loc)
   (.getBlockZ loc)
   (org.bukkit.Location/normalizePitch (.getPitch loc))
   (org.bukkit.Location/normalizeYaw (.getYaw loc))])

(defn location-to-xyz [loc]
  [(.getBlockX loc)
   (.getBlockY loc)
   (.getBlockZ loc)])

(defn location-to-xz [loc]
  [(.getBlockX loc)
   (.getBlockZ loc)])

(defn get-player-location [^String name]
  (location-to-xyzpy (.getLocation (get-player-named name))))

;; (.getLocation (get-player-named "kyle_burton"))

(defn get-player-loc-xz [^String name]
  (let [loc (.getLocation (get-player-named name))]
    [(.getBlockX loc)
     (.getBlockZ loc)]))

(defn get-player-loc-xyz [^String name]
  (let [loc (.getLocation (get-player-named name))]
    [(.getBlockX loc)
     (.getBlockY loc)
     (.getBlockZ loc)]))

(comment

  (->
   "kyle_burton"
   get-player-named
   (.setInvulnerable true))

  )

(defn chat-to-player [name & messages]
  (let [player (get-player-named name)]
    (doseq [message messages]
      (.sendMessage player message))))

(defn inc-loc-dir [loc ^String dir]
  ;; org.bukkit.Location
  (let [new-loc (org.bukkit.Location.
                 (.getWorld loc)
                 (.getX loc)
                 (.getY loc)
                 (.getZ loc))]
    (cond
      (= dir "SOUTH")
      ;; :+1z
      (.setZ loc (inc (.getZ loc)))

      (= dir "WEST")
      ;; :-1x
      (.setX loc (dec (.getX loc)))
      (= dir "NORTH")
      ;; :-1z
      (.setZ loc (dec (.getZ loc)))
      (= dir "EAST")
      ;; :+1x
      (.setX loc (inc (.getX loc)))

      :else
      (throw (RuntimeException. (format "Error: invalid direction=%s" dir))))
    new-loc))

(defn loc->coords [loc]
  (cond
    (vector? loc)
    loc

    (location? loc)
    [(.getX loc) (.getY loc) (.getZ loc)]

    :else
    (throw (RuntimeException. (format "loc->coords unsure how to get [x,y,z] from loc=%s" loc)))))

(defn loc+ [& locations]
  (let [[xx yy zz] (->>
                    locations
                    (map loc->coords)
                    (reduce
                     (fn [[x1 y1 z1] [x2 y2 z2]]
                       [(+ x1 x2)
                        (+ y1 y2)
                        (+ z1 z2)])))]
    (org.bukkit.Location.
     ;; TODO: how can we factor out overworld/nether/end? ... and keep things convienient
     (overworld)
     xx
     yy
     zz
     0 ;; yaw
     0 ;; pitch
     )))

(defn loc- [& locations]
  (let [[xx yy zz] (->>
                    locations
                    (map loc->coords)
                    (reduce
                     (fn [[x1 y1 z1] [x2 y2 z2]]
                       [(- x1 x2)
                        (- y1 y2)
                        (- z1 z2)])))]
    (org.bukkit.Location.
     (overworld)
     xx
     yy
     zz
     0 ;; yaw
     0 ;; pitch
     )))

(defn loc* [loc scale]
  (let [loc        (->loc loc)
        xx (.getX loc)
        yy (.getY loc)
        zz (.getZ loc)]
    [(* xx scale)
     (* yy scale)
     (* zz scale)]))

(defn loc+-x [loc ii]
  (map
   (fn [ii]
     [(+ ii (.getX loc))
      (.getY loc)
      (.getZ loc)])
   (range (* -1 ii) (inc ii))))

(defn loc+-y [loc ii]
  (map
   (fn [ii]
     [(.getX loc)
      (+ ii (.getY loc))
      (.getZ loc)])
   (range (* -1 ii) (inc ii))))

(defn loc+-z [loc ii]
  (map
   (fn [ii]
     [(.getX loc)
      (.getY loc)
      (+ ii (.getZ loc))])
   (range (* -1 ii) (inc ii))))

(def cardinal-directions
  {:north                                      :north
   :east                                       :east
   :south                                      :south
   :west                                       :west
   :up                                         :up
   :down                                       :down
   org.bukkit.block.BlockFace/NORTH            :north
   org.bukkit.block.BlockFace/EAST             :east
   org.bukkit.block.BlockFace/SOUTH            :south
   org.bukkit.block.BlockFace/WEST             :west
   org.bukkit.block.BlockFace/UP               :up
   org.bukkit.block.BlockFace/DOWN             :down
   org.bukkit.block.BlockFace/NORTH_EAST       :north
   org.bukkit.block.BlockFace/NORTH_WEST       :north
   org.bukkit.block.BlockFace/SOUTH_EAST       :south
   org.bukkit.block.BlockFace/SOUTH_WEST       :south
   org.bukkit.block.BlockFace/WEST_NORTH_WEST  :west
   org.bukkit.block.BlockFace/NORTH_NORTH_WEST :north
   org.bukkit.block.BlockFace/NORTH_NORTH_EAST :north
   org.bukkit.block.BlockFace/EAST_NORTH_EAST  :east
   org.bukkit.block.BlockFace/EAST_SOUTH_EAST  :east
   org.bukkit.block.BlockFace/SOUTH_SOUTH_EAST :south
   org.bukkit.block.BlockFace/SOUTH_SOUTH_WEST :south
   org.bukkit.block.BlockFace/WEST_SOUTH_WEST  :west})

(def standard-directions
  {:north                                      :north
   :east                                       :east
   :south                                      :south
   :west                                       :west
   :up                                         :up
   :down                                       :down
   org.bukkit.block.BlockFace/NORTH            :north
   org.bukkit.block.BlockFace/EAST             :east
   org.bukkit.block.BlockFace/SOUTH            :south
   org.bukkit.block.BlockFace/WEST             :west
   org.bukkit.block.BlockFace/UP               :up
   org.bukkit.block.BlockFace/DOWN             :down
   org.bukkit.block.BlockFace/NORTH_EAST       :north-east
   org.bukkit.block.BlockFace/NORTH_WEST       :north-west
   org.bukkit.block.BlockFace/SOUTH_EAST       :south-east
   org.bukkit.block.BlockFace/SOUTH_WEST       :south-west
   org.bukkit.block.BlockFace/WEST_NORTH_WEST  :north-west
   org.bukkit.block.BlockFace/NORTH_NORTH_WEST :north-west
   org.bukkit.block.BlockFace/NORTH_NORTH_EAST :north-east
   org.bukkit.block.BlockFace/EAST_NORTH_EAST  :north-east
   org.bukkit.block.BlockFace/EAST_SOUTH_EAST  :south-east
   org.bukkit.block.BlockFace/SOUTH_SOUTH_EAST :south-east
   org.bukkit.block.BlockFace/SOUTH_SOUTH_WEST :south-west
   org.bukkit.block.BlockFace/WEST_SOUTH_WEST  :south-west})

(def direction-unit-offsets
  (let [offsets {org.bukkit.block.BlockFace/NORTH            [ 0  0 -1]
                 org.bukkit.block.BlockFace/EAST             [ 1  0  0]
                 org.bukkit.block.BlockFace/SOUTH            [ 0  0  1]
                 org.bukkit.block.BlockFace/WEST             [-1  0  0]
                 org.bukkit.block.BlockFace/UP               [ 0  1  0]
                 org.bukkit.block.BlockFace/DOWN             [ 0 -1  0]
                 org.bukkit.block.BlockFace/NORTH_EAST       [ 1  0 -1]
                 org.bukkit.block.BlockFace/NORTH_WEST       [-1  0 -1]
                 org.bukkit.block.BlockFace/SOUTH_EAST       [ 1  0  1]
                 org.bukkit.block.BlockFace/SOUTH_WEST       [-1  0  1]
                 org.bukkit.block.BlockFace/WEST_NORTH_WEST  [-1  0 -1]
                 org.bukkit.block.BlockFace/NORTH_NORTH_WEST [-1  0 -1]
                 org.bukkit.block.BlockFace/NORTH_NORTH_EAST [ 1  0 -1]
                 org.bukkit.block.BlockFace/EAST_NORTH_EAST  [ 1  0 -1]
                 org.bukkit.block.BlockFace/EAST_SOUTH_EAST  [ 1  0  1]
                 org.bukkit.block.BlockFace/SOUTH_SOUTH_EAST [ 1  0  1]
                 org.bukkit.block.BlockFace/SOUTH_SOUTH_WEST [-1  0  1]
                 org.bukkit.block.BlockFace/WEST_SOUTH_WEST  [-1  0  1]}]
    (assoc
     offsets
     :up    (get offsets org.bukkit.block.BlockFace/UP)
     :down  (get offsets org.bukkit.block.BlockFace/DOWN)
     :north (get offsets org.bukkit.block.BlockFace/NORTH)
     :east  (get offsets org.bukkit.block.BlockFace/EAST)
     :south (get offsets org.bukkit.block.BlockFace/SOUTH)
     :west  (get offsets org.bukkit.block.BlockFace/WEST))))


(defn loc+direction
  ([loc block-face]
   (loc+ loc (direction-unit-offsets block-face)))
  ([loc block-face scale]
   (let [[x y z] (direction-unit-offsets block-face)
         offset  [(* x scale)
                  (* y scale)
                  (* z scale)]]
     (loc+ loc offset))))

(defn player-facing-axis [player-or-name]
  (let [direction (-> player-or-name ->player .getFacing)]
    (cond
      (= direction org.bukkit.block.BlockFace/NORTH) :z
      (= direction org.bukkit.block.BlockFace/SOUTH) :z
      (= direction org.bukkit.block.BlockFace/EAST)  :x
      (= direction org.bukkit.block.BlockFace/WEST)  :x)))

(defn player-perpendicular-axis [player-or-name]
  (let [direction (-> player-or-name ->player .getFacing)]
    (cond
      (= direction org.bukkit.block.BlockFace/NORTH) :x
      (= direction org.bukkit.block.BlockFace/SOUTH) :x
      (= direction org.bukkit.block.BlockFace/EAST)  :z
      (= direction org.bukkit.block.BlockFace/WEST)  :z)))

(defn left-perpendicular-offset [player-or-name]
  (let [direction (.getFacing (->player player-or-name))]
    (= direction (= direction org.bukkit.block.BlockFace/NORTH)) [-1  0 0]
    (= direction (= direction org.bukkit.block.BlockFace/SOUTH)) [ 1  0 0]
    (= direction (= direction org.bukkit.block.BlockFace/EAST))  [ 0  0 -1]
    (= direction (= direction org.bukkit.block.BlockFace/WEST))  [ 0  0  1]))

(defn right-perpendicular-offset [player-or-name]
  (let [direction (.getFacing (->player player-or-name))]
    (= direction org.bukkit.block.BlockFace/NORTH) [ 1  0 0]
    (= direction org.bukkit.block.BlockFace/SOUTH) [-1  0 0]
    (= direction org.bukkit.block.BlockFace/EAST)  [ 0  0  1]
    (= direction org.bukkit.block.BlockFace/WEST)  [ 0  0 -1]))


(comment

  ;; (import 'com.github.kyleburton.krb_minerepl_bukkit.REPL)
  ;; com.github.kyleburton.krb_minerepl_bukkit.REPL
  ;; com.github.kyleburton.krb_minerepl_bukkit.REPL/server
  (java.lang.Class/forName  "com.github.kyleburton.krb_minerepl_bukkit.REPL")
  )


(defn overworld-live-entities []
  (->>
   (overworld)
   .getEntities
   (filter #(.isSpawnable (.getType %)))))

(defn send-em-all-there! [loc]
  (doseq [thing (overworld-live-entities)]
    (try
      (.teleport thing loc)
      (catch Exception ex
        (.println System/out (format "err: ex=%s" ex))))))

(defn light-em-up! []
  (doseq [thing (overworld-live-entities)]
    (.setFireTicks thing 1000)))

(defn scheduler []
  (->
   (org.bukkit.Bukkit/getServer)
   .getScheduler))

(defn schedule-fn! [f delay]
  (->
   (scheduler)
   (.scheduleSyncDelayedTask
    (plugin)
    f
    delay)))

(defmacro schedule! [& body]
  `(->
    (scheduler)
    (.runTask
     (plugin)
     (fn []
       (try
         (do ~@body)
         (catch Exception e#
           (log/infof e# "Error: e=%s" e#)))))))

(defmacro schedule-later! [delay & body]
  `(->
    (scheduler)
    (.runTaskLater
     (plugin)
     (fn []
       (try
         (do ~@body)
         (catch Exception e#
           (log/infof e# "Error: e=%s" e#))))
     ~delay)))

(defmacro schedule-sync! [& body]
  `(->
    (scheduler)
    (.scheduleSyncDelayedTask
     (plugin)
     (fn []
       (try
         (do ~@body)
         (catch Exception e#
           (log/infof e# "Error: e=%s" e#)))))))

(defn schedule-seq! [elts func]
  (let [scheduler (scheduler)
        recfunc   (fn rfunc [elts]
                    (when-not (empty? elts)
                      (.scheduleSyncDelayedTask
                       scheduler
                       (plugin)
                       (fn []
                         (func (first elts))
                         (rfunc (rest elts)))
                       0)))]
    (recfunc elts)))

(defn make-tower-of [base-location type height]
  (schedule!
   (dotimes [ii height]
     (->
      (.getBlockAt
       (overworld)
       (loc+
        base-location
        ;; x is east/west
        ;; y is hieght
        ;; z is north/south
        [0 ii 0 0 0]))
      (.setType type)))))

;; TODO: catling-gun

;; NB: 1000 ticks is an hour in-game
;;     0 is 6am
;;  1000 is 7am  / morning
;;  6000 is 12pm / noon
;; 12000 is  6pm / sunset
;; 18000 is 12am / midnight
;; 23000 is  5am / sunrise
(defn set-world-time-abs! [^long ticks]
  (schedule! (.setFullTime (overworld) ticks)))

(defn rewind-world-time-abs! [^long ticks]
  (let [new-time (-> (overworld) .getFullTime (- ticks))]
    (set-world-time-abs! new-time)))

(defn forward-world-time-abs! [^long ticks]
  (let [new-time (-> (overworld) .getFullTime (+ ticks))]
    (set-world-time-abs! new-time)))

(defn set-world-time! [time-keyword]
  (cond
    (= :morning time-keyword)
    (set-world-time-abs! 1000)

    (= :noon time-keyword)
    (set-world-time-abs! 6000)

    (= :sunset time-keyword)
    (set-world-time-abs! 12000)

    (= :midnight time-keyword)
    (set-world-time-abs! 18000)

    (= :sunrise time-keyword)
    (set-world-time-abs! 23000)

    :else
    (throw (RuntimeException. (format "Error: unrecognized time=%s" time-keyword)))))


(defn get-highest-block-y-at
  ([thing]
   (let [loc (->loc thing)]
     (.getHighestBlockYAt (.getWorld loc) (-> loc .getX int) (-> loc .getZ int))))

  ([xx zz]
   (.getHighestBlockYAt (overworld) (int xx) (int zz))))

(defn get-highest-block-at [loc]
  (.getBlockAt (.getWorld loc) (.getX loc) (get-highest-block-y-at loc) (.getZ loc)))

(defn round [val]
  (cond
    (int? val)
    val

    :else
    (Math/round val)))

(defn get-block-at
  ([xx yy zz]
   (.getBlockAt (overworld) (round xx) (round yy) (round zz)))
  ([arg]
   (cond
     (and
      (vector? arg)
      (= 3 (count arg)))
     (get-block-at (nth arg 0) (nth arg 1) (nth arg 2))

     (location? arg)
     (.getBlockAt (overworld) (.getX arg) (.getY arg) (.getZ arg))

     :else
     (throw (RuntimeException. (format "get-block-at: unsure what arg=%s is" arg))))))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; materials helpers

(comment

  (.name (first (vec (.getEnumConstants org.bukkit.Material))))
  )

(def materials
  (reduce
   (fn [acc material]
     (assoc acc
            (-> material .name) material
            (-> material .name .toLowerCase) material
            (-> material .name .toLowerCase keyword) material
            (-> material .name .toLowerCase csk/->kebab-case keyword) material))
   {}
   (.getEnumConstants org.bukkit.Material)))

(defn ->material [thing]
  (cond
    (material? thing)
    thing

    (block? thing)
    (.getType thing)

    (location? thing)
    (.getType (.getBlock thing))

    (or (keyword? thing) (string? thing))
    (materials thing)

    (and (vector? thing)
         (= 3 (count thing)))
    (.getType (apply get-block-at thing))

    :else
    (throw (RuntimeException. (format "->material?: unsure what thing=%s is" thing)))))

(defn =material? [thing1 & things]
  (loop [thing1            (->material thing1)
         [thing2 & things] (map ->material things)]
    (cond
      (not thing1)
      false

      (not thing2)
      false

      (= thing1 thing2)
      true

      :else
      (recur thing2 things))))

(def materials-affected-by-gravity
  (reduce
   #(conj %1 %2)
   #{}
   (->>
    (.getEnumConstants org.bukkit.Material)
    (filter #(.hasGravity %))
    vec)))

(defn material-affected-by-gravity? [thing]
  (contains? materials-affected-by-gravity (->material thing)))

(comment

  (->>
   (.getEnumConstants org.bukkit.Material)
   (filter #(.hasGravity %))
   vec)

  )

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(comment
  (set-world-time! :sunrise)
  (set-world-time! :morning)
  (set-world-time! :sunset)

  (rewind-world-time-abs!  1000)
  (forward-world-time-abs! 1000)

  (schedule!
   (doseq [entity (overworld-live-entities)]
     ;; bring 'em here
     ;; make a chicken
     ;; add the entity as a passenger of the chicken
     (let [wheres-kyle (.getLocation (get-player-named "kyle_burton"))
           near-kyle   (loc+ wheres-kyle [(int (rand 10)) 5 (int (rand 10))])
           ;; chicken     (.spawnEntity (overworld) near-kyle org.bukkit.entity.EntityType/CHICKEN)
           ]
       (.teleport entity near-kyle)
       #_(.addPassenger chicken entity)
       #_(.setFireTicks entity 1000))))

  (make-tower-of
   (loc+ (.getLocation (get-player-named "kyle_burton")) [2 0 2])
   org.bukkit.Material/ANDESITE
   10)

  (make-tower-of
   (loc+ (.getLocation (get-player-named "kyle_burton")) [2 0 2])
   org.bukkit.Material/WITHER_SKELETON_SKULL
   10)

  (make-tower-of
   (loc+ (.getLocation (get-player-named "kyle_burton")) [2 0 2])
   org.bukkit.Material/TUBE_CORAL
   10)

  (make-tower-of
   (loc+ (.getLocation (get-player-named "kyle_burton")) [2 0 2])
   org.bukkit.Material/AIR
   10)

  (org.bukkit.Material/values)

  (schedule! (chat-to-player "kyle_burton" "hey hey"))

  (after 2000
         (schedule!
          (let [wheres-kyle (.getLocation (get-player-named "kyle_burton"))
                near-kyle   (loc+ wheres-kyle [1 0 1])]
            (dotimes [ii 10]
              (let [cat (.spawnEntity (overworld) near-kyle org.bukkit.entity.EntityType/CAT)]
                (.setVelocity cat (org.bukkit.util.Vector. 2 2 2)))
              ;;(chat-to-player "kyle_burton" (format "cat: %s" ii))
              ))))

  (after 2000
         (schedule!
          (let [wheres-kyle (.getLocation (get-player-named "kyle_burton"))
                near-kyle   (loc+ wheres-kyle [1 0 1])]
            (dotimes [ii 10]
              (let [creeper (.spawnEntity (overworld) near-kyle org.bukkit.entity.EntityType/CREEPER)
                    loc  (loc+ near-kyle [ii 0 ii])
                    chicken (.spawnEntity (overworld) near-kyle org.bukkit.entity.EntityType/CHICKEN)]
                (.setVelocity creeper (org.bukkit.util.Vector. 1 1 1))
                (.setPowered creeper true)
                (.setMaxFuseTicks creeper 100)
                (.setPassenger chicken creeper)
                (def creep1 creeper))))))

  (loop [ii 10]
    (cond
      (= 0 ii)
      :done

      :otherwise
      (do
        (schedule!
         (let [wheres-kyle (.getLocation (get-player-named "kyle_burton"))
               near-kyle   (loc+ wheres-kyle [1 0 1])]
           (let [cat (.spawnEntity (overworld) near-kyle org.bukkit.entity.EntityType/CAT)]
             (.setVelocity cat (org.bukkit.util.Vector. 2 2 2))
             (chat-to-player "kyle_burton" (format "cat: %s" ii)))))
        (recur (dec ii)))))

  (overworld-live-entities)
  (.setWeatherDuration (overworld) 1)
  (.setFireTicks (get-player-named "kyle_burton") 0)

  (.getFullTime (overworld))
  (.setFullTime (overworld) 0)     ;; morning
  (.setFullTime (overworld) 10000) ;; afternoon
  (.setFullTime (overworld) 20000) ;;
  (.setFullTime (overworld) -100)

  (set-world-time-abs! 30000)

  ;; 96483

  (let [wheres-kyle (.getLocation (get-player-named "kyle_burton"))
        near-kyle   (loc+ wheres-kyle [10 10 10])]
    ;; make sure it's not raining
    (after 2000
           (do
             (send-em-all-there! near-kyle)
             (chat-to-player "kyle_burton" "brought 'em all near ya")
             (light-em-up!)
             (chat-to-player "kyle_burton" "set 'em on fire"))))

  (after 2000 (light-em-up!))

  (chat-to-player "kyle_burton" "yo" "stuff is stuff")

  (def players (org.bukkit.Bukkit/getOnlinePlayers))

  (def kyle (org.bukkit.Bukkit/getPlayer "kyle_burton"))

  (-> players
      first
      (.sendMessage "yo yo"))

  (def thing1
    (-> (overworld)
        .getEntities
        first))

  (seq (.getEffectivePermissions thing1))

  (.getMaxFireTicks thing1)
  ;; getType
  ;; isAlive
  ;; isSpawnable

  ;; .teleport
  ;; .getLocation

  (.getLocation thing1)

  (.getLocation kyle)

  (let [wheres-kyle (.getLocation kyle)]
    (.setZ wheres-kyle (+ 3 (.getZ wheres-kyle)))
    (.teleport thing1 wheres-kyle))

  (doseq [thing (-> (overworld)
                    .getEntities)]
    (let [wheres-kyle (.getLocation kyle)]
      (.setZ wheres-kyle (+ 3 (.getZ wheres-kyle)))
      (try
        (.teleport thing wheres-kyle)
        (catch Exception ex
          (.println System/out (format "err: ex=%s" ex))))))

  )

(defn horiz-coords-around [[origin-xx origin-yy origin-zz] to-dist]
  (for [xx   (->> (range (* -1 to-dist) to-dist)
                  (map int)
                  (map #(+ % (int origin-xx))))
        zz   (->> (range (* -1 to-dist) to-dist)
                  (map int)
                  (map #(+ % (int origin-zz))))]
    [xx origin-yy zz]))

;; (horiz-coords-around (get-player-loc-xyz "kyle_burton") 2)

;; in a square, out from the locaiton of the player, anywhere the
;; light level is under 8 place a torch
(defn place-torches! [origin to-dist]
  (let [place-fn (fn placef [coords]
                   (when-not (empty? coords)
                     (let [[xx _ zz] (first coords)
                           yy        (inc (.getHighestBlockYAt (overworld) xx zz))
                           block     (.getBlockAt (overworld) xx yy zz)]
                       (if (and (= org.bukkit.Material/AIR (.getType block))
                                (< (.getLightLevel block) 8))
                         (do
                           (log/infof "place-torches: PLACING torch at [%s,%s,%s] light-level=%s" xx yy zz (.getLightLevel block))
                           (schedule-fn!
                            (fn []
                              (.setType block org.bukkit.Material/TORCH)
                              (placef (rest coords)))
                            1)
                           (.setType block org.bukkit.Material/TORCH))
                         (do
                           (log/infof "place-torches: NOT PLACING TORCH at [%s,%s,%s] light-level=%s" xx yy zz (.getLightLevel block))
                           (placef (rest coords)))))))]

    (place-fn (horiz-coords-around origin to-dist))))


(comment
  (set-world-time! :midnight)

  (place-torches! (get-player-loc-xyz "kyle_burton") 1)

  (place-torches! (get-player-loc-xyz "kyle_burton") 10)

  (place-torches! (get-player-loc-xyz "kyle_burton") 10)

  (.setFlying (get-player-named "kyle_burton") true)
  (.isFlying (get-player-named "kyle_burton"))

  (schedule-seq!
   (horiz-coords-around (get-player-loc-xyz "kyle_burton") 10)
   (fn [[xx _ zz]]
     (let [yy    (inc (.getHighestBlockYAt (overworld) xx zz))
           block (.getBlockAt (overworld) xx yy zz)]
       (when (= org.bukkit.Material/TORCH (.getType block))
         (log/infof "removing torch at %s" [xx yy zz])
         (.setType block org.bukkit.Material/AIR)))))

  )

(defn replace-with-material-around-player [player-name to-dist if-material to-material]
  (schedule-seq!
   (horiz-coords-around (get-player-loc-xyz player-name) to-dist)
   (fn [[xx _ zz]]
     (let [yy    (.getHighestBlockYAt (overworld) xx zz)
           block (.getBlockAt (overworld) xx yy zz)]
       (when (= if-material (.getType block))
         (.setType block to-material))))))

(comment
  (replace-with-material-around-player "kyle_burton" 10 org.bukkit.Material/TNT org.bukkit.Material/AIR)

  )

(comment


  ;; getHighestBlockYAt
  ;; org.bukkit.HeightMap
  (.getHighestBlockYAt (overworld) (.getLocation (get-player-named "kyle_burton")))
  (.getHighestBlockYAt (overworld) -240 106)

  ;; I think this is the type of material,
  ;; .getMaterial
  (.getMaterial (.getBlockData (.getBlockAt (overworld) -240 79 106)))
  (.getType (.getBlockAt (overworld) -240 80 106))
  ;; #object[org.bukkit.Material 0x3e447947 "STONE"]

  (.getMaterial (.getBlockData (.getBlockAt (overworld) -240 80 106)))
  ;; #object[org.bukkit.Material 0x41575ce3 "AIR"]

  ;; nb: need to get the light level of the air block above the target block
  (.getLightLevel (.getBlockAt (overworld) -240 80 106))
  (.getLightFromBlocks (.getBlockAt (overworld) -240 80 106)) ;; on the air block
  (.getLightFromBlocks (.getBlockAt (overworld) -240 79 106)) ;; on the stone block

  (.getLightFromSky (.getBlockAt (overworld) -240 80 106))
  (.getLightFromSky (.getBlockAt (overworld) -240 79 106))

  (set-world-time! :sunset)
  (forward-world-time-abs! 500)

  (overworld)
  ;; => 81

  (def kyle (org.bukkit.Bukkit/getPlayer "kyle_burton"))
  ;; setRotation
  ;; setExp
  ;; setFlying
  ;; setFireTicks
  ;; setGravity

  ;; this throws an exception
  ;; (.setRotation kyle 0.0 0.0)

  ;; south is pitch=0
  ;; west  is pitch=90
  ;; north is pitch=180
  ;; east  is pitch=270
  (let [new-loc (.getLocation kyle)]
    ;; (.setPitch new-loc 0.0)
    ;; (.setYaw new-loc 0.0)
    (.setPitch new-loc 0.0)
    (.setYaw new-loc 270.0)
    (schedule!
     (.teleport kyle new-loc)))

  (.getDirection (.getLocation (get-player-named "kyle_burton")))

  )


(defn flatten-to-bedrock [loc-xyz to-dist]
  (schedule!
   (doseq [[xx _ zz] (horiz-coords-around loc-xyz to-dist)
           :let [yy (get-highest-block-y-at xx zz)]]
     (loop [yy yy]
       (let [block (get-block-at xx yy zz)]
         (cond
           (= 0 yy)
           (do
             (log/infof "flatten-to-bedrock: halting, hit yy=0")
             :done)

           (= (.getType block) org.bukkit.Material/AIR)
           (do
             (log/infof "flatten-to-bedrock: skipping air block at [%s,%s,%s]" xx yy zz)
             (recur (dec yy)))

           (= (.getType block) org.bukkit.Material/BEDROCK)
           (do
             (log/infof "flatten-to-bedrock: halting, found bedrock block at [%s,%s,%s]" xx yy zz)
             :done)

           :else
           (do
             (log/infof "flatten-to-bedrock: setting block to air at [%s,%s,%s]" xx yy zz)
             (.setType block org.bukkit.Material/AIR)
             (recur (dec yy)))))))))

(defn flatten-to-bedrock-slowly! [loc-xyz to-dist]
  (let [place-fn (fn placef [coords]
                   (log/infof "placef: (count coords)=%s" (count coords))
                   (when-not (empty? coords)
                     (let [[xx _ zz]  (first coords)
                           highest-yy (inc (.getHighestBlockYAt (overworld) xx zz))]
                       (doseq [yy (range highest-yy -1 -1)
                               :let [block (.getBlockAt (overworld) xx yy zz)]]
                         (schedule-later! (- 100 yy)
                                          (cond
                                            (= 0 yy)
                                            (log/infof "flatten-to-bedrock-slowly!: noop at yy=0")

                                            (= (.getType block) org.bukkit.Material/AIR)
                                            (log/infof "flatten-to-bedrock-slowly!: skipping air block at [%s,%s,%s]" xx yy zz)

                                            (= (.getType block) org.bukkit.Material/BEDROCK)
                                            (log/infof "flatten-to-bedrock-slowly!: noop, at bedrock [%s,%s,%s]" xx yy zz)

                                            :else
                                            (do
                                              (log/infof "flatten-to-bedrock-slowly!: setting block to air at [%s,%s,%s]" xx yy zz)
                                              (.setType block org.bukkit.Material/AIR)))))
                       (placef (rest coords)))))]

    (place-fn (horiz-coords-around loc-xyz to-dist))))


(defn right-triangle-find-a-for-c-and-a-degrees [c-len a-degrees]
  (* c-len (Math/sin (Math/toRadians a-degrees))))

(comment
  (Math/round (right-triangle-find-a-for-c-and-a-degrees 5 36.86))
  (Math/round (right-triangle-find-a-for-c-and-a-degrees 5 53))

  (Math/round (right-triangle-find-a-for-c-and-a-degrees 5 0))
  )

;; TODO: locations and distances should probably be based on the
;; middle of the block the player is standing on and to the middle of
;; the target block to make things "look right"
(defn loc-in-front-of-player [player-name straightline-dist]
  (let [[xx _ zz _pitch yaw] (get-player-location player-name)
        ;; xx                   (+ 0.5 (Math/floor xx))
        ;; zz                   (+ 0.5 (Math/floor zz))
        yy                   (inc (.getHighestBlockYAt (overworld) (int xx) (int zz)))
        x-sign               (if (neg? yaw)          1 -1)
        z-sign               (if (and (>= yaw -90.0) (<= yaw 90.0)) 1 -1)
        yaw-abs              (Math/abs yaw)
        yaw-rad              (Math/toRadians yaw-abs)
        xdelta               (Math/round (* x-sign straightline-dist (Math/abs (Math/sin yaw-rad))))
        zdelta               (Math/round (* z-sign straightline-dist (Math/abs (Math/cos yaw-rad))))]
    [(+ xx xdelta)
     yy
     (+ zz zdelta)]))

(comment
  (.getLocation (get-player-named "kyle_burton"))

  (org.bukkit.Location/normalizeYaw (.getYaw (.getLocation (get-player-named "kyle_burton"))))

  (loc-in-front-of-player "kyle_burton" 3)

  )

(defn place-infront-of-player [player-name dist material]
  (let [[xx _yy zz] (loc-in-front-of-player player-name dist)
        yy          (inc (.getHighestBlockYAt (overworld) (int xx) (int zz)))]
    (schedule!
     (log/infof "place-infront-of-player: placing %s at %s" material [xx yy zz])
     (.setType (get-block-at xx yy zz) material))))

(comment
  (loc-in-front-of-player "kyle_burton" 3)

  (place-infront-of-player "kyle_burton" 3 org.bukkit.Material/TNT)

  (.getLocation (get-player-named "kyle_burton"))


  )

(defn sin-of-degrees [deg]
  (Math/sin (Math/toRadians deg)))

(defn cos-of-degrees [deg]
  (Math/cos (Math/toRadians deg)))

(comment
  ;; 3,4,5 triangle
  ;; the angle opposite the 3 side:
  (Math/toDegrees (Math/asin (/ 3.0 5.0)))
  36.86989764584402
  ;; the angle opposite the 4 side:
  (Math/toDegrees (Math/asin (/ 4.0 5.0)))
  53.13010235415598

  (Math/sin 90)
  (Math/cos 180)

  (for [deg (range 0 360)
        :let [rad (Math/toRadians deg)]]
    [deg rad (Math/sin rad) (Math/cos rad)])

  (/ 4 5.0)
  0.8
  (Math/toDegrees 0.8)
  45.83662361046586
  (/ 3 5.0)
  0.6
  (Math/toDegrees 0.6)
  34.37746770784939

  )

(comment

  org.bukkit.Material
  (flatten-to-bedrock (get-player-loc-xyz "kyle_burton") 6)

  (flatten-to-bedrock-slowly! (get-player-loc-xyz "kyle_burton") 6)

  (flatten-to-bedrock-slowly! (get-player-loc-xyz "kyle_burton") 10)

  (horiz-coords-around (get-player-loc-xyz "kyle_burton") 1)

  ;; south:         0 /   0
  ;; west:         90 /  90
  ;; north:       180 / 180 and -180
  ;; east:        270 / -90
  (get-player-location "kyle_burton")


  ;; (flatten-to-bedrock (get-player-loc-xyz "kyle_burton") __128)

  ;; 5244

  )


(defn layer-of [loc-xyz xlen zlen material]
  (let [[xxo yyo zzo] loc-xyz]
    (doseq [xxd (range xlen) ;; x delta
            zzd (range zlen)
            :let [xx (+ xxo xxd)
                  zz (+ zzo zzd)]]
      (.setType (get-block-at xx yyo zz) material))))

(defn place-beacon [loc-xyz material]
  (schedule!
   (let [[xxo _ zzo] loc-xyz ;; x origin
         yy          (inc (.getHighestBlockYAt (overworld) (int xxo) (int zzo)))]
     (layer-of [(+ 0 xxo) (+ 0 yy) (+ 0 zzo)] 11 10 material)
     (layer-of [(+ 1 xxo) (+ 1 yy) (+ 1 zzo)]  9  8 material)
     (layer-of [(+ 2 xxo) (+ 2 yy) (+ 2 zzo)]  7  6 material)
     (layer-of [(+ 3 xxo) (+ 3 yy) (+ 3 zzo)]  5  4 material)
     (layer-of [(+ 4 xxo) (+ 4 yy) (+ 4 zzo)]  3  2 org.bukkit.Material/BEACON))))

(defn vec+ [v1 v2]
  (mapv + v1 v2))

(comment
  (vec+ [1 2 3]
        [2 2 2])

  (place-beacon
   (loc-in-front-of-player "kyle_burton" 3)
   org.bukkit.Material/DIAMOND_BLOCK)

  (dotimes [_ 5]
    (replace-with-material-around-player "kyle_burton" 13 org.bukkit.Material/DIAMOND_BLOCK org.bukkit.Material/AIR)
    (replace-with-material-around-player "kyle_burton" 13 org.bukkit.Material/BEACON org.bukkit.Material/AIR))


  )

(defn is-air? [material]
  (contains? #{org.bukkit.Material/AIR
               org.bukkit.Material/CAVE_AIR
               org.bukkit.Material/VOID_AIR
               org.bukkit.Material/LEGACY_AIR}
             material))


(defn flatten-fill [[xxo yyo zzo :as _loc-xyz] material to-dist]
  (doseq [xx (->> to-dist range (map #(- (+ % xxo) (/ to-dist 2))))
          zz (->> to-dist range (map #(- (+ % zzo) (/ to-dist 2))))
          yy (range (get-highest-block-y-at xx zz) yyo)
          :let [block (get-block-at xx yy zz)]]
    (if (is-air? (.getType block))
      (do
        (log/infof "flatten-fill: setting [%s,%s,%s] to %s" xx yy zz material)
        (.setType block material))
      (log/infof "flatten-fill: block at [%s,%s,%s] is not air, it is %s" xx yy zz (.getType block)))))

(comment

  (schedule!
   (flatten-fill
    (get-player-loc-xyz "kyle_burton")
    ;; (-> (get-player-loc-xyz "kyle_burton") (vec+ [0 -1 0]) get-block-at .getType)
    org.bukkit.Material/GRASS_BLOCK
    10))

  (schedule!
   (flatten-fill
    (get-player-loc-xyz "kyle_burton")
    org.bukkit.Material/STRUCTURE_BLOCK
    10))

  org.bukkit.Material

  (get-player-location "kyle_burton")

  (-> (loc-in-front-of-player "kyle_burton" 1) (vec+ [0 0 0]) get-block-at)

  (-> (loc-in-front-of-player "kyle_burton" 1) (vec+ [0 0 0]) get-block-at .getNMS .getStateMap)


  net.minecraft.server.v1_15_R1.IBlockState
  (-> (loc-in-front-of-player "kyle_burton" 1) (vec+ [0 0 0]) get-block-at .getBlockPower)
  (-> (loc-in-front-of-player "kyle_burton" 1) (vec+ [0 0 0]) get-block-at .getBlockData)

  org.bukkit.block.Beacon

  (cast
   org.bukkit.block.Beacon
   (-> ;; (loc-in-front-of-player "kyle_burton" 1)
    [-288 79 44]
    ;; (vec+ [0 0 0])
    get-block-at
    .getState)


   )

  (log/infof "state=%s" (-> ;; (loc-in-front-of-player "kyle_burton" 1)
                         [-288 79 44]
                         ;; (vec+ [0 0 0])
                         get-block-at
                         .getState))


  )


(defn get-block-state [loc-xyz]
  (.getState
   (get-block-at loc-xyz)))

(defn get-block-state! [loc-xyz]
  (let [p (promise)]
    (schedule!
     (deliver
      p
      (get-block-state loc-xyz)))
    @p))

(defn all-coords-around-location [loc dist]
  (let [[xx yy zz] (-> loc ->loc location-to-xyz)
        dmin       (* -1 dist)]
    (for [x1 (range dmin (inc dist))
          y1 (range dmin (inc dist))
          z1 (range dmin (inc dist))]
      [(+ xx x1)
       (+ yy y1)
       (+ zz z1)])))

(defn shell-coords-around-location [loc dist]
  ;; surface of a cube, top is dist^2, bottom is dist^2
  (let [[xx yy zz] (-> loc ->loc location-to-xyz)
        x-min      (- xx dist)
        x-max      (+ xx dist)
        y-min      (- yy dist)
        y-max      (+ yy dist)
        z-min      (- zz dist)
        z-max      (+ zz dist)]
    (vec
     (concat
      ;; top - y
      (for [xx (range x-min (inc x-max))
            zz (range z-min (inc z-max))]
        [xx y-max zz])
      ;; bottom - y
      (for [xx (range x-min (inc x-max))
            zz (range z-min (inc z-max))]
        [xx y-min zz])
      ;; front - x
      (for [yy (range y-min (inc y-max))
            zz (range (inc z-min) z-max)]
        [x-min yy zz])
      ;; back  - x
      (for [yy (range y-min (inc y-max))
            zz (range (inc z-min) z-max)]
        [x-max yy zz])
      ;; left  - z
      (for [xx (range (inc x-min) x-max)
            yy (range (inc y-min) y-max)]
        [xx yy z-max])
      ;; right - z
      (for [xx (range (inc x-min) x-max)
            yy (range (inc y-min) y-max)]
        [xx yy z-min])))))

(comment

  (count (shell-coords-around-location "DominusSermonis" 2))

  )


;; NB: it's not possible to name a placed block!
#_(defn find-named-block-near-player [player block-name]
    ;; starting at where the player is standing, search outward
    (loop [dist     1
           max-dist 3 #_8]
      (let [coords (shell-coords-around-location player dist)
            item   (->> coords
                        (filter (fn [coord]
                                  (let [block (get-block-at coord)]
                                    (if (= org.bukkit.Material/STONE (.getType block))
                                      (def block block))
                                    (log/infof "block=%s" block)
                                    nil)))
                        first)]
        (cond
          item
          item

          (zero? max-dist)
          nil

          :else
          (recur (inc dist) (dec max-dist))))))
;; (find-named-block-near-player "DominusSermonis" "marker")

(defn find-blocks-of-material-around-player [player material dist]
  (let [player (->player player)]
    (->>
     (all-coords-around-location player dist)
     (map #(get-block-at %))
     (filter #(= material (.getType %)))
     vec)))

(comment

  block
  (.getState block)
  (.getNMS block)
  ;; (.getMetadata block <String>) ;;
  (.getDrops block)
  (.getBlockData block) ;;

  (.getState (.getBlockData block))

  (.getPrimaryEffect (get-block-state! [-288 79 44]))
  (.getSecondaryEffect (get-block-state! [-288 79 44]))

  (schedule!
   (let [ ;; effect org.bukkit.potion.PotionEffectType/SPEED
         effect org.bukkit.potion.PotionEffectType/FAST_DIGGING
         beacon (get-block-state [-288 79 44])]
     (.setPrimaryEffect beacon effect)
     (.setSecondaryEffect beacon effect)
     (.update beacon)
     (log/infof "set primary and secondary")))

  org.bukkit.potion.PotionEffectType

  (get-player-loc-xyz "kyle_burton")
  [-289 77 41]

  (do
    (set-world-time! :morning)
    (def chicken
      (->
       (let [world (overworld)
             loc   (-> world (.getHighestBlockAt 0 0) .getLocation)]
         [world
          loc
          (.locateNearestStructure
           (overworld)
           loc ;; Location (origin)
           ;; org.bukkit.StructureType/VILLAGE ;; StructureType
           ;; org.bukkit.StructureType/STRONGHOLD
           org.bukkit.StructureType/OCEAN_MONUMENT
           500  ;; int radius
           true ;; bool findUnexplored
           )])
       (nth 2)
       location-to-xz))
    chicken)


  ;; chicken


  (schedule!
   (let [player (get-player-named "kyle_burton")
         world  (.getWorld player)
         ;; [xx zz] [32 736]       ;; village: snow
         ;; [xx zz] [768 592]      ;; village: snow
         ;; [xx zz] [-912 1040]    ;; village: desert
         ;; [xx zz] [112 1216]     ;; village: snow
         ;; [xx zz] [512 1136]     ;; village: snow
         ;; [xx zz] [1024 576]     ;; village: snow
         ;; [xx zz] [1024 576]     ;; village: snow
         ;; [xx zz] [1392 1280]    ;; village: snow
         [xx zz] [-1328 -1424] ;; village: plans/desert
         ;; [xx zz] [-1360 1200]   ;; village: desert
         ;; [xx zz] [-976 -1504]   ;; village: savanah
         ;; [xx zz] [-992 1824]    ;; village: savanah
         ;; [xx zz] [-1808 800]    ;; village: savanah
         ;; [xx zz] [-1968 1040]   ;; village: plains
         ;; [xx zz] [-1792 1904]   ;; village: desert w/temple
         ;; [xx zz] [-928 -1728]   ;; village: desert
         ;; [xx zz] [2416 -1904]   ;; village: plains
         ;; [xx zz] [-1688 -1256]  ;; stronghold
         ;; [xx zz] [-752 256]     ;; ocean monument (big)
         ;; [xx zz] [-752 2144]    ;; ocean monument
         yy     (inc (.getHighestBlockYAt world xx zz))
         dest   (org.bukkit.Location.
                 world
                 xx
                 yy
                 zz)]
     (.teleport player dest)))

  (schedule!
   (.teleport (get-player-named "kyle_burton") (org.bukkit.Location. (overworld) -289 77 41)))


  ;; TODO: generate more structures
  )


(defn loc->loc-and-type [loc]
  (let [loc   (->loc loc)
        block (.getBlock loc)]
    [loc (.getType block)]))


(defn range-up [ii jj]
  (if (< ii jj)
    (range ii jj)
    (range jj ii)))

(defn range-down [ii jj]
  (if (< ii jj)
    (range jj ii -1)
    (range ii jj -1)))
