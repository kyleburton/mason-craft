(ns krb-minerepl-bukkit.core
  (:require
   [clojure.tools.logging :as log]))

;; M-x cider-connect 4123
;; https://hub.spigotmc.org/javadocs/bukkit/index.html?org/bukkit/entity/package-summary.html

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defn logback-configure! [xml-config-file-path]
  (let [logger-factory     (org.slf4j.LoggerFactory/getILoggerFactory)
        joran-configurator (ch.qos.logback.classic.joran.JoranConfigurator.)]
    (.setContext joran-configurator logger-factory)
    (.doConfigure joran-configurator xml-config-file-path)))

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
  (plugin)

  )

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

;; X is east (pos) / west (neg)
;; Y is height
;; Z is north (neg) / south (pos)
(defn get-player-location [^String name]
  (let [loc (.getLocation (get-player-named name))]
    [(.getX loc)
     (.getY loc)
     (.getZ loc)
     (.getPitch loc)
     (.getYaw loc)]))

(defn get-player-loc-xz [^String name]
  (let [loc (.getLocation (get-player-named name))]
    [(.getX loc)
     (.getZ loc)]))

(defn get-player-loc-xyz [^String name]
  (let [loc (.getLocation (get-player-named name))]
    [(.getX loc)
     (.getY loc)
     (.getZ loc)]))

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

(defn loc+ [loc [x y z yaw pitch]]
  ;; (if yaw (.setYaw loc yaw))
  ;; (if pitch (.setPitch loc pitch))
  (let [newloc (org.bukkit.Location.
                (.getWorld loc)
                (+ x (.getX loc))
                (+ y (.getY loc))
                (+ z (.getZ loc)))]
    newloc))

(def overworld-instance (atom nil))
(defn overworld []
  (when (nil? @overworld-instance)
    (reset! overworld-instance
            (-> (org.bukkit.Bukkit/getWorlds) first)))
  @overworld-instance)

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

(defn schedule-fn! [f delay]
  (->
   (org.bukkit.Bukkit/getServer)
   .getScheduler
   (.scheduleSyncDelayedTask
    (plugin)
    f
    delay)))

(defmacro schedule! [& body]
  `(->
    (org.bukkit.Bukkit/getServer)
    .getScheduler
    (.runTask
     (plugin)
     (fn []
       (try
         (do ~@body)
         (catch Exception e#
           (log/infof e# "Error: e=%s" e#)))))))

(defn schedule-seq! [elts func]
  (let [scheduler (-> (org.bukkit.Bukkit/getServer) .getScheduler)
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

(comment
  (.getScheduler (org.bukkit.Bukkit/getServer))

  )

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


(comment
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
           chicken     (.spawnEntity (overworld) near-kyle org.bukkit.entity.EntityType/CHICKEN)]
       (.teleport entity near-kyle)
       (.addPassenger chicken entity)
       (.setFireTicks entity 1000))))

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

    (place-fn (horiz-coords-around origin to-dist)))
  #_(schedule-seq!
     (horiz-coords-around origin to-dist)
     (fn [[xx _ zz]]
       (let [yy    (inc (.getHighestBlockYAt (overworld) xx zz))
             block (.getBlockAt (overworld) xx yy zz)]
         (if (and (= org.bukkit.Material/AIR (.getType block))
                  (< (.getLightLevel block) 8))
           (do
             (log/infof "place-torches: PLACING torch at [%s,%s,%s] light-level=%s" xx yy zz (.getLightLevel block))
             (.setType block org.bukkit.Material/TORCH))
           (log/infof "place-torches: NOT PLACING TORCH at [%s,%s,%s] light-level=%s" xx yy zz (.getLightLevel block)))))))


(comment
  (set-world-time! :midnight)

  (place-torches! (get-player-loc-xyz "kyle_burton") 1)

  (place-torches! (get-player-loc-xyz "kyle_burton") 10)

  (place-torches! (get-player-loc-xyz "kyle_burton") 10)

  (schedule-seq!
   (horiz-coords-around (get-player-loc-xyz "kyle_burton") 10)
   (fn [[xx _ zz]]
     (let [yy    (inc (.getHighestBlockYAt (overworld) xx zz))
           block (.getBlockAt (overworld) xx yy zz)]
       (when (= org.bukkit.Material/TORCH (.getType block))
         (log/infof "removing torch at %s" [xx yy zz])
         (.setType block org.bukkit.Material/AIR)))))

  )

(comment
  (logback-configure! "/home/kyle/code/github.com/kyleburton/mason-craft/spigot/krb-minerepl-bukkit/resources/logback.xml")



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
  (.getLightFromBlocks (.getBlockAt (overworld) -240 80 106));; on the air block
  (.getLightFromBlocks (.getBlockAt (overworld) -240 79 106));; on the stone block

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

  (place-torches )


  )
