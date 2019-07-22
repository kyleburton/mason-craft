(ns krb-minerepl-bukkit.core
  (:require [clojure.tools.logging :as log]))
;; krb_minerepl_bukkit/core


(def plugin (atom nil))
(defn set-plugin! [p]
  (reset! plugin p))

(defn plugin []
  (->>
   (org.bukkit.Bukkit/getServer)
   .getPluginManager
   .getPlugins
   (filter #(= "krbminerepl" (.getName %)))
   first))

(comment
  (plugin)

)

;; M-x cider-connect 4123
;; https://hub.spigotmc.org/javadocs/bukkit/index.html?org/bukkit/entity/package-summary.html


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

(defn loc+ [loc [x y z]]
  (.setX loc (+ x (.getX loc)))
  (.setY loc (+ x (.getY loc)))
  (.setZ loc (+ x (.getZ loc)))
  loc)

(defn overworld []
  (-> (org.bukkit.Bukkit/getWorlds) first))


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

;; TODO: catling-gun

(comment
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
    (-> (org.bukkit.Bukkit/getWorlds)
        first
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

  (doseq [thing (-> (org.bukkit.Bukkit/getWorlds)
                    first
                    .getEntities)]
    (let [wheres-kyle (.getLocation kyle)]
      (.setZ wheres-kyle (+ 3 (.getZ wheres-kyle)))
      (try
        (.teleport thing wheres-kyle)
        (catch Exception ex
          (.println System/out (format "err: ex=%s" ex))))))

  (doseq [thing (-> (org.bukkit.Bukkit/getWorlds)
                    first)
          .getEntities]
    (.setFireTicks thing 1))


  )
