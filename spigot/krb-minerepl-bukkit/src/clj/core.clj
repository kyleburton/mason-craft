(ns krb-minerepl-bukkit.core)

(defmacro after [stime & body]
  `(.run
    (Thread.
     (fn []
       (Thread/sleep ~stime)
       ((fn [] ~@body))))))

(defn all-players []
  (org.bukkit.Bukkit/getOnlinePlayers))

(defn get-player-named [name]
  (org.bukkit.Bukkit/getPlayer "kyle_burton"))

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

(defn send-em-all-there! [loc]
  (doseq [thing (-> (overworld) .getEntities)]
    (try
      (.teleport thing loc)
      (catch Exception ex
        (.println System/out (format "err: ex=%s" ex))))))

(defn light-em-up! []
  (doseq [thing (-> (overworld)) .getEntities)]
    (.setFireTicks thing 5000)))


(comment
  (.setWeatherDuration (overworld) 1)

  (let [wheres-kyle (.getLocation (get-player-named "kyle_burton"))
        near-kyle   (loc+ wheres-kyle [10 0 10])]
    ;; make sure it's not raining
    (after 2000
           (do
             (send-em-all-there! near-kyle)
             (send-em-all-there! near-kyle) "sent them near you"
             (light-em-up!)
             (send-em-all-there! near-kyle) "lit them up"))))

(after 2000 (light-em-up!))

  (chat-to-player "kyle_burton" "yo" "stuff is stuff")
  ;; M-x cider-connect 4123

  ;; https://hub.spigotmc.org/javadocs/bukkit/index.html?org/bukkit/entity/package-summary.html

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

  (.getMaxFireTicks thing1)

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
    (.setFireTicks thing 1)))
