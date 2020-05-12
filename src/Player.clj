(ns Player
  (:gen-class))

; Grab the pellets as fast as you can!

(defn output [msg] (println msg) (flush))
(defn debug [msg] (binding [*out* *err*] (println msg) (flush)))

(def grid (atom []))
(def waypoints (atom []))
(def destination (atom []))

(defn is-wall [tile] (= tile \#))
(defn is-free-space [tile] (= tile \space))

(defn indexed-cols [row]
  (map-indexed (fn [index column] [index column]) row))

(defn spaces-of-indexed-row [row]
  (filter (fn [[_ col]] (is-free-space col)) row))

(defn top-left-corner []
  (->> @grid
       (map indexed-cols)
       (map spaces-of-indexed-row)
       (map-indexed (fn [row-index row] [(first (map first row)) row-index]))
       (filter #(not (nil? (first %))))
       (sort-by (fn [[_ col]] col))
       first))

(defn bottom-right-corner []
  (->> @grid
       (map indexed-cols)
       (map spaces-of-indexed-row)
       (map-indexed (fn [row-index row] [(last (map first row)) row-index]))
       (filter #(not (nil? (first %))))
       last))

(defn move-to [id [x y]]
  (output (str "MOVE " id " " x " " y)))

(defn -main [& args]
  (let [width (read) height (read) _ (read-line)]
    ; width: size of the grid
    ; height: top left corner is (x=0, y=0)
    (reset! grid (repeatedly height read-line))
    (reset! waypoints [(top-left-corner)
                       (bottom-right-corner)])
    (reset! destination (first @waypoints))
    (debug (str "waypoints: " @waypoints))
    (debug (str "destination: " @destination))
    (while true
      (let [myScore (read) opponentScore (read) visiblePacCount (read) _ (read-line)
            pacman-pos (atom [0 0])]
        ; visiblePacCount: all your pacs and enemy pacs in sight
        (loop [i visiblePacCount]
          (when (> i 0)
            (let [pacId (read) mine (read) x (read) y (read) typeId (read) speedTurnsLeft (read) abilityCooldown (read) _ (read-line)]
              (when (and (= 0 pacId) (= mine 1))
                (reset! pacman-pos [x y]))
              ; pacId: pac number (unique within a team)
              ; mine: true if this pac is yours
              ; x: position in the grid
              ; y: position in the grid
              ; typeId: unused in wood leagues
              ; speedTurnsLeft: unused in wood leagues
              ; abilityCooldown: unused in wood leagues
              (recur (dec i)))))
        (let [visiblePelletCount (read)]
          ; visiblePelletCount: all pellets in sight
          (loop [i visiblePelletCount]
            (when (> i 0)
              (let [x (read) y (read) value (read) _ (read-line)]
                ; value: amount of points this pellet is worth
                (recur (dec i)))))

          ; (debug "Debug messages...")

          ; MOVE <pacId> <x> <y>
          (let [[x y] @pacman-pos
                [x2 y2] @destination]
            (when (and (= x x2) (= y y2))
              (do
                (swap! waypoints rest))
                (reset! destination (first @waypoints)))
            (move-to 0 @destination)))))))