(ns Player
  (:gen-class))

; Grab the pellets as fast as you can!
(def raw-text "#################################\n#####   #     # # #     #   #####\n##### ##### # # # # # ##### #####\n#           #       #           #\n# # # # # # # ##### # # # # # # #\n# #   #   # #   #   # #   #   # #\n# # ### ### ### # ### ### ### # #\n# #                           # #\n### # ##### ### # ### ##### # ###\n#   #   #   ### # ###   #   #   #\n# # ### # # ### # ### # # ### # #\n# #       #           #       # #\n### ### # ### ##### ### # ### ###\n#################################")

(def test-rows (vec (seq (.split raw-text "\n"))))

(defn output [msg] (println msg) (flush))
(defn debug [msg] (binding [*out* *err*] (println msg) (flush)))

(def small-test-rows
  "#####\n#   #\n#####")

(def small-rows (vec (.split small-test-rows "\n")))

(def small-grid (atom small-rows))

(def grid (atom test-rows))
(def waypoints (atom []))
(def destination (atom []))

(defn coordinated-grid []
  (->> @grid
       (map-indexed (fn [row-index row]
                      (map-indexed (fn [col-index col] [col-index row-index col]) row)))
       (apply concat)
       vec))

(def static-neighbours-coordinates-set
  (->> (for [i (range -1 2)
             j (range -1 2)]
         [i j])
       (filter #(not= [0 0] %))
       set))

(defn neighbours-coordinates [[x0 y0]]
  (-> (map (fn [[x y]] [(+ x0 x) (+ y0 y)]) static-neighbours-coordinates-set)
      set))

(defn tile-coord [[tx ty _]]
  [tx ty])

(defn is-neighbour [tile coord]
  (contains? (neighbours-coordinates (tile-coord tile)) coord))

(defn find-neighbours [coord]
  (let [grid (coordinated-grid)]
    (filter #(is-neighbour % coord) grid)))

(defn is-wall [tile] (= tile \#))
(defn is-free-space [tile] (= tile \space))
(defn is-tile-free [[_ _ tile]] (is-free-space tile))

(defn find-free-neighbours [coord]
  (->> (find-neighbours coord)
       (filter is-tile-free)))

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

(defn vertices [] (set (filter is-tile-free (coordinated-grid))))
(defn edges [] (->> (vertices)
                    (map (fn [vertex] [vertex (find-free-neighbours (tile-coord vertex))]))
                    (map (fn [[vertex neighbours]]
                           (for [n neighbours]
                             [(tile-coord vertex) (tile-coord n)])))
                    (apply concat)
                    set))
(defn graph [] [(vertices), (edges)])

(defn visited? [vertex visited]
  (contains? (set visited) vertex))

;; From http://dnaeon.github.io/graphs-and-clojure/
(defn graph-dfs []
  (let [[vertices edges] (graph)]
    (loop [stack (vec vertices)
           visited []]
      (if (empty? stack)
        visited
        (let [vertex (peek stack)
              neighbours (find-free-neighbours (tile-coord vertex))
              not-visited (filter (complement #(visited? % visited)) neighbours)
              new-stack (into (pop stack) not-visited)]
          (if (visited? vertex visited)
            (recur new-stack visited)
            (recur new-stack (conj visited vertex))))))))

(defn -main [& args]
  (let [width (read) height (read) _ (read-line)]
    ; width: size of the grid
    ; height: top left corner is (x=0, y=0)
    (reset! grid (repeatedly height read-line))
    (reset! waypoints [(top-left-corner)
                       (bottom-right-corner)])
    (reset! destination (first @waypoints))
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