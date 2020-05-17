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

(def medium-test-rows
  "#####\n#   #\n#####")

(def grid (atom test-rows))
(def waypoints (atom []))
(def destination (atom []))

(defn coordinated-grid [grid]
  (->> grid
       (map-indexed (fn [row-index row]
                      (map-indexed (fn [col-index col] [col-index row-index col]) row)))
       (apply concat)
       vec))

(def static-neighbours-coordinates-set
  #{[0 -1] [1 0] [0 1] [-1 0]})

(defn neighbours-coordinates [[width _] [x0 y0]]
  (let [wrapping-coordinates (if (= x0 0)
                               [[(- width 1) y0]]
                               [])]
    (-> (map (fn [[x y]] [(+ x0 x) (+ y0 y)]) static-neighbours-coordinates-set)
        (concat wrapping-coordinates)
        set)))

(defn tile-coord [[tx ty _]]
  [tx ty])

(defn is-wall [tile] (= tile \#))
(defn is-free-space [tile] (= tile \space))
(defn is-tile-free [[_ _ tile]] (is-free-space tile))

(defn find-neighbours [graph vertex]
  (get graph vertex))

(defn move-to! [id [x y]]
  (output (str "MOVE " id " " x " " y)))

(defn is-in-grid [[width height] [x y]]
  (and (<= 0 x) (<= 0 y)
       (> width x) (> height y)))

(defn neighbours-in-grid [grid-dimensions coord]
  (set (filter (partial is-in-grid grid-dimensions) (neighbours-coordinates grid-dimensions coord))))

(defn dimensions [grid]
  [(count (first grid)) (count grid)])

(defn is-space-in-grid [nodes [x y]]
  (= (last (nth (get nodes y) x)) \space))

(defn build-graph [grid]
  (let [coordinated-grid (coordinated-grid grid)
        neighbours-in-grid (partial neighbours-in-grid (dimensions grid))
        is-space-in-grid (partial is-space-in-grid (group-by second coordinated-grid))]
    (->> (for [tile (filter is-tile-free coordinated-grid)]
           (let [neighbours (filter is-space-in-grid (neighbours-in-grid tile))]
             [(tile-coord tile) (set neighbours)]))
         (into {}))))

(defn visited? [vertex visited]
  (contains? (set visited) vertex))

;; From http://dnaeon.github.io/graphs-and-clojure/
(defn graph-dfs [graph]
  (loop [stack (vec (keys graph))
         visited []]
    (if (empty? stack)
      visited
      (let [vertex (peek stack)
            neighbours (find-neighbours graph vertex)
            not-visited (filter (complement #(visited? % visited)) neighbours)
            new-stack (into (pop stack) not-visited)]
        (if (visited? vertex visited)
          (recur new-stack visited)
          (recur new-stack (conj visited vertex)))))))

(defn shortcut-vertex [graph vertex left right]
  (-> graph
      (dissoc vertex)
      (update left conj right)
      (update left disj vertex)
      (update right conj left)
      (update right disj vertex)))

(defn simplify-vertex [graph vertex]
  (let [neighbours (get graph vertex)
        [left right] (vec neighbours)]
    (if (= (count neighbours) 2)
      (shortcut-vertex graph vertex left right)
      (assoc graph vertex neighbours))))

(defn simplify-graph [graph]
  (->> graph
       keys
       (reduce simplify-vertex graph)))

(defn at-destination [[pos-x pos-y] [x y]]
  (and (= pos-x x) (= pos-y y)))

(defn update-pac-positions [visible-pac-count pacman-pos]
  ; visiblePacCount: all your pacs and enemy pacs in sight
  (loop [i visible-pac-count]
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
        (recur (dec i))))))

(defn update-pellets []
  (let [visiblePelletCount (read)]
    ; visiblePelletCount: all pellets in sight
    (loop [i visiblePelletCount]
      (when (> i 0)
        (let [x (read) y (read) value (read) _ (read-line)]
          ; value: amount of points this pellet is worth
          (recur (dec i))))))

  (defn -main [& args]
    (let [width (read) height (read) _ (read-line)
          grid (repeatedly height read-line)
          graph (-> (build-graph grid) simplify-graph)
          nodes (graph-dfs graph)]
      (debug grid)
      (loop [destination (first nodes)
             waypoints (rest nodes)]
        (let [my-score (read)
              opponent-score (read)
              visible-pac-count (read)
              _ (read-line)
              pacman-pos (atom [])]
          (update-pac-positions visible-pac-count pacman-pos)
          (update-pellets)
          (move-to! 0 destination)
          (if (at-destination @pacman-pos destination)
            (recur (first waypoints) (rest waypoints))
            (recur destination waypoints)))))))