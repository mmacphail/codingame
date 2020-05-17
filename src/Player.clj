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

(def medium-graph (build-graph (vec (.split "#####\n#   #\n## ##\n## ##\n#   #\n## ##\n#####" "\n"))))

(def debug-graph (build-graph (vec (.split "#################################\n###   #     # ##### #     #   ###\n### # # ### # ##### # ### # # ###\n#   # #   #   #####   #   # #   #\n# ### # # # # ##### # # # # ### #\n#   #   #   #       #   #   #   #\n### # # ### ### # ### ### # # ###\n      #     #   #   #     #      \n### # ##### # # # # # ##### # ###\n#   #         #   #         #   #\n# # ##### ### # # # ### ##### # #\n#################################" "\n"))))

(def max-path-length 7)
(defn find-paths
  ([graph position]
   (find-paths graph [] [position] position max-path-length))
  ([graph paths current-path position moves-left]
   (let [visited (set current-path)
         neighbours (->> (get graph position)
                         (filter #(not (contains? visited %))))]
     (if (or (= moves-left 0) (empty? neighbours))
       [current-path]
       (vec (mapcat (fn [neighbour]
                      (find-paths graph paths
                                  (conj current-path neighbour)
                                  neighbour
                                  (- moves-left 1)))
                    neighbours))))))

(defn calc-coord-pellets-value [pellets coords]
  (reduce
    (fn [sum coord]
      (let [pellets-value (get pellets coord)
            value (if (nil? pellets-value) 0 pellets-value)]
        (+ sum value)))
    0 coords))

(defn find-next-waypoint [graph pellets position]
  (->> (find-paths graph position)
       (map (fn [coords] {:coord coords
                          :points (calc-coord-pellets-value pellets coords)}))
       (sort-by :points)
       reverse
       first
       :coord
       last))

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

(defn read-pellet []
  (let [x (read) y (read) value (read) _ (read-line)]
    [[x y] value]))

(defn read-pellets []
  (let [visible-pellet-count (read)]
    (->> (repeatedly visible-pellet-count read-pellet)
         vec
         (into {}))))

(defn -main [& args]
  (let [width (read) height (read) _ (read-line)
        grid (repeatedly height read-line)
        graph (-> (build-graph grid) simplify-graph)
        nodes (graph-dfs graph)
        pacman-pos (atom [0 0])]
    (debug grid)
    (loop [destination nil]
      (debug (str "destination " destination))
      (let [my-score (read)
            opponent-score (read)
            visible-pac-count (read)
            _ (read-line)]
        (update-pac-positions visible-pac-count pacman-pos)
        (let [pellets (read-pellets)
              dest (if (nil? destination)
                     (find-next-waypoint graph pellets @pacman-pos)
                     destination)]
          (debug (str "pellets " pellets))
          (move-to! 0 dest)
          (if (at-destination @pacman-pos dest)
            (recur (find-next-waypoint graph pellets @pacman-pos))
            (recur dest)))))))