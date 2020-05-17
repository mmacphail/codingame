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

(def debug-graph (build-graph (vec (.split "#################################\n###   # # # # #   # # # # #   ###\n### # # # # # # # # # # # # # ###\n    #     #     #     #     #    \n### # # ### # ##### # ### # # ###\n#   # #     #       #     # #   #\n# ### ##### ### # ### ##### ### #\n#                               #\n### ### # # ### # ### # # ### ###\n#     # #     #   #     # #     #\n# # # # ##### # # # ##### # # # #\n# # # #       #   #       # # # #\n# # # # ### # # # # # ### # # # #\n# #     ### #       # ###     # #\n# ### # ### # ##### # ### # ### #\n#     #     #   #   #     #     #\n#################################" "\n"))))
(def debug-pellets {[8 7] 1, [3 15] 1, [8 11] 1, [9 8] 1, [7 1] 1, [7 12] 1, [26 13] 1, [27 9] 1, [13 3] 1, [30 15] 1, [23 5] 1, [10 5] 1, [13 15] 1, [15 11] 10, [11 9] 1, [11 2] 1, [7 11] 1, [17 5] 1, [7 13] 1, [22 7] 1, [21 11] 1, [21 7] 1, [25 10] 1, [25 1] 1, [3 9] 1, [7 7] 1, [18 3] 1, [23 2] 1, [31 10] 1, [27 3] 1, [10 15] 1, [22 5] 1, [17 2] 1, [27 14] 1, [19 9] 1, [17 6] 1, [2 3] 1, [18 7] 1, [2 5] 1, [7 2] 1, [16 5] 1, [29 15] 1, [25 14] 1, [6 7] 1, [15 12] 1, [24 5] 1, [7 4] 1, [25 7] 1, [8 3] 1, [21 12] 1, [9 15] 1, [27 5] 1, [3 3] 1, [13 12] 1, [10 9] 1, [5 4] 1, [15 3] 1, [5 10] 1, [25 15] 1, [6 3] 1, [27 12] 1, [21 3] 1, [3 4] 1, [27 4] 1, [22 9] 1, [11 14] 1, [1 12] 1, [31 11] 1, [31 3] 1, [28 1] 10, [29 8] 1, [7 3] 1, [19 12] 1, [22 15] 1, [16 7] 1, [17 12] 1, [31 5] 1, [1 15] 1, [15 9] 1, [17 1] 1, [14 13] 1, [7 8] 1, [3 12] 1, [23 1] 1, [26 7] 1, [13 2] 1, [31 15] 1, [21 8] 1, [1 13] 1, [11 13] 1, [13 9] 1, [19 10] 1, [6 13] 1, [1 9] 1, [20 3] 1, [5 3] 1, [9 9] 1, [13 7] 1, [9 3] 1, [24 11] 1, [29 7] 1, [27 1] 1, [29 10] 1, [23 15] 1, [4 7] 1, [24 3] 1, [13 1] 1, [20 11] 1, [4 15] 1, [19 7] 1, [21 2] 1, [3 13] 1, [4 9] 1, [25 5] 1, [31 6] 1, [1 10] 1, [2 9] 1, [11 11] 1, [27 15] 1, [5 13] 1, [29 12] 1, [5 14] 1, [16 13] 1, [31 7] 1, [19 1] 1, [4 1] 10, [5 2] 1, [21 6] 1, [14 15] 1, [15 7] 1, [21 1] 1, [23 11] 1, [27 6] 1, [11 4] 1, [11 8] 1, [1 11] 1, [5 7] 1, [11 12] 1, [21 4] 1, [21 14] 1, [12 7] 1, [27 13] 1, [29 2] 1, [29 4] 1, [27 10] 1, [10 7] 1, [31 14] 1, [25 3] 1, [1 3] 1, [10 11] 1, [12 11] 1, [15 1] 1, [1 5] 1, [13 13] 1, [11 6] 1, [11 3] 1, [15 10] 1, [1 7] 1, [15 5] 1, [29 1] 1, [25 11] 1, [30 5] 1, [16 11] 1, [21 13] 1, [19 4] 1, [30 7] 1, [0 3] 1, [29 9] 1, [5 1] 1, [5 11] 1, [5 6] 1, [24 15] 1, [2 15] 1, [13 11] 1, [8 15] 1, [25 4] 1, [28 7] 1, [8 5] 1, [15 8] 1, [13 5] 1, [9 11] 1, [5 5] 1, [7 9] 1, [2 7] 1, [11 1] 1, [27 2] 1, [15 6] 1, [5 9] 1, [19 14] 1, [23 9] 1, [5 15] 1, [14 5] 1, [7 10] 1, [19 13] 1, [17 3] 1, [22 11] 1, [17 10] 1, [9 2] 1, [20 7] 1, [19 5] 1, [11 7] 1, [9 1] 1, [9 7] 1, [17 15] 1, [31 9] 1, [15 13] 1, [30 9] 1, [12 9] 1, [25 12] 1, [20 9] 1, [19 11] 1, [24 7] 1, [19 3] 1, [17 11] 10, [11 15] 1, [21 5] 1, [23 8] 1, [23 7] 1, [29 3] 1, [3 11] 1, [25 8] 1, [11 5] 1, [26 3] 1, [3 1] 1, [19 15] 1, [3 10] 1, [12 3] 1, [30 3] 1, [17 13] 1, [27 11] 1, [1 14] 1, [9 5] 1, [3 8] 1, [18 15] 1, [7 14] 1, [25 13] 1, [16 1] 1, [21 15] 1, [1 6] 1, [29 5] 1, [16 9] 1, [14 3] 1, [27 7] 1, [3 7] 1, [28 15] 1, [7 5] 1, [28 9] 1, [25 9] 1, [7 15] 1, [29 11] 1, [13 10] 1, [17 9] 1, [13 4] 1, [25 2] 1, [15 15] 1, [21 9] 1, [14 7] 1, [23 3] 1, [18 13] 1, [3 5] 1, [13 14] 1, [5 12] 1, [31 13] 1, [3 2] 1, [17 8] 1, [19 2] 1, [18 5] 1, [29 13] 1, [15 2] 1, [32 3] 1, [31 12] 1, [17 7] 1})
(def debug-start-pos [28 13])

(def debug-o-graph (build-graph (vec (.split "##################################\n    # #   #   # ### #   #   # #   \n### # # # # # # ### # # # # # # ##\n#   #   #   #         #   #   #   \n# ### ##### ### ### ### ##### ### \n#       #   #         #   #       \n### # # # # # # # # # # # # # # ##\n### #     #   # # # #   #     # ##\n### ### # ### # ### # ### # ### ##\n#   #   #     #     #     #   #   \n# # # # ### # ### ### # ### # # # \n# #     #   #         #   #     # \n### ### # # # ### ### # # # ### ##\n    #     # #   # #   # #     #   \n##################################" "\n"))))
(def debug-o-pellets {[7 6] 1, [27 8] 1, [8 7] 1, [9 8] 1, [7 1] 1, [7 12] 1, [26 13] 1, [27 9] 1, [15 4] 1, [13 3] 1, [28 5] 1, [23 5] 1, [10 5] 1, [15 11] 10, [11 9] 1, [11 2] 1, [7 11] 1, [17 5] 1, [21 10] 1, [19 6] 1, [7 13] 1, [22 7] 1, [21 11] 1, [21 7] 1, [25 1] 1, [3 9] 1, [34 13] 1, [13 8] 1, [7 7] 1, [18 3] 1, [23 2] 1, [31 10] 1, [27 3] 1, [13 6] 1, [19 9] 1, [17 6] 1, [2 3] 1, [2 5] 1, [7 2] 1, [16 5] 1, [6 7] 1, [33 5] 1, [24 5] 1, [25 7] 1, [21 12] 1, [33 11] 1, [27 5] 1, [3 3] 1, [13 12] 1, [10 9] 1, [5 4] 1, [15 3] 1, [5 10] 1, [1 1] 1, [6 3] 1, [28 3] 1, [27 12] 1, [24 9] 1, [21 3] 1, [22 9] 1, [33 3] 1, [32 5] 1, [31 11] 1, [31 3] 1, [7 3] 1, [17 12] 1, [31 5] 1, [15 9] 1, [20 5] 1, [14 13] 1, [7 8] 1, [3 12] 1, [32 13] 1, [23 1] 1, [26 7] 1, [13 2] 1, [21 8] 1, [9 6] 1, [1 13] 1, [12 1] 1, [11 13] 1, [13 9] 1, [6 13] 1, [1 9] 1, [18 11] 1, [0 13] 1, [20 3] 1, [5 3] 1, [9 9] 1, [13 7] 1, [9 3] 1, [24 11] 1, [29 7] 1, [27 1] 1, [9 12] 1, [29 10] 1, [23 12] 1, [24 3] 1, [13 1] 1, [20 11] 1, [19 7] 1, [21 2] 1, [3 13] 1, [25 5] 1, [22 1] 1, [1 10] 1, [2 9] 1, [18 9] 1, [6 5] 1, [11 11] 1, [5 13] 1, [31 7] 1, [33 10] 1, [19 1] 1, [4 11] 1, [26 1] 1, [5 2] 1, [21 6] 1, [2 13] 1, [20 13] 1, [16 3] 1, [15 7] 1, [21 1] 1, [23 11] 1, [33 9] 1, [27 6] 1, [11 4] 1, [1 4] 1, [30 11] 1, [23 4] 1, [1 11] 1, [5 7] 1, [11 12] 1, [33 1] 1, [12 7] 1, [27 13] 1, [29 2] 1, [29 4] 1, [27 10] 1, [11 10] 1, [25 3] 1, [1 3] 1, [10 11] 1, [15 1] 1, [1 5] 1, [13 13] 1, [11 6] 1, [11 3] 1, [15 5] 1, [29 1] 1, [25 11] 1, [8 1] 1, [30 5] 1, [32 1] 1, [16 11] 1, [21 13] 1, [19 4] 1, [29 9] 1, [5 1] 1, [5 11] 1, [5 6] 1, [13 11] 1, [31 1] 1, [8 13] 1, [28 7] 1, [31 2] 1, [14 11] 1, [15 8] 1, [13 5] 1, [9 11] 1, [6 11] 1, [5 5] 1, [7 9] 1, [11 1] 1, [23 6] 1, [34 1] 1, [27 2] 1, [15 6] 1, [5 9] 1, [23 9] 1, [14 5] 1, [7 10] 1, [19 13] 1, [17 3] 1, [23 13] 1, [17 10] 1, [9 2] 1, [19 5] 1, [4 5] 1, [11 7] 1, [9 1] 1, [9 7] 1, [31 9] 1, [15 13] 1, [12 9] 1, [25 12] 1, [19 11] 10, [6 9] 10, [19 3] 1, [17 11] 1, [21 5] 1, [23 7] 1, [23 10] 1, [29 3] 1, [3 11] 1, [25 8] 1, [11 5] 1, [9 13] 1, [3 1] 1, [3 10] 1, [25 6] 1, [17 13] 1, [27 11] 1, [2 1] 1, [19 8] 1, [9 5] 1, [3 8] 1, [25 13] 1, [29 5] 1, [16 9] 1, [14 3] 1, [27 7] 1, [3 7] 1, [7 5] 1, [28 9] 10, [25 9] 1, [29 11] 1, [13 10] 1, [17 9] 1, [25 2] 1, [21 9] 1, [31 8] 1, [23 3] 1, [10 3] 1, [28 13] 1, [3 5] 1, [33 4] 1, [31 13] 1, [3 2] 1, [32 9] 1, [19 2] 1, [18 5] 1, [29 13] 1, [29 6] 1, [0 1] 1, [33 13] 1, [15 2] 1, [32 3] 1, [31 12] 1, [28 11] 1, [17 7] 1})
(def debug-o-start-pos [3 6])

(defn find-paths
  ([graph position max-path-length]
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

(defn parts-of-path [array]
  (let [middle-coord (nth array (int (Math/floor (-> array count (/ 2)))))
        last-coord (last array)]
    [middle-coord last-coord]))

(defn find-best-path [graph pellets position max-path-length]
  (->> (find-paths graph position max-path-length)
       (map (fn [coords] {:coord  coords
                          :points (calc-coord-pellets-value pellets coords)}))
       (sort-by :points)
       reverse
       first))

(defn find-waypoints [graph pellets position]
  (->> (find-best-path graph pellets position 7)
       :coord
       parts-of-path))

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
        graph (build-graph grid)
        pacman-pos (atom [0 0])]
    (debug grid)
    (loop [waypoints []]
      (let [my-score (read)
            opponent-score (read)
            visible-pac-count (read)
            _ (read-line)]
        (update-pac-positions visible-pac-count pacman-pos)
        (let [pellets (read-pellets)
              wpts (if (empty? waypoints)
                     (find-waypoints graph pellets @pacman-pos)
                     waypoints)
              dest (first wpts)]
          (debug (str "pos " @pacman-pos))
          (debug (str "wpts " wpts))
          (debug (str "dest " dest))
          (debug (str "pellets " pellets))
          (move-to! 0 dest)
          (if (at-destination @pacman-pos dest)
            (if (empty? (rest waypoints))
              (recur (find-waypoints graph pellets @pacman-pos))
              (recur (rest waypoints)))
            (recur waypoints)))))))

;; List of things to improve
;; prevent collisions with other bot
;; if hasn't moved in 3 rounds ; move in a random direction ?
;; discard loops (paths that return to adjacent tiles)