(ns Player-test
  (:require [clojure.test :refer :all]
            [Player :refer :all]))

(deftest test-is-free-space
  (testing "A space tile is free space"
    (is (is-free-space \space))
    (is (not (is-free-space \#)))))

(let [grid "###################################     # #   #   # ### #   #   # #     ### # # # # # # ### # # # # # # ### #   #   #   #         #   #   #   # # ### ##### ### ### ### ##### ### # #       #   #         #   #       # ### # # # # # # # # # # # # # # ### ### #     #   # # # #   #     # ### ### ### # ### # ### # ### # ### ### #   #   #     #     #     #   #   # # # # # ### # ### ### # ### # # # # # #     #   #         #   #     # # ### ### # # # ### ### # # # ### ###     #     # #   # #   # #     #     ###################################"
      width 35
      height (+ (mod (count grid) width) 1)
      row-width (+ width 1)]
  (->> (range height)
       (map
         (fn [i]
           (subs grid (* row-width i) (if (= i (- height 1))
                                        (- (count grid) 1)
                                        (* row-width (+ i 1))))))
       (map (fn [row] (if (> (count row) width)
                        (subs row 0 (- width 1))
                        row)))
       (clojure.string/join "\n")))