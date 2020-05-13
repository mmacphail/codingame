(ns Player-test
  (:require [clojure.test :refer :all]
            [Player :refer :all]))

(deftest test-is-free-space
  (testing "A space tile is free space"
    (is (is-free-space \space))
    (is (not (is-free-space \#)))))