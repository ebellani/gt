(ns gt.graph-test
  (:require [clojure.test :refer :all]
            [gt.graph :refer :all]))

(deftest graph-making
  (testing "The making of a graph"
    (let [node1 (->Node 0 #{1 2})
          node2 (->Node 1 #{0 3})
          graph #{node1 node2}]
      (is (= (-> #{}
                 (append-connection 0 1)
                 (append-connection 0 2)
                 (append-connection 1 0)
                 (append-connection 1 3))
             graph)))))

(deftest node-distance
  (testing "The distance of some nodes in a graph"
    (let [node0 (->Node 0 #{1})
          node1 (->Node 1 #{0 2})
          node2 (->Node 2 #{3 4})
          node3 (->Node 3 #{4})
          node5 (->Node 5 #{0})
          graph #{node0 node1 node2 node3}]
      (is (= (search-distance node0 node1 graph)
             1))
      (is (= (search-distance node0 node2 graph)
             2))
      (is (= (search-distance node0 node3 graph)
             3))
      (is (= (search-distance node5 node3 graph)
             4))
      (is (= (search-distance node0 node5 graph)
             false)))))
