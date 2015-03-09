(ns gt.graph-test
  (:require [clojure.test :refer :all]
            [gt.graph :refer :all]))

(def test-graph
  "graph used for the tests"
  {0 #{1 5}
   1 #{0 2}
   2 #{1 3 4}
   3 #{2 4}
   4 #{2 3}
   5 #{0}
   ;; a node that links and is not linked by another node should not
   ;; be possible in an undirected graph. it is included here to check
   ;; the search function itself.
   6 #{}})

(deftest graph-making
  (testing "The making of a graph"
    (is (= (-> empty-graph
               (append-undirected-link 0 1)
               (append-undirected-link 0 5)
               (append-undirected-link 1 2)
               (append-undirected-link 2 3)
               (append-undirected-link 2 4)
               (append-undirected-link 3 2)
               (append-undirected-link 3 4))
           (dissoc test-graph 6)))))

(deftest node-distance
  (testing "The distance of some nodes in a graph"
    (let [search-distance  @#'gt.graph/search-distance
          graph->distances @#'gt.graph/graph->distances]
      (is (= (search-distance 0 1 test-graph)
             1))
      (is (= (search-distance 0 2 test-graph)
             2))
      (is (= (search-distance 0 3 test-graph)
             3))
      (is (= (search-distance 5 3 test-graph)
             4))
      (is (= (search-distance 0 6 test-graph)
             false))
      (is (= (graph->distances test-graph)
             {#{4 3} 1,
              #{0 1} 1,
              #{3 5} 4,
              #{0 4} 3,
              #{0 3} 3,
              #{1 4} 2,
              #{1 5} 2,
              #{4 2} 1,
              #{1 3} 2,
              #{1 2} 1,
              #{0 2} 2,
              #{2 5} 3,
              #{4 5} 4,
              #{3 2} 1,
              #{0 5} 1})))))

(deftest graph-farnesses
  (testing "The farnesses of the nodes in the graph"
    (is (= (sort-graph-by-closeness test-graph)
           '(2 1 0 3 4 5)))))
