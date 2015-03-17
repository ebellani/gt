(ns gt.graph
  "Type (and operators) for dealing with the graph structure, which is basically
  a set of nodes and their links."
  (:require [clojure.set :as set]))

;;; description of the data structures used in this module:

;;; The fundamental entity of the graph system is the node, represented
;;; by an id (this id is called simply `node` in the application). For
;;; now the graph is assumed to be undirected and the links between
;;; nodes are represented as a set of linked nodes.
;;;
;;; The graph itself is implemented in terms of a map of
;;;
;;; node -> links

;;; The closeness calculation involves the distance, which is a tuple
;;; containing a set of 2 nodes and the quantity of links between these
;;; two nodes. This quantity is a scalar integer.
;;;
;;; The distaces structure is implemented in terms of a map of
;;;
;;; set of 2 nodes -> quantity

;;; Closeness is a computation that is defined as such: the *closeness* of
;;; a given node *n* is the sum of all distances from each node to
;;; *n*, inverted.
;;;
;;; It is represented as a tuple containing an id and a scalar quantity.
;;;
;;; The closenesses structure is implemented in terms of a map of
;;;
;;; id -> quantity

;; these empty definitions are here in order to avoid having magic
;; literals spread in the code, and to provide a single point of
;; construction, making it easier to swap implementations in the
;; future (from the current map to another structure).
(def empty-graph
  {})

(def empty-distances
  {})

(def empty-closenesses
  {})

;;; graph querying section

(defn contains-undirected-link? [graph node-a node-b]
  (and (contains? graph node-a)
       (contains? graph node-b)
       (contains? (get graph node-a) node-b)
       (contains? (get graph node-b) node-a)))

;;; end section

;;; distances section

(defn- search-distance
  "Returns a scalar integer value as the distance between the nodes represented
  by the SOURCE-NODE and the TARGET-NODE in a given GRAPH. If there is no
  linkage, returns false. The implementation for now is a breadth first
  algorithm[1].

  [1] https://en.wikipedia.org/wiki/Breadth-first_search"
  [source-node target-node graph]
  (let [get-next-links (fn [current-next-links current-node]
                         (if-let [links (get graph current-node)]
                           (set/union current-next-links links)
                           current-next-links))]
    (loop [current-links      (get graph source-node)
           next-links         #{}
           searched-so-far    #{}
           distance           1]
      (if-let [current-node (first current-links)]
        (cond (= current-node target-node) distance ;; <- end recursion
              (contains? searched-so-far current-node)
              (recur (next current-links)
                     next-links
                     searched-so-far
                     distance)
              :otherwise
              (recur (next current-links)
                     (get-next-links next-links current-node)
                     (conj searched-so-far current-node)
                     distance))
        (if (empty? next-links)
          false ;; <- end recursion with failure to find target
          (recur next-links
                 #{}
                 searched-so-far
                 (inc distance)))))))

(defn- node->distances
  "Returns the distances from a a NODE to all the other NODES. The
  COMPLETE-GRAPH is used to properly calculate these distances."
  [node target-nodes complete-graph]
  (reduce (fn [distances-so-far current-target-node]
            (if-let [distance (search-distance node
                                               current-target-node
                                               complete-graph)]
              (assoc distances-so-far #{node current-target-node} distance)
              distances-so-far))
          empty-distances
          target-nodes))

(defn graph->distances
  "Creates Distances from a GRAPH. These distances enable the sorting and the
  stablishment of centrality."
  [graph]
  (loop [distances empty-distances
         subgraph (keys graph)]
    (if-let [current-node (first subgraph)]
      (recur (merge distances
                    (node->distances current-node
                                     (next subgraph)
                                     graph))
             (next subgraph))
      distances)))

;;; section end

;;; degree section

(defn sort-by-degree
  "Type of sorting of a GRAPH by the degree centrality, which is defined as the
  number of links incident upon a node. In the case of a unweighted graph, a
  link is incident from and upon a node at the same time."
  [graph]
  (->> graph
       (sort (fn [[_ links-a] [_ links-b]]
               (compare (count links-a) (count links-b))))
       (map first)))

;;; section end

;;; closenesses section

(defn distances->closenesses
  "Computes the closenesses out of the DISTANCES structure. This structure is one
  of the basis of sorting the nodes of a graph."
  [distances]
  (letfn [(add-farness [node current-distance-pair farnesses-so-far]
            (+ (get distances current-distance-pair)
               (get farnesses-so-far node 0)))]
    (->> distances
         keys
         (reduce
          (fn [farnesses-so-far current-distance-pair]
            (let [node-a (first current-distance-pair)
                  node-b (second current-distance-pair)]
              (-> farnesses-so-far
                  (assoc node-a (add-farness node-a
                                             current-distance-pair
                                             farnesses-so-far))
                  (assoc node-b (add-farness node-b
                                             current-distance-pair
                                             farnesses-so-far)))))
          empty-closenesses)
         (map (fn [[node farness]] [node (/ 1 farness)]))
         (into {}))))

(defn invalidate-node
  "Returns a the CLOSENESSES structure 'updated' (actually a new structure) as
  such:

  - The INVALID-NODE quantity should be zero.

  - Nodes directly referred by the INVALID-NODE should
  have their score halved.

  - Quantities of nodes indirectly referred by the INVALID-NODE should be
  multiplied by a coefficient F:

  F(k) = (1 - (1/2)^k)

  where k is the shortest path from the INVALID-NODE to the node in
  question. This is calculated using the DISTANCES as a cache.

  The point of this function is to be able to represent distrust of a node. This
  function assumes the GRAPH, where the invalid links are taken from, is
  undirected."
  [node closenesses distances graph]
  (let [invalid-node-quantity 0
        invalid-direct-links (get graph node)
        indirect-node-modification
        (fn [distance]
          (- 1 (Math/pow (/ 1 2) distance)))]
    (->> closenesses
         (map
          (fn [[current-node quantity]]
            [current-node
             (cond (= current-node node)
                   invalid-node-quantity
                   (contains? invalid-direct-links current-node)
                   (/ quantity 2)
                   :otherwise
                   (-> distances
                       (get #{current-node node})
                       indirect-node-modification
                       (* quantity)))]))
         (into {}))))

;;; section end

;;; formatting section

(defn- sort-closenesses
  "Sorts the CLOSENESSES by the inverse of their quantity, making the more
  'relevant' nodes come first."
  [closenesses]
  (sort (fn [[_ closeness-a] [_ closeness-b]]
          (compare closeness-b closeness-a))
        closenesses))

(defn closenesses->html [closenesses]
  [:ol (->> closenesses
            sort-closenesses
            (map (fn [[id _]] [:li id])))])

;;; section end

;;; graph edition section

(defn create-directed-link
  "this updates the graph with a connection (source-node -> target-node). This
  is the base function to build a graph incrementally."
  [graph source-node target-node]
  (assoc graph source-node
         (if (contains? graph source-node)
           (conj (get graph source-node) target-node)
           #{target-node})))

(defn append-undirected-link
  "this updates the graph with a connection (NODE-A <-> NODE-B). The point of
  this function is to provide an utility to build undirected graphs."
  [graph node-a node-b]
  (-> graph
      (create-directed-link node-a node-b)
      (create-directed-link node-b node-a)))

;;; section end
