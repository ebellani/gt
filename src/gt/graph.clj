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

;;; The farness calculation involves the distance, which is a tuple
;;; containing a set of 2 nodes and the quantity of links between these
;;; two nodes. This quantity is a scalar integer.
;;;
;;; The distaces structure is implemented in terms of a map of
;;;
;;; set of 2 nodes -> quantity

;;; Farness is a computation that is defined as such: the *farness* of
;;; a given node *n* is the sum of all distances from each node to
;;; *n*. Finally, the *closeness* of a node *n* is the inverse of the
;;; *farness*.
;;;
;;; It is represented as a tuple containing an id and a scalar quantity.
;;;
;;; The farnesses structure is implemented in terms of a map of
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

(def empty-farnesses
  {})

;;; graph creation functions section

;;; TODO: change this into a map of indexes -> ids and an adjacency
;;; matrix

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

;;; distances section

(defn- search-distance
  "Returns a scalar integer value as the distance between the nodes represented
  by the SOURCE-NODE and the TARGET-NODE in a given GRAPH. If there is no
  linkage, returns false. The implementation for now is a breadth first
  algorithm[1].

  [1] https://en.wikipedia.org/wiki/Breadth-first_search "
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
        (cond (= current-node target-node) distance
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
          false
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

(defn- graph->distances
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

;;; farness section

(defn- distances->farnesses
  "Computes the farnesses out of the DISTANCES structure. This structure is the
  basis of sorting the nodes of a graph by their closeness/farness."
  [distances]
  (letfn [(add-farness [node current-distance-pair farnesses-so-far]
            (+ (get distances current-distance-pair)
               (get farnesses-so-far node 0)))]
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
     empty-farnesses
     (keys distances))))

(defn sort-by-closeness
  "Type of sorting of a GRAPH by closeness. WARNING: this function will do all
  computations involved in getting to the farnesses."
  [graph]
  (->> graph
       graph->distances
       distances->farnesses
       (sort (fn [[_ farness-a] [_ farness-b]]
               (compare farness-a farness-b)))
       (map first)
       reverse))

;;; section end
