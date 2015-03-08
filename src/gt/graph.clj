(ns gt.graph
  "Type (and operators) for dealing with the graph structure, which is basically
  a set of nodes."
  (:require [clojure.set :as set]))

;; NODE is a relation of ID and LINKS where:
;;
;; * ID is a number and
;;
;; * LINKS is a set of ID.
;;
;; A graph would be a set of Nodes
(defrecord Node [id links])

;; DISTANCE is a relation of a NODE-PAIR and a scalar QUANTITY, where:
;;
;; * NODE-PAIR is a set consisting of exactly 2 Nodes.
;;
;; * QUANTITY is defined by the length of the shortest path of the NODE-PAIR
(defrecord Distance [node-pair quantity])


;; FARNESS is a relation of a NODE with a scalar quantity, where:
;;
;; * NODE is a regular node
;;
;; * QUANTITY is the sum of all distances from each node in a graph to
;; the NODE
(defrecord farness  [node quantity])

(defn- append-link-node-id
  "Updates the links attribute of a given NODE. Auxiliary function to
  `append-connection`"
  [node link-id]
  (assoc node :links (conj (:links node) link-id)))

(defn- get-node
  "Returns a node with a given ID in the GRAPH. Returns nil if the node is
  missing."
  [id graph]
  (->> graph
       (set/select #(= (:id %) id) ,,)
       first))

(defn- search-distance
  "Returns a scalar integer value as the distance between the SOURCE-NODE and
  the TARGET-NODE. The implementation for now is a breadth first algorithm[1].

  [1] https://en.wikipedia.org/wiki/Breadth-first_search "
  [source-node target-node graph]
  (let [target-id (:id target-node)
        get-next-nodes (fn [current-next-nodes-ids current-id]
                         (if-let [node (get-node current-id graph)]
                           (set/union current-next-nodes-ids
                                      (:links node))
                           current-next-nodes-ids))]
    (loop [current-nodes-ids  (:links source-node)
           next-nodes-ids     #{}
           searched-so-far    #{}
           distance           1]
      (if-let [current-id (first current-nodes-ids)]
        (cond (= current-id target-id) distance
              (contains? searched-so-far current-id)
              (recur (next current-nodes-ids)
                     next-nodes-ids
                     searched-so-far
                     distance)
              :otherwise
              (recur (next current-nodes-ids)
                     (get-next-nodes next-nodes-ids current-id)
                     (conj searched-so-far current-id)
                     distance))
        (if (empty? next-nodes-ids)
          false
          (recur next-nodes-ids
                 #{}
                 searched-so-far
                 (inc distance)))))))

(defn- node->distances
  "Returns a set distances from a NODE to all nodes in the SUBGRAPH. The
  COMPLETE-GRAPH is used to calculate the distance between nodes."
  [node subgraph complete-graph]
  (loop [graph subgraph
         distances #{}]
    (if-let [current-target-node (first graph)]
      (recur (next graph)
             (conj distances
                   (Distance. #{node current-target-node}
                              (search-distance node
                                               current-target-node
                                               complete-graph))))
      distances)))

(defn append-connection
  "This updates the GRAPH with a connection (SOURCE-NODE-ID ->
  LINK-NODE-ID). If there is an existing node (marked by the same id), this
  creates a new node with the link id conjoined to the links property. The
  point of this function is to provide a way to build a graph incrementally."
  [graph source-node-id link-node-id]
  (if-let [source-node (get-node source-node-id graph)]
    (-> graph
        (disj ,, source-node)
        (conj ,, (append-link-node-id source-node link-node-id)))
    (conj graph (Node. source-node-id (hash-set link-node-id)))))

(defn graph->distances
  "Creates Distances from a GRAPH. These distances enable the sorting and the
  stablishment of centrality."
  [graph]
  (loop [distances #{}
         subgraph graph]
    (println "Graph size -> " (count subgraph)
             "Dists size -> " (count distances))
    (if-let [current-node (first subgraph)]
      (recur (set/union distances
                        (node->distances current-node
                                         (next subgraph)
                                         graph))
             (next subgraph))
      distances)))

;; (defn distances->farnesses [distances]
;;   )
