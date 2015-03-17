(ns gt.server
  "Provides an HTTP server for the user to include new nodes in the
  *current-graph*."
  (:require [gt.graph                 :as gr]
            [gt.reader-io             :as io]
            [hiccup.core              :refer [html]]
            [compojure.route          :refer [not-found]]
            [compojure.core           :refer [defroutes GET]]
            [ring.middleware.defaults :refer [wrap-defaults site-defaults]]
            [org.httpkit.server       :refer [run-server]]))

(def ^{:private true} current-graph
  "This is the graph that is available to the clients of this
  server. This is prone to be side-effected by multiple threads, and
  the current-closenesses and the current-distances need to be kept in
  sync with it. Hence, it needs to deal with coordination and
  synchronization."
  (ref gr/empty-graph))

(def ^{:private true} current-distances
  "This is the distances between the nodes of the *current-graph*. It
  is here to act as a cache, since calculating closeness is a heavy
  operation. As a cache, it this must be kept in sync with the
  *current-graph*." (ref gr/empty-distances))

(def ^{:private true} current-closenesses
  "This is the closeness of the nodes of the *current-graph*. It is
  here to act as a cache, since calculating closeness is a heavy
  operation. As a cache, it this must be kept in sync with the
  *current-graph*." (ref gr/empty-closenesses))


(def ^{:private true} fraudulent-nodes
  "Stores the nodes marked as fraudulent. This is used to update the
  closenesses after each mutation of the graph"
  (ref #{}))

(defn- get-current-closeness-html []
  (html (gr/closenesses->html @current-closenesses)))

(defn- reset-vars!
  "Resets the main vars that are derived out of the GRAPH. This *must* be called
  from a transaction body. This returns the html of the current-closenesses."
  [graph]
  (ref-set current-graph graph)
  (ref-set current-distances (gr/graph->distances @current-graph))
  (ref-set current-closenesses
           (reduce (fn [updated-closenesses fraudulent-node]
                     (gr/invalidate-node fraudulent-node
                                         updated-closenesses
                                         @current-distances
                                         @current-graph))
                   (gr/distances->closenesses @current-distances)
                   @fraudulent-nodes))
  (get-current-closeness-html))

(defn- reset-graph-to-default!
  "Resets the graph to the default configuration given by the edges file."
  []
  (let [default-undirected-graph-file-path "resources/edges"]
    (dosync
     (let [old-graph @current-graph
           new-graph (io/path->undirected-graph
                      default-undirected-graph-file-path)]
       (if-not (= old-graph new-graph)
         (reset-vars! new-graph)
         (get-current-closeness-html))))))

(defn- add-undirected-link
  "Adds a link between NODE-A and NODE-B, if none exists, in the current-graph
  in a synchronized and coordinated way, avoiding thus conflicts between clients
  inserting new links and reading the closeness."
  [node-a node-b]
  (dosync
   (if-not (gr/contains-undirected-link? @current-graph node-a node-b)
     (reset-vars! (gr/append-undirected-link @current-graph
                                             node-a
                                             node-b))
     (get-current-closeness-html))))

(defn- mark-as-fraudulent
  "Marks a given NODE as fraudulent."
  [id]
  (dosync
   (alter fraudulent-nodes #(conj % id))
   (reset-vars! @current-graph)))

(defroutes ^{:private true}
  all-routes
  (GET "/closeness" _ (get-current-closeness-html))
  (GET "/start_default" _ (reset-graph-to-default!))
  (GET "/mark_as_fraudulent" req
    (mark-as-fraudulent (-> req
                            :params
                            :node
                            Integer/parseInt)))
  (GET "/link" req
    (let [node-a (-> req :params :node_a)
          node-b (-> req :params :node_b)]
      (add-undirected-link node-a node-b)))
  (not-found "Route not found")) ;; returns 404

(defn start! [port]
  (run-server (wrap-defaults all-routes site-defaults)
              {:port port}))
