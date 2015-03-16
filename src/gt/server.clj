(ns gt.server
  "Provides an HTTP server for the user to include new nodes in the
  *current-graph*."
  (:require [gt.graph                 :as gr]
            [gt.reader-io             :as io]
            [clojure.tools.logging    :as log]
            [hiccup.core              :refer [html]]
            [compojure.route          :refer [not-found]]
            [compojure.core           :refer [defroutes GET]]
            [ring.middleware.defaults :refer [wrap-defaults site-defaults]]
            [org.httpkit.server       :refer [run-server]]))

(def ^{:private true} current-graph
  "This is the graph that is available to the clients of this
  server. This is prone to be side-effected by multiple threads, and
  the current-farnesses needs to be kept in sync with it. Hence, it
  needs to deal with coordination and synchronization."
  (ref gr/empty-graph))

(def ^{:private true} current-closeness
  "This is the closeness of the nodes of the *current-graph*. It is
  here to act as a cache, since calculating closeness is a heavy
  operation. As a cache, it this must be kept in sync with the
  *current-graph*." (ref gr/empty-farnesses))

(defn- get-current-closeness-html []
  (html (gr/farnesses->html @current-closeness)))

(defn- reset-graph-to-default!
  "Resets the graph to the default configuration given by the edges file."
  []
  (let [default-undirected-graph-file-path "resources/edges"]
    (dosync
     (let [old-graph @current-graph]
       (ref-set
        current-graph
        (io/path->undirected-graph default-undirected-graph-file-path))
       (when-not (= old-graph @current-graph)
         (ref-set current-closeness (gr/graph->sorted-closeness @current-graph)))
       (get-current-closeness-html)))))

(defn- add-undirected-link
  "Adds a link between NODE-A and NODE-B, if none exists, in the current-graph
  in a synchronized and coordinated way, avoiding thus conflicts between clients
  inserting new links and reading the closeness."
  [node-a node-b]
  (dosync
   (when-not (gr/contains-undirected-link? @current-graph node-a node-b)
     (alter current-graph #(gr/append-undirected-link % node-a node-b))
     (ref-set current-closeness (gr/graph->sorted-closeness @current-graph)))
   (get-current-closeness-html)))

(defn- mark-as-fraudulent
  "Marks a given NODE as fraudulent."
  [id]
  ;; do stuff
  )

(defroutes ^{:private true}
  all-routes
  (GET "/closeness" [] (get-current-closeness-html))
  (GET "/start_default" [] (reset-graph-to-default!))
  (GET "/mark_as_fraudulent" req (mark-as-fraudulent (-> req :params :node)))
  (GET "/link" req
    (let [node-a (-> req :params :node_a)
          node-b (-> req :params :node_b)]
      (add-undirected-link node-a node-b)))
  (not-found "Route not found")) ;; returns 404

(defn start! [port]
  (run-server (wrap-defaults all-routes site-defaults)
              {:port port}))
