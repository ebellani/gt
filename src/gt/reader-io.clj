(ns gt.reader-io
  "Generates data structures out of file paths. The point of this module is to
  isolate the notion of filesystem and the file formats from the application,
  since they are really orthogonal to the task."
  (:require [clojure.java.io :as io]
            [clojure.string  :as str]
            [gt.graph        :as gr]))

(defn path->graph
  "Returns a graph (a set of Nodes) structure from the given FILE-PATH."
  [file-path]
  (with-open [rdr (io/reader file-path)]
    (doall (reduce (fn [graph current-line]
                     (let [[source-node-id link-node-id]
                           (->> #" "
                                (str/split current-line)
                                (map read-string))]
                       (gr/append-connection graph
                                             source-node-id
                                             link-node-id)))
                   gr/empty-graph
                   (line-seq rdr)))))
