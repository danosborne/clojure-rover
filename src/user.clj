(ns user
  (:require
   [clojure.tools.namespace.repl :as nsrepl]
    [clojure.stacktrace :as st]
    [clojure.repl :refer [doc source]]
    [clojure.pprint :refer [pprint pp]]))

(defn refresh [] (nsrepl/refresh))
