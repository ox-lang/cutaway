(ns cutaway.uuids
  "Defines some quick and dirty helpers for interacting with Java
  uuids from Clojure. Why these aren't in clojure.uuid is an open
  question for the ages and Rich.")

(defn uuid [] 
  (java.util.UUID/randomUUID))

(defn uuid? [x]
  (= (class x)
     java.util.UUID))
