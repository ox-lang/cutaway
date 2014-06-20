(ns cutaway.core
  (:require [cutaway.uuids :refer :all]))


(defn cutaway?
  "(λ Object) → Boolean"

  [x]
  (and (map? x)
       (every? uuid? (keys x))))

(defn make-cutaway
  "(λ Map) → Cutaway"

  [x]
  {:pre [(map? x)]}
  x)


(defn fetch
  "(λ Cutaway → Id) → Any"

  [cutaway id]
  {:pre [(cutaway? cutaway)
         (uuid? id)]}
  (get cutaway id))


(defn typeof
  "(λ Cutaway → Id) → Keyword"

  [cutaway id]
  {:pre [(cutaway? cutaway)
         (uuid? id)]}
  (-> cutaway
      (get id)
      (get :type)))


(defn idof
  "(λ Map) → Uuid

  Function from an object presumed to have come out of the cutaway to its
  cutaway id, if it has one. The idea is that I can pack the ::self key in
  object metadata and that by doing so I not only hide an
  implementation detail but can build a nicer API for assembling and
  disassembling cutaway entities"

  [obj]
  (::self (meta obj)))


(defn resolve
  "(λ Cutaway → Map) → Map

  Takes a map, being {keyword → id} and recursively fetches the
  cutaway objects identified by that uuid until such time as the
  object has been fully realized into a simple Clojure map form.

  Note that the keyword ::self is ignored by this expansion, lest
  infinite recursion occur."

  [cutaway struct]
  {:pre [(cutaway? cutaway)
         (map? struct)]}
  (with-meta
    (->> (for [[k x] struct]
           [k
            (cond (map? x)
                  (resolve cutaway x)
                  
                  (vector? x)
                  (mapv (partial get cutaway) x)
                  
                  (uuid? x)
                  (->> x
                       (fetch cutaway)
                       (resolve cutaway))
                  true
                  x)])
         (into {}))
    {::self (idof struct)}))


(declare decompose)


(defn- decompose-seq
  [[cutaway ids] x]
  (if (map? x)
    (let [[cutaway id] (decompose cutaway x)]
      [cutaway (conj ids id)])
    [cutaway (conj ids x)]))


(defn- decomposer
  [[cutaway m] [k v]]
  (cond
   (map? v)
     (let [[cutaway id]
           (decompose cutaway v)]
       [cutaway (assoc m k id)])

   (or (list? v)
       (vector? v))
     (let [[cutaway ids]
           (reduce decompose-seq [cutaway []] v)]
       [cutaway (assoc m k ids)])

   true
     [cutaway (assoc m k v)]))


(defn decompose
  "(λ Cutaway → Map) → (Cutaway, Id)
   (λ Cutaway → Map → Id) → (Cutaway, Id)

  Breaks down nested maps recursively, allowing for the \"direct\"
  insertion and implicit restructuring of nested maps into multiple
  linked components. These nested linked components are installed into
  the cutaway, and an updated cutaway containing the flattened
  components is returned along with the UUID of obj itself.

  Note that decompose deliberately reuses uuids specified by the :self
  key. This means that round-tripping a datastructure from the cutaway
  through resolve, update-in to decompose is entirely valid as the
  desired effect of updating the nested datastructure(s) will be
  achieved. This update technique is massively inefficient performance
  wise and that the cuttaway pattern of identifying standard
  substructures and passing them by ID for direct manipulation is
  preferred."

  ([cutaway obj id]
     {:pre [(cutaway? cutaway)
            (uuid? id)]}
     (let [id (uuid)]
       (if (map? obj)
         (let [id (or (idof obj) id)
               [cutaway clean] (reduce decomposer [cutaway {}] obj)
               clean (with-meta clean {::self id})]
           [(assoc cutaway id clean) id])
         [(assoc cutaway id obj) id])))

  ([cutaway obj]
     {:pre [(cutaway? cutaway)]}
     (decompose cutaway obj (uuid))))


(defn install
  "(λ Cutaway → Map → Id) → Cutaway

  Provides a wrapper around decompose for when you just want to
  install something to a known key quickly and cleanly."

  [cutaway obj id]
  {:pre [(cutaway? cutaway)
         (uuid? id)]}
  (first (decompose cutaway obj id)))
