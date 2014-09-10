(ns cutaway.core
  (:refer-clojure :exclude [resolve])
  (:require [cutaway.uuids :refer [uuid uuid?]]))

;; CUTAWAY API
;;------------------------------------------------------------------------------
(defn typeof
  "(λ cutaway -> id) -> keyword"
  [cutaway id]
  (-> cutaway
     (get id)
     (get :type)))

(defn idof
  "(λ map) -> uuid

  Function from an object presumed to have come out of the CUTAWAY to its
  CUTAWAY id, if it has one. The idea is that I can pack the :self key in
  object metadata and that by doing so I not only hide an
  implementation detail but can build a nicer API for assembling and
  disassembling CUTAWAY entities"
  [obj]
  (:self (meta obj)))

(defn resolve
  "(λ cutaway -> map) -> map

  Takes a map, being {<keyword> <uuid>} and recursively fetches the
  CUTAWAY objects identified by that uuid until such time as the object
  has been fully realized into a simple Clojure map form.

  Note that the keyword :self is ignored by this expansion, lest
  infinite recursion occur."
  [cutaway struct]
  (->> struct
     (vals)
     (mapv (fn [x]
             (cond (map? x)
                   (resolve cutaway x)

                   (or (vector? x)
                      (list? x))
                   (mapv (partial get cutaway) x)

                   (uuid? x)
                   (->> x
                      (get cutaway)
                      (resolve cutaway))
                   true
                   x)))
     (zipmap (keys struct))
     ((fn [x] (with-meta x
               {:self (idof struct)})))))

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
  "(λ cutaway -> entry) -> (cutaway, uuid)
  (λ cutaway → entry → uuid) → (cutaway, uuid)

  Breaks down nested maps recursively, allowing for the \"direct\"
  insertion and implicit restructuring nested maps into multiple
  linked components. These nested linked components are installed into
  the CUTAWAY, and an updated CUTAWAY containing the flattened components is
  returned along with the UUID of obj itself.

  Note that decompose deliberately reuses uuids specified by the :self
  key in the two arity case and recursively. This means that
  round-tripping a datastructure from the CUTAWAY through resolve,
  update-in to decompose is entirely valid as the desired effect of
  updating the nested datastructure(s) will be achieved. This update
  technique is massively inefficient performance wise and that the CUTAWAY
  accutaways pattern of identifying standard substructures and passing
  them by ID for direct manipulation is preferred."

  ([cutaway obj id]
     (let [id (uuid)]
       (if (map? obj)
         (let [id (or (idof obj) id)
               [cutaway clean] (reduce decomposer [cutaway {}] obj)
               clean (with-meta clean {:self id})]
           [(assoc cutaway id clean) id])
         [(assoc cutaway id obj) id])))

  ([cutaway obj]
     (decompose cutaway obj 
                (or (:self (meta obj))
                   (uuid)))))

(defn upsert
  "(λ cutaway -> entry) -> cutaway

  Provides a wrapper around decompose for when you just want to
  reinstall something to a known key quickly and cleanly or if you
  don't care where it goes."

  [cutaway obj]
  (first (decompose cutaway obj)))

(defn- matches?
  [cache cutaway entity pattern]
  (let [c   @cache
        key [entity pattern]]
    (if (contains? c key)
      (get c key)
      (let [res (every? identity
                   (for [[k p] pattern]
                     (cond (and (map? pattern)
                              (uuid? (get entity k)))
                           ,,(matches? cache cutaway (get cutaway (get entity k)) p)

                           (and (set? pattern)
                              (vector? (get entity k)))
                           ,,(let [v (set (get entity k))]
                               (every? identity
                                  (for [e pattern]
                                    (matches? cache cutaway e v))))
                           :else
                           ,,(= p (get entity k)))))]
        (swap! cache assoc key res)
        res))))

(defn search
  "Applies a form of pattern matching to the elements of a CUTAWAY,
  returning the set of node identifiers which match the pattern in no
  guranteed order.

  Pattern structure:

    Patterns are presumed to be maps. If a key exists in a pattern,
    that key must exist in all matches of that pattern.

    Ex.
      The pattern {:foo _} will match only maps where the key :foo
      is set and the _ term matches the value of the key foo in that
      map.

    If the value of a key is neither a set nor a map, the value is
    interpreted to be a constant and will only match equal values.

    Ex.
      The pattern {:foo :bar} will match only the set of maps such
      that the pair [:foo :bar] exists in the map.

    If the value of a key is a map, it will only match recursively
    equal map structures.

    Ex.
      The pattern {:op :invoke :fn {:op :var}}
      will match only the set of structures such that the \"parent\"
      has the pair [:op :invoke], and the child on the :fn key matches
      the pattern {:op :var}

    If the value of a key is a set, it will match only sequencutaway such
    that all elements of the set match an element of the sequence.

    Ex.
      The pattern {:children #{:body}} will match only the set of maps
      such that the value of the :children key _contains_ the value
      :body.

    TODO:
      If the value of a key is a sequence, it will match only
      sequencutaway such that the ith element of the pattern sequence
      matches the ith element of the target sequence."
  [cutaway pattern]
  (let [cache (atom {})]
    (for [[k e] cutaway
          :when (matches? cache cutaway e pattern)]
      k)))

(defn update-where
  [cutaway pattern f]
  (reduce (fn [cutaway match]
            (upsert cutaway (f (get cutaway match))))
          cutaway (search cutaway pattern)))

;; Example
;;--------------------------------------------------------------------
(let [cutaway (-> {}
             (upsert {:op       :invoke
                      :children [:fn :args]
                      :fn       {:op :var
                                 :sym 'clojure.core/+}
                      :args     [{:op   :const
                                  :cost 3}
                                 {:op    :const
                                  :const 4}]})
             (upsert {:op       :def
                      :children [:var :init]
                      :var      {:op  :var
                                 :sym 'user/foo}
                      :init     {:op       :fn
                                 :children [:methods]
                                 :methods  [{:op       :fn-method
                                             :children [:ret]
                                             :ret      {:op       :invoke
                                                        :children [:fn :args]
                                                        :fn       {:op  :var
                                                                   :sym 'clojure.core/-}
                                                        :args     [{:op  :const
                                                                    :val -1}
                                                                   {:op  :const
                                                                    :val -1}]}}
                                            {:op :fn-method
                                             :children [:ret]
                                             :ret {:op       :let
                                                   :children [:bindings :ret]
                                                   :bindings [{:op       :binding
                                                               :children [:val]
                                                               :sym      'quxx
                                                               :val      {:op :fn}}]
                                                   :ret      {:op :invoke
                                                              :fn {:op  :local
                                                                   :sym 'quxx}}}}
                                            {:op :fn-method}]}}))]
  (println (search cutaway {:op :invoke}))
  (println (search cutaway {:op :invoke :fn {:op :var}}))
  (println (search cutaway {:op :const})))
