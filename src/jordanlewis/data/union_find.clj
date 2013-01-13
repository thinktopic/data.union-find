(ns
  ^{:doc "Persistent disjoint set forests using Tarjan's union-find algorithm."
    :author "Jordan Lewis"}
  jordanlewis.data.union-find)

(defprotocol DisjointSetForest
  "A data structure that maintains information on a number of disjoint sets."
  (union [this x y] "Union two sets. Return a new disjoint set forest with the
sets that x and y belong to unioned.")
  (count-sets [this] "Get the number of disjoint sets in this forest.")
  (get-canonical [this x] "Get the canonical element of an element, or nil if no
element exists in the forest."))

(defrecord ^:private UFNode [value rank parent])

(declare empty-union-find)

(deftype PersistentDSF [elt-map num-sets _meta]
  Object
  ;; prints out a map from canonical element to elements unioned to that element.
  (toString [this] (str (group-by this (keys @elt-map))))
  (hashCode [this] (.hashCode @elt-map))
  (equals [this that] (or (identical? this that) (.equals @elt-map @(.elt-map that))))

  clojure.lang.Seqable
  ;; seq returns each of the canonical elements, not all of the elements
  (seq [this]
    (keys @elt-map))

  clojure.lang.IPersistentCollection
  ;; cons adds the input to a new singleton set
  (cons [this x]
    (if (@elt-map x)
      this
      (PersistentDSF. (ref (assoc @elt-map x (->UFNode x 0 nil))) (inc num-sets) _meta)))
  (empty [this] empty-union-find)
  (equiv [this that] (.equals this that))

  ;; count returns the number of disjoint sets, not the number of total elements
  clojure.lang.Counted
  (count [this] (count @elt-map))

  clojure.lang.ILookup
  ;; valAt gets the canonical element of the key without path compression
  (valAt [this k] (.valAt this k nil))
  (valAt [this k not-found]
    (if-let [canonical (get-canonical this k)]
      canonical
      not-found))

  clojure.lang.IFn
  ;; invoking as function behaves like valAt.
  (invoke [this k] (.valAt this k))
  (invoke [this k not-found] (.valAt this k not-found))

  ;; implementing IMeta and IObj gives us meta
  clojure.lang.IMeta
  (meta [this] _meta)
  clojure.lang.IObj
  (withMeta [this meta] (PersistentDSF. (ref @elt-map) num-sets meta))

  DisjointSetForest
  (get-canonical [this x]
    (let [node (@elt-map x)
          parent (:parent node)]
      (cond
        (= node nil) nil
        (= parent nil) x
        ;; path compression. set the parent of each node on the path we take
        ;; to the root that we find.
        :else (let [canonical (get-canonical this parent)]
                (dosync
                  (alter elt-map #(assoc-in % [x :parent] canonical)))
                canonical))))
  (union [this x y]
    (let [x-root (get-canonical this x)
          y-root (get-canonical this y)
          ;; grab consistent version of our map. I'm pretty sure this isn't strictly
          ;; necessary, since the only thing that can change about the elt-map under
          ;; our nose is compressing paths, which never 'screws up' the map. Grabbing
          ;; a consistent version immediately could throw away some path compressions
          ;; if they occur in the middle of a union operation.
          ;; TODO determine whether this is necessary, remove if not
          elt-map @elt-map
          num-sets (dec num-sets)
          x-rank (:rank (elt-map x-root))
          y-rank (:rank (elt-map y-root))]
      (cond (or (nil? x-root) ;; no-op - either the input doesn't exist in the
                (nil? y-root) ;; universe, or the two inputs are already unioned
                (= x-root y-root)) this
            (< x-rank y-rank) (PersistentDSF.
                                (ref (assoc-in elt-map [x-root :parent] y-root))
                                num-sets _meta)
            (< y-rank x-rank) (PersistentDSF.
                                (ref (assoc-in elt-map [y-root :parent] x-root))
                                num-sets _meta)
            :else (PersistentDSF.
                    (-> elt-map
                      (transient)
                      (assoc! y-root (assoc (elt-map y-root) :parent x-root))
                      (assoc! x-root (assoc (elt-map x-root) :rank (inc x-rank)))
                      (persistent!)
                      (ref))
                    num-sets _meta))))
  (count-sets [this] num-sets))

(def ^:private empty-union-find (->PersistentDSF (ref {}) 0 {}))

(defn union-find
  "Returns a new union-find data structure with provided elements as singletons."
  [& xs]
  (reduce conj empty-union-find xs))
