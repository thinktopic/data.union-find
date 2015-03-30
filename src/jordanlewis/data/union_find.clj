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

(defprotocol TransientDisjointSetForest
  "A data structure that maintains information on a number of disjoint sets."
  (union! [this x y] "Union two sets. Return a mutated disjoint set forest with the
sets that x and y belong to unioned."))

(defprotocol IUFNode
  (value [n])
  (rank [n])
  (parent [n])
  (mutable-node? [n]))

(defprotocol IMutableUFNode
  (set-immutable! [n])
  (set-rank! [n r])
  (set-parent! [n p]))

(deftype ^:private UFNode [v r p]
  IUFNode
  (value [_] v)
  (rank [_] r)
  (parent [_] p)
  (mutable-node? [_] false)

  Object
  (equals [_ o]
    (boolean (and o
                  (= v (value o))
                  (= r (rank o))
                  (= p (parent o))))))

(deftype ^:private MutableUFNode [v ^:unsynchronized-mutable r ^:unsynchronized-mutable p ^:unsynchronized-mutable m]
  IUFNode
  (value [_] v)
  (rank [_] r)
  (parent [_] p)
  (mutable-node? [_] m)

  IMutableUFNode
  (set-immutable! [_]
    (set! m false))
  (set-rank! [n rank]
    (set! r rank))
  (set-parent! [n parent]
    (set! p parent))

  Object
  (equals [_ o]
    (boolean (and o
                  (= v (value o))
                  (= r (rank o))
                  (= p (parent o))))))

(declare empty-union-find)

(declare ->TransientDSF)

(deftype PersistentDSF [elt-map num-sets _meta]
  Object
  ;; prints out a map from canonical element to elements unioned to that element.
  (toString [this] (str (group-by this (keys @elt-map))))
  (hashCode [this] (.hashCode @elt-map))
  (equals [this that] (or (identical? this that)
                          (and (instance? PersistentDSF that)
                               (.equals @elt-map @(.elt-map that)))))

  clojure.lang.Seqable
  ;; seq returns all of the elements
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

  clojure.lang.IEditableCollection
  (asTransient [this]
    (->TransientDSF
     (transient @elt-map) num-sets _meta))

  clojure.lang.Counted
  ;; count returns the total number of elements
  (count [this] (count @elt-map))

  clojure.lang.ILookup
  ;; valAt gets the canonical element of the key without path compression
  (valAt [this k] (.valAt this k nil))
  (valAt [this k not-found]
    (loop [x k]
      (if-let [node (@elt-map x)]
        (if-let [parent (parent node)]
          (recur parent)
          x)
        not-found)))

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
          parent (when node (parent node))]
      (cond
        (= node nil) nil
        (= parent nil) x
        ;; path compression. set the parent of each node on the path we take
        ;; to the root that we find.
        :else (let [canonical (get-canonical this parent)]
                (dosync
                  (alter elt-map (fn [m] (update-in m [x] #(->UFNode (value %) (rank %) canonical)))))
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
          x-node (elt-map x-root)
          y-node (elt-map y-root)
          x-rank (when x-node (rank x-node))
          y-rank (when y-node (rank y-node))]
      (cond (or (nil? x-root) ;; no-op - either the input doesn't exist in the
                (nil? y-root) ;; universe, or the two inputs are already unioned
                (= x-root y-root)) this
            (< x-rank y-rank) (PersistentDSF.
                                (ref (update-in elt-map [x-root] #(->UFNode (value %) (rank %) y-root)))
                                num-sets _meta)
            (< y-rank x-rank) (PersistentDSF.
                                (ref (update-in elt-map [y-root] #(->UFNode (value %) (rank %) x-root)))
                                num-sets _meta)
            :else (PersistentDSF.
                    (ref
                      (-> elt-map
                          (transient)
                          (assoc! y-root (->UFNode (value y-node) (rank y-node) x-root))
                          (assoc! x-root (->UFNode (value x-node) (inc x-rank) (parent x-node)))
                          (persistent!)))
                    num-sets _meta))))
  (count-sets [this] num-sets))

(deftype TransientDSF [^:unsynchronized-mutable elt-map
                       ^:unsynchronized-mutable num-sets
                       meta]
  clojure.lang.Counted
  (count [this] (count elt-map))

  clojure.lang.ILookup
  ;; valAt gets the canonical element of the key without path compression
  (valAt [this k] (.valAt this k nil))
  (valAt [this k not-found]
    (loop [x k]
      (if-let [node (elt-map x)]
        (if-let [parent (parent node)]
          (recur parent)
          x)
        not-found)))

  clojure.lang.IFn
  ;; invoking as function behaves like valAt.
  (invoke [this k] (.valAt this k))
  (invoke [this k not-found] (.valAt this k not-found))

  clojure.lang.ITransientCollection
  (conj [this x]
    (if (elt-map x)
      this
      (do (set! elt-map (assoc! elt-map x (->MutableUFNode x 0 nil true)))
          (set! num-sets (inc num-sets))
          this)))

  (persistent [this]
    (let [elt-map (persistent! elt-map)]
      (doseq [node (vals elt-map)] (when (mutable-node? node) (set-immutable! node) ))
      (PersistentDSF. (ref elt-map) num-sets meta)))

  DisjointSetForest
  (get-canonical [this x]
    (let [node (elt-map x)
          parent (when node (parent node))]
      (cond
       (= node nil) nil
       (= parent nil) x
       ;; path compression. set the parent of each node on the path we take
       ;; to the root that we find.
       :else (let [canonical (get-canonical this parent)]
               (do (if (mutable-node? node)
                     (set-parent! node canonical)
                     (set! elt-map (assoc! elt-map x (->MutableUFNode (value node) (rank node) canonical true))))
                   canonical)))))
  (union [this x y]
    (throw (java.lang.UnsupportedOperationException "Use union! on transients")))

  TransientDisjointSetForest
  (union! [this x y]
    (let [x-root (get-canonical this x)
          y-root (get-canonical this y)
          x-node (elt-map x-root)
          y-node (elt-map y-root)
          x-rank (when x-node (rank x-node))
          y-rank (when y-node (rank y-node))]
      (cond (or (nil? x-root) ;; no-op - either the input doesn't exist in the
                (nil? y-root) ;; universe, or the two inputs are already unioned
                (= x-root y-root)) this
            (< x-rank y-rank)
            (do
              (if (mutable-node? x-node)
                (set-parent! x-node y-root)
                (set! elt-map (assoc! elt-map x-root (->MutableUFNode (value x-node) (rank x-node) y-root true))))
              (set! num-sets (dec num-sets))
              this)
            (< y-rank x-rank)
            (do
              (if (mutable-node? y-node)
                (set-parent! y-node x-root)
                (set! elt-map (assoc! elt-map y-root (->MutableUFNode (value y-node) (rank y-node) x-root true))))
              (set! num-sets (dec num-sets))
              this)
            :else
            (do
              (if (mutable-node? y-node)
                (set-parent! y-node x-root)
                (set! elt-map (assoc! elt-map y-root (->MutableUFNode (value y-node) (rank y-node) x-root true))))
              (if (mutable-node? x-node)
                (set-rank! x-node (inc x-rank))
                (set! elt-map (assoc! elt-map x-root (->MutableUFNode (value x-node) (inc x-rank) (parent x-node) true))))
              (set! num-sets (dec num-sets))
              this))))
  (count-sets [this] num-sets))

(def ^:private empty-union-find (->PersistentDSF (ref {}) 0 {}))

(defn union-find
  "Returns a new union-find data structure with provided elements as singletons."
  [& xs]
  (reduce conj empty-union-find xs))
