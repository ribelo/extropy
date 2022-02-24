(ns ribelo.extropy
  (:refer-clojure :exclude [-first -assoc -assoc! -dissoc -dissoc! -reduce -remove -count])
  #?(:cljs (:require-macros [ribelo.extropy :refer [-if-clj -if-cljs -now-dt* -now-udt* -now-nano* -iter* -get* -loop
                                                    -loop-all -run! -k* -v* -reduce -time-ms -time-ns -qb
                                                    -count* -assoc* -assoc!* -dissoc* -dissoc!*
                                                    -ms* -compose-preds -compose-set-intersection -compose-map-intersection
                                                    -compose-vector-intersection -compose-vector-diffrence
                                                    -compose-cartesian-product-function -compose-union
                                                    -compose-cartesian-product -catching -catch-errors
                                                    -wrap-promise->
                                                    ]]))
  #?(:cljs
     (:require
       [goog.string :as gstr]))
  #?(:clj
     (:import
      (java.util Map HashMap HashSet Iterator ArrayDeque ArrayList)
      (java.lang Iterable))))

#?(:cljs
   (if (exists? js/window)
     (def -crypto js/crypto)
     (def -crypto (js/require "crypto"))))

#?(:clj (set! *warn-on-reflection* true))

(def -sentinel #?(:clj (Object.) :cljs (js/Object.)))

(defmacro -if-clj  [then & [else]] (if (:ns &env) else then))
(defmacro -if-cljs [then & [else]] (if (:ns &env) then else))

(def -clj?  (-if-clj  true false))
(def -cljs? (-if-cljs true false))

(defmacro -now-dt* [] `(-if-clj (java.util.Date.) (js/Date.)))

(defn -now-dt []
  (-now-dt*))

(defmacro -now-udt* []
  `(-if-clj (System/currentTimeMillis) (.getTime (js/Date.))))

(defn -now-udt []
  (-now-udt*))

#?(:clj
   (defn -now-nano ^long [] (System/nanoTime))

   :cljs
   (def -now-nano
     (if (and (exists? js/window) (aget js/window "performance"))
       (fn [] (* 1000000 (js/performance.now)))
       (fn [] (* 1000000 (-now-udt))))))

(defmacro -now-nano* []
  `(-if-clj (System/nanoTime) (-now-nano)))

(defmacro -iter* ^Iterator [xs]
  `(-if-clj (.iterator ~(with-meta xs {:tag 'Iterable}))
            (cljs.core/iter ~xs)))

(defn -iter ^Iterator [xs]
  (-iter* xs))

(defn -iter? [x]
  #?(:clj
     (instance? Iterator x)
     :cljs
     (iterable? (iter [1 2 3]))
     (or (instance? cljs.core/RangedIterator x)
         (instance? cljs.core/RangeIterator x)
         (instance? cljs.core/IndexedSeqIterator x)
         (instance? cljs.core/PersistentArrayMapIterator x)
         (instance? cljs.core/ArrayNodeIterator x)
         (instance? cljs.core/MultiIterator x)
         (instance? cljs.core/NodeIterator x))))

(defn -ensure-iter ^Iterator [x]
  (if (-iter? x) x (-iter* x)))

(defn -array-list
  (^ArrayList [] (-array-list []))
  (^ArrayList [xs]
   #?(:clj  (ArrayList. ^java.util.Collection xs)
      :cljs (to-array xs))))

(defn -native-map
  (^HashSet [] (-native-map {}))
  (^HashSet [^Map xs]
   #?(:clj  (HashMap. xs)
      :cljs (reduce-kv (fn [acc k v] (doto acc (aset k v))) #js {} xs))))

(defn -native-set
  (^HashSet [] (-native-set []))
  (^HashSet [^java.util.Collection xs]
   #?(:clj  (HashSet. xs)
      :cljs (js/Set. xs))))

(declare -mapv)

;; https://github.com/rauhs/clj-bench/blob/master/src/clj_bench/loop_it.clj#L14
(defn -multi-iter
  (^Iterator [colls]
   ;; Copied from clojure.lang.MultiIterator
   (let [iters ^{:tag "[Ljava.util.Iterator;"} (into-array (-mapv -ensure-iter colls))]
     (reify #?(:clj Iterator :cljs Object)
       (hasNext [this]
         (let [len (alength iters)]
           (loop [i 0]
             (if (< i len)
               (let [it (aget iters i)]
                 (if (.hasNext ^Iterator it)
                   (recur (unchecked-inc i))
                   false))
               true))))
       (next [this]
         (let [len (alength iters)
               arr #?(:clj (object-array len) :cljs (array))]
           (dotimes [i len]
             (let [it (aget iters i)]
               (aset arr i (.next ^Iterator it))))
           (vec arr))))))
  (^Iterator [xf colls]
   #?(:clj (clojure.lang.TransformerIterator/createMulti xf (-mapv -ensure-iter colls))
      :cljs (.create cljs.core/TransformerIterator xf (-mapv -ensure-iter colls)))))

;; https://github.com/rauhs/clj-bench/blob/master/src/clj_bench/loop_it.clj
(defmacro -loop
  {:style/indent :defn}
  [bindings body & finish]
  {:pre [(vector? bindings) (even? (count bindings))
         (vector? (last bindings)) (= :let (last (butlast bindings)))]}
  (let [it-binds (butlast (butlast bindings))
        sym-binds (take-nth 2 it-binds)
        colls (take-nth 2 (rest it-binds))
        iter-syms (repeatedly (count sym-binds) #(gensym "iter"))]
    `(let ~(vec (mapcat (fn [it coll]
                          [it `(-ensure-iter ~coll)])
                        iter-syms colls))
       (loop ~(vec (last bindings))
         (if (and ~@(map (fn [it] `(.hasNext ~(with-meta it {:tag 'java.util.Iterator}))) iter-syms))
           (let ~(vec (mapcat (fn [bind it]
                                [bind `(.next ~(with-meta it {:tag 'java.util.Iterator}))])
                              sym-binds iter-syms))
             ~body)
           (do ~@finish))))))

(defmacro -loop-all
  {:style/indent :defn}
  [bindings body & finish]
  {:pre [(vector? bindings) (even? (count bindings))
         (vector? (last bindings)) (= :let (last (butlast bindings)))]}
  (let [it-binds (butlast (butlast bindings))
        sym-binds (take-nth 2 it-binds)
        colls (take-nth 2 (rest it-binds))
        iter-syms (repeatedly (count sym-binds) #(gensym "iter"))]
    `(let ~(vec (mapcat (fn [it coll]
                          [it `(-ensure-iter ~coll)])
                        iter-syms colls))
       (loop ~(vec (last bindings))
         (if (or ~@(map (fn [it] `(.hasNext ~it)) iter-syms))
           (let ~(vec (mapcat (fn [bind it]
                                [bind `(if (.hasNext ~it) (.next ~it) -sentinel)])
                              sym-binds iter-syms))
             ~body)
           (do ~@finish))))))

(defn -run! [f xs]
  (let [it (-ensure-iter xs)]
    (loop []
      (when (.hasNext it)
        (when-not (reduced? (f (.next it)))
          (recur))))))

(defmacro -k* [m]
  `(-if-clj
     (.key  ~(with-meta m {:tag 'clojure.lang.MapEntry}))
     (.-key ~(with-meta m {:tag 'cljs.core.MapEntry}))))

(defn -k [m]
  (-k* m))

(defmacro -v* [m]
  `(-if-clj
     (.val  ~(with-meta m {:tag 'clojure.lang.MapEntry}))
     (.-val ~(with-meta m {:tag 'cljs.core.MapEntry}))))

(defn -v [m]
  (-v* m))

#?(:clj
   (defn -reduce
     ([f xs]
      (let [iter (-iter xs)]
        (if (.hasNext iter)
          (let [init (.next iter)]
            (loop [acc init]
              (if (.hasNext iter)
                (let [acc' (f acc (.next iter))]
                  (if (reduced? acc')
                    @acc'
                    (recur acc')))
                acc)))
          (f))))
     ([f init xs]
      (let [iter (-iter xs)]
        (loop [acc init]
          (if (.hasNext iter)
            (let [acc' (f acc (.next iter))]
              (if (reduced? acc')
                @acc'
                (recur acc')))
            acc)))))
   :cljs
   (def -reduce reduce))

#?(:clj
   (defn -reduce-kv
     ([f coll]
      (let [iter (-iter coll)]
        (if (.hasNext iter)
          (let [init (.next iter)]
            (loop [acc init]
              (if (.hasNext iter)
                (let [me (.next iter)
                      acc' (f acc (-k me) (-v me))]
                  (if (reduced? acc')
                    @acc'
                    (recur acc')))
                acc)))
          (f))))
     ([f init coll]
      (let [iter (-iter coll)]
        (loop [acc init]
          (if (.hasNext iter)
            (let [me (.next iter)
                  acc' (f acc (-k me) (-v me))]
              (if (reduced? acc')
                @acc'
                (recur acc')))
            acc)))))

   :cljs
   (def -reduce-kv reduce-kv))

(defn -reduce-kvs
  [rf init kvs]
  (-loop [x kvs :let [acc init k nil v nil]]
    (if (and k v)
      (recur (rf acc k v) x nil)
      (cond
        (nil? k)
        (recur acc x nil)
        (nil? v)
        (recur (rf acc k x) nil nil)))
    acc))

(defn -kw-identical? [x y]
  #?(:clj
     (identical? x y)
     :cljs
     (keyword-identical? x y)))

(defmacro -get*
  ([m k]
   `(-if-clj
     ~(if (symbol? m)
        `(.valAt ~(with-meta m {:tag 'clojure.lang.ILookup}) ~k)
        `(let [m# ~m] (-get* m# ~k)))
     (cljs.core/-lookup ~m ~k)))
  ([m k not-found]
   `(-if-clj
     ~(if (symbol? m)
        `(.valAt ~(with-meta m {:tag 'clojure.lang.ILookup}) ~k ~not-found)
        `(let [m# ~m] (-get* m# ~k ~not-found)))
     (if (and ~m (cljs.core/-contains-key? ~m ~k)) (cljs.core/-lookup ~m ~k) ~not-found))))

(defn -get
  ([m k]
   (when m (-get* m k)))
  ([m k not-found]
   (if m (-get* m k not-found) not-found)))

(defn -get-in
  ([m ks]
   (-loop [k ks :let [acc m]] (if-let [x (-get* acc k)] (recur x) nil) acc))
  ([m ks not-found]
   (-loop [k ks :let [acc m]]
     (let [x (-get* acc k -sentinel)]
       (if (identical? x -sentinel)
         not-found
         (recur x)))
     acc)))

(defn -first [xs]
  (-> (-iter* xs) .next))

(defn -ffirst [xs]
  (-> (-first xs) -first))

(defn -second [xs]
  (.next (doto (-iter* xs) .next)))

(defmacro -count* [xs]
  `(-if-clj
    ~(if (symbol? xs)
       `(.count ~(with-meta xs {:tag 'clojure.lang.Counted}))
       `(let [xs# ~xs] (-count* xs# xs)))
    (cljs.core/count ~xs)))

(defn -count [xs]
  (-count* xs))

(defn -last [xs]
  (nth xs (dec (-count xs))))

(defn -first-key [xs]
  (first (keys xs)))

(defn -first-val [xs]
  (first (vals xs)))

(declare -assoc! -assoc-some!)

(defn -select-keys [m xs]
  (-loop [k xs :let [acc (transient {})]]
    (recur (-assoc-some! acc k (-get* m k)))
    (persistent! acc)))

(defn -maybe-transient [xs]
  (if #?(:clj  (instance? clojure.lang.IEditableCollection xs)
         :cljs (satisfies? cljs.core.IEditableCollection xs))
    (.asTransient ^clojure.lang.IEditableCollection xs)
    xs))

(defn -transient? [xs]
  #?(:clj  (instance? clojure.lang.ITransientCollection xs)
     :cljs (satisfies? cljs.core.ITransientCollection xs)))

(defn -ensure-persisten! [xs]
  (if (-transient? xs) (persistent! xs) xs))

(defn -ensure-transient ^clojure.lang.ITransientCollection [xs]
  (if (-transient? xs) xs (transient xs)))

(defn -maybe-persistent! [xs]
  (if #?(:clj  (instance? clojure.lang.ITransientCollection xs)
         :cljs (satisfies? cljs.core.ITransientCollection xs))
    #?(:clj  (.persistent ^clojure.lang.ITransientCollection xs)
       :cljs (-persistent! xs))
    xs))

(defn -recursive-transient [xs]
  (if #?(:clj (instance? Iterable xs) :cljs (iterable? xs))
    (-loop [x xs :let [acc (transient (empty xs))]]
      (cond
        (map? xs)
        (recur (-assoc! acc (nth x 0) (-recursive-transient (nth x 1))))
        (or (vector? xs) (set? xs))
        (recur (conj! acc (-recursive-transient x))))
      acc)
    (-maybe-transient xs)))

(defn -recursive-persistent! [xs]
  (let [xs' (-maybe-persistent! xs)]
    (if #?(:clj (instance? Iterable xs') :cljs (iterable? xs'))
      (-loop [x xs' :let [acc (transient (empty xs'))]]
        (cond
          (map? xs')
          (recur (-assoc! acc (nth x 0) (-recursive-persistent! (nth x 1))))
          (or (vector? xs') (set? xs'))
          (recur (conj! acc (-recursive-persistent! x))))
        (persistent! acc))
      xs')))

(defn -deque
  (^ArrayDeque [] (-deque []))
  (^ArrayDeque [^java.util.Collection xs]
   #?(:clj (java.util.ArrayDeque. xs) :cljs (to-array xs))))

(defn -comp
  ([] identity)
  ([f] f)
  ([f g] (fn [x] (f (g x))))
  ([f g h] (fn [x] (f (g (h x)))))
  ([f1 f2 f3 f4] (fn [x] (-> x f4 f3 f2 f1)))
  ([f1 f2 f3 f4 f5] (fn [x] (-> x f5 f4 f3 f2 f1)))
  ([f1 f2 f3 f4 f5 f6] (fn [x] (-> x f6 f5 f4 f3 f2 f1)))
  ([f1 f2 f3 f4 f5 f6 f7] (fn [x] (-> x f7 f6 f5 f4 f3 f2 f1)))
  ([f1 f2 f3 f4 f5 f6 f7 f8] (fn [x] (-> x f8 f7 f6 f5 f4 f3 f2 f1)))
  ([f1 f2 f3 f4 f5 f6 f7 f8 & fs]
   (-comp
     (apply -comp fs)
     (fn [x] (-> x f8 f7 f6 f5 f4 f3 f2 f1)))))

(defmacro -assoc* [m k v]
  `(-if-clj
    ~(if (symbol? m)
       `(.assoc ~(with-meta m {:tag 'clojure.lang.Associative}) ~k ~v)
       `(let [m# ~m] (-assoc* m# ~k ~v)))
    (cljs.core/-assoc ~m ~k ~v)))

(defmacro -assoc!* [m k v]
  `(-if-clj
    ~(if (symbol? m)
       `(.assoc ~(with-meta m {:tag 'clojure.lang.ITransientAssociative}) ~k ~v)
       `(let [m# ~m] (-assoc!* m# ~k ~v)))
    (cljs.core/-assoc! ~m ~k ~v)))

(defn -assoc
  ([m k v]
   (-assoc* m k v))
  ([m k1 v1 k2 v2]
   (-> (-assoc m k1 v1) (-assoc k2 v2)))
  ([m k1 v1 k2 v2 k3 v3]
   (-> (-assoc m k1 v1) (-assoc k2 v2) (-assoc k3 v3)))
  ([m k1 v1 k2 v2 k3 v3 k4 v4]
   (-> (-assoc m k1 v1) (-assoc k2 v2) (-assoc k3 v3) (-assoc k4 v4)))
  ([m k1 v1 k2 v2 k3 v3 k4 v4 & kvs]
   (-reduce-kvs -assoc (-> (-assoc m k1 v1) (-assoc k2 v2) (-assoc k3 v3) (-assoc k4 v4)) kvs)))

(defn -assoc!
  ([m k v]
   (-assoc!* (or m (transient {})) k v))
  ([m k1 v1 k2 v2]
   (-> (-assoc! m k1 v1) (-assoc! k2 v2)))
  ([m k1 v1 k2 v2 k3 v3]
   (-> (-assoc! m k1 v1) (-assoc! k2 v2) (-assoc! k3 v3)))
  ([m k1 v1 k2 v2 k3 v3 k4 v4]
   (-> (-assoc! m k1 v1) (-assoc! k2 v2) (-assoc! k3 v3) (-assoc! k4 v4)))
  ([m k1 v1 k2 v2 k3 v3 k4 v4 & kvs]
   (-reduce-kvs -assoc! (-> (-assoc! m k1 v1) (-assoc! k2 v2) (-assoc! k3 v3) (-assoc! k4 v4)) kvs)))

(defn -assoc-in [m [k & ks] v]
  #?(:clj
     (if ks
       (.assoc ^clojure.lang.Associative m k (-assoc-in (-get* m k) ks v))
       (.assoc ^clojure.lang.Associative m k v))

     :cljs
     (if ks
       (cljs.core/-assoc m k (-assoc-in (-get* m k) ks v))
       (cljs.core/-assoc m k v))))

(defn -assoc-in! [m [k & ks] v]
  #?(:clj
     (if ks
       (.assoc ^clojure.lang.ITransientAssociative m k (-assoc-in! (-get* m k) ks v))
       (.assoc ^clojure.lang.ITransientAssociative m k v))

     :cljs
     (if ks
       (cljs.core/-assoc! m k (-assoc-in! (-get* m k) ks v))
       (cljs.core/-assoc! m k v))))

(defn -assoc-some
  ([m k v]
   (if (nil? v) (or m {}) (-assoc m k v)))
  ([m k v & kvs]
   (persistent!
    (-reduce-kvs
     (fn [acc k v] (if (nil? v) acc (-assoc! acc k v)))
     (transient (-assoc-some m k v))
     kvs))))

(defn -assoc-some!
  ([m k v]
   (if (nil? v) (or m (transient {})) (-assoc! m k v)))
  ([m k v & kvs]
   (-reduce-kvs
    (fn [acc k v] (if (nil? v) acc (-assoc! acc k v)))
    (-assoc-some! m k v)
    kvs)))

(defn -update
  ([m k f]
   (-assoc m k (f (-get* m k))))
  ([m k f x]
   (-assoc m k (f (-get* m k) x)))
  ([m k f x y]
   (-assoc m k (f (-get* m k) x y)))
  ([m k f x y z]
   (-assoc m k (f (-get* m k) x y z)))
  ([m k f x y z & more]
   (-assoc m k (apply f (-get* m k) x y z more))))

(defn -update!
  ([m k f]
   (-assoc! m k (f (-get* m k))))
  ([m k f x]
   (-assoc! m k (f (-get* m k) x)))
  ([m k f x y]
   (-assoc! m k (f (-get* m k) x y)))
  ([m k f x y z]
   (-assoc! m k (f (-get* m k) x y z)))
  ([m k f x y z & more]
   (-assoc m k (apply f (-get* m k) x y z more))))

(defn -update-in
  ([m ks f & args]
   (let [up (fn up [m ks f args]
              (let [[k & ks] ks]
                (if ks
                  (-assoc m k (up (-get* m k) ks f args))
                  (-assoc m k (apply f (-get* m k) args)))))]
     (up m ks f args))))

(defmacro -dissoc* [m k]
  `(-if-clj
    (clojure.lang.RT/dissoc ~m ~k)
    (cljs.core/-dissoc ~m ~k)))

(defn -dissoc
  ([m k]
   (-dissoc* m k))
  ([m k & ks]
   (-loop [k' ks :let [acc (-dissoc* m k)]]
     (recur (-dissoc* acc k'))
     acc)))

(defn -dissoc-in
  ([m ks]
   (-update-in m (pop ks) -dissoc (peek ks))))

(defmacro -dissoc!* [m k]
  `(-if-clj
    ~(if (symbol? m)
       `(.without ~(with-meta m {:tag 'clojure.lang.ITransientMap}) ~k)
       `(let [m# ~m] (-dissoc!* m# ~k)))
    (cljs.core/-dissoc! ~m ~k)))

(defn -dissoc!
  ([m k]
   (-dissoc!* m k))
  ([m k & ks]
   (-loop [k' ks :let [acc (-dissoc!* m k)]]
     (recur (-dissoc!* acc k'))
     acc)))

(defn -update-in!
  ([m ks f & args]
   (let [up (fn up [m ks f args]
              (let [[k & ks] ks]
                (if ks
                  (-assoc! m k (up (-get* m k (transient {})) ks f args))
                  (-assoc! m k (apply f (-get* m k (transient {})) args)))))]
     (up m ks f args))))

(defn -conj-some
  ([xs x]
   (if-not (or (nil? x) (identical? x -sentinel)) (conj xs x) xs))
  ([xs x & more]
   (if-not (or (nil? x) (identical? x -sentinel))
     (recur (-conj-some xs x) (-first more) (next more))
     xs)))

(defn -conj-some!
  ([xs x]
   (if-not (or (nil? x) (identical? x -sentinel)) (conj! xs x) xs))
  ([xs x & more]
   (if-not (or (nil? x) (identical? x -sentinel))
     (recur (-conj-some! xs x) (-first more) (next more))
     xs)))

(def -conjv (fnil conj []))
(def -conjv! (fnil conj! (transient [])))
(def -conjs (fnil conj #{}))
(def -conjs! (fnil conj! (transient #{})))

(defn -into-all
  ([to from       ]
   (if-not (-transient? to) (into to from) (-reduce conj! to from)))
  ([to from & more]
   (persistent!
    (-reduce (fn [acc in] (-reduce conj! acc in))
      (-ensure-transient to)
      (cons from more)))))

(defn -merge
  ([] {})
  ([m] m)
  ([m1 m2]
   (if m1 (-into-all m1 m2) m2))
  ([m1 m2 m3]
   (-into-all (or m1 {}) m2 m3))
  ([m1 m2 m3 m4]
   (-into-all (or m1 {}) m2 m3 m4))
  ([m1 m2 m3 m4 & maps]
   (apply -into-all m1 m2 m3 m4 maps)))

(def -merge! -merge)

(defn -rename-keys [m kmap]
  (-loop [me kmap :let [acc (transient m)]]
    (let [k (-k* me)
          v (-v* me)]
      (if-let [x (-get* m k)]
        (recur (-dissoc!* (-assoc!* acc v x) k))
        acc))
    (persistent! acc)))

;; https://github.com/ptaoussanis/encore/blob/master/src/taoensso/encore.cljc#L2942
#?(:clj
   (defn -rcompare
     {:inline (fn [x y] (. clojure.lang.Util compare ~y ~x))}
     [x y]
     (compare y x))

   :cljs
   (defn -rcompare
     [x y]
     (compare y x)))

(defn -empty? [xs]
  (zero? (-count* xs)))

(defn -not-empty [xs]
  (when-not (zero? (-count* xs)) xs))

(defn -mapv
  ([f xs]
   (-loop [x xs :let [acc (transient [])]]
     (recur (conj! acc (f x)))
     (persistent! acc)))
  ([f xs1 xs2]
   (-loop [x1 xs1 x2 xs2  :let [acc (transient [])]]
     (recur (conj! acc (f x1 x2)))
     (persistent! acc)))
  ([f xs1 xs2 xs3]
   (-loop [x1 xs1 x2 xs2 x3 xs3 :let [acc (transient [])]]
     (recur (conj! acc (f x1 x2 x3)))
     (persistent! acc)))
  ([f xs1 xs2 xs3 xs4]
   (-loop [x1 xs1 x2 xs2 x3 xs3 x4 xs4 :let [acc (transient [])]]
     (recur (conj! acc (f x1 x2 x3 x4)))
     (persistent! acc)))
  ([f xs1 xs2 xs3 xs4 & colls]
   (-loop [xs (-multi-iter (into [xs1 xs2 xs3 xs4] colls)) :let [acc (transient [])]]
     (recur (conj! acc (apply f xs)))
     (persistent! acc))))

(defn -filter
  ([pred xs]
   (-loop [x xs :let [acc (transient (empty xs))]]
     (if (pred x)
       (recur (conj! acc x))
       (recur acc))
     (persistent! acc)))
  ([pred xs & colls]
   (-loop [x (-multi-iter (into [xs] colls)) :let [acc (transient (empty xs))]]
     (if (pred x)
       (recur (conj! acc x))
       (recur acc))
     (persistent! acc))))

(defn -remove [pred xs]
  (-filter (complement pred) xs))

(defn -keep [pred xs]
  (-loop [x xs :let [acc (transient [])]]
    (if-let [v (pred x)]
      (recur (conj! acc v))
      (recur acc))
    (persistent! acc)))

(defn -group-by [f data]
  (-loop [x data :let [acc (transient {})]]
    (recur (-update! acc (f x) -conjv x))
    (persistent! acc)))

;; (defn -reverse [xs]
;;   (-loop [x xs :let [acc (-deque)]]
;;     (recur (-add-first acc x))
;;     (vec acc)))

(defn -every? [pred xs]
  (-loop [x xs :let [acc true]]
    (if (pred x)
      (recur acc)
      false)
    acc))

(defn -some [pred xs]
  (-loop [x xs :let [acc false]]
    (if (pred x)
      true
      (recur acc))
    acc))

(defn -some= [x xs]
  (-some (fn [y] (= x y)) xs))

(defn -sort
  ([xs] (-sort compare xs))
  ([comp xs]
   (let [sorted (doto (-array-list xs) #?(:clj (java.util.Collections/sort comp) :cljs (.sort comp)))]
     (-loop [x sorted :let [acc (transient [])]]
       (recur (conj! acc x))
       (persistent! acc)))))

(defn -filter-keys [pred m]
  (-loop [me m :let [acc (transient m)]]
    (let [k (-k* me)]
      (if (pred k)
        (recur acc)
        (recur (-dissoc!* acc k))))
    (persistent! acc)))

(defn -filter-vals [pred m]
  (-loop [me m :let [acc (transient m)]]
    (let [k (-k* me)
          v (-v* me)]
      (if (pred v)
        (recur acc)
        (recur (-dissoc!* acc k))))
    (persistent! acc)))

(defn -sort-by
  ([kfn xs] (-sort-by kfn compare xs))
  ([kfn comp xs]
   #?(:clj  (-sort (fn [x y] (.compare ^java.util.Comparator comp (kfn x) (kfn y))) xs)
      :cljs (-sort (fn [x y] (comp (kfn x) (kfn y))) xs))))

(defmacro -compose-preds [f nc nf]
  (let [preds (gensym "preds__")
        this (gensym "composed-fn__")
        xs (repeatedly nf (partial gensym "x__"))
        p (gensym "p__")
        r (gensym "r__")
        fn-body (fn [i] (map (fn [j] `([~@(take j xs)] (~f ~@(map (fn [k] `((nth ~preds ~k) ~@(take j xs))) (range 0 i))))) (range 1 (inc nf))))
        cases (mapcat (fn [i] `[~i (fn ~@(fn-body i))]) (range 1 (inc nc))) ;TODO else more
        else `(let [~p (~this (take ~nc ~preds))
                    ~r (~this (drop ~nc ~preds))]
                (fn ~@(map (fn [i] `([~@(take i xs)] (~f (~p ~@(take i xs)) (~r ~@(take i xs))))) (range 1 (inc nf)))))
        body `([~preds]
               (case (count ~preds)
                 0 (constantly true)
                 ~@cases
                 ~else))
        ]
    `(fn ~this
       ~@body)))

(def ^{:arglists '([[& preds]])} -every-pred
  #?(:clj  (-compose-preds and 16 4)
     :cljs (fn [preds] (fn [x] (boolean (-reduce (fn [_ pred] (or (pred x) (reduced false))) true preds))))))

(def ^{:arglists '([[& preds]])} -some-pred
  #?(:clj  (-compose-preds or 16 4)
     :cljs (fn [preds] (fn [x] (boolean (-some (fn [pred] (pred x)) preds))))))

(defprotocol IExtropyCollection
  (-add       [_ x] [_ i x])
  (-add-first [_ x])
  (-add-last  [_ x])
  (-clear     [_  ])
  (-contains  [_ x])
  (-put       [  k v])
  (-put-in    [  ks v]))

(extend-protocol IExtropyCollection
  #?@(:clj
      [ArrayDeque
       (-add-first [this x] (doto this (.addFirst x)))
       (-add-last [this x] (doto this (.addLast x)))
       (-clear [this] (doto this (.clear)))
       (-contains [this x] (.contains this x))

       ArrayList
       (-add [this x] (doto this (.add x)))
       (-clear [this] (doto this (.clear)))
       (-contains [this x] (.contains this x))]

      :cljs
      [array
       (-add [this x] (if-not (.find this #(= % x)) (doto this (.push x)) this))
       (-add-first [this x] (if-not (.find this #(= % x)) (doto this (.unshift x)) this))
       (-add-last [this x] (if-not (.find this #(= % x)) (doto this (.push x)) this))]))

(deftype CacheEntry [udt v])

(defn -memoize
  ([f]
   (let [cache_ (volatile! {})]
     (fn
       ([]
        (if-let [ov (-get* @cache_ -sentinel)]
          (if (identical? ov -sentinel) nil ov)
          (if-some [v (f)]
            (do (vswap! cache_ assoc -sentinel v) v)
            (do (vswap! cache_ assoc -sentinel -sentinel) nil))))
       ([x]
        (let [xs [x]]
          (if-let [ov (-get* @cache_ xs)]
            (if (identical? ov -sentinel) nil ov)
            (if-some [v (f x)]
              (do (vswap! cache_ assoc xs v) v)
              (do (vswap! cache_ assoc xs -sentinel) nil)))))
       ([x1 x2]
        (let [xs [x1 x2]]
          (if-let [ov (-get* @cache_ xs)]
            (if (identical? ov -sentinel) nil ov)
            (if-some [v (f x1 x2)]
              (do (vswap! cache_ assoc xs v) v)
              (do (vswap! cache_ assoc xs -sentinel) nil)))))
       ([x1 x2 x3]
        (let [xs [x1 x2 x3]]
          (if-let [ov (-get* @cache_ xs)]
            (if (identical? ov -sentinel) nil ov)
            (if-some [v (f x1 x2 x3)]
              (do (vswap! cache_ assoc xs v) v)
              (do (vswap! cache_ assoc xs -sentinel) nil)))))
       ([x1 x2 x3 & more]
        (let [xs [x1 x2 x3 more]]
          (if-let [ov (-get* @cache_ xs)]
            (if (identical? ov -sentinel) nil ov)
            (if-some [v (apply f x1 x2 x3 more)]
              (do (vswap! cache_ assoc xs v) v)
              (do (vswap! cache_ assoc xs -sentinel) nil))))))))
  ([ttl-ms f]
   (assert (pos? ttl-ms))
   (let [cache_ (volatile! {})]
     (fn [& args]
       (let [instant (-now-udt)]
         (if-let [?e (-get* cache_ args)]
           (if (> (- instant (.-udt ^CacheEntry ?e)) ttl-ms)
             (let [v (apply f args)]
               (vswap! cache_ assoc args (CacheEntry. instant v))
               v)
             (.-v ^CacheEntry ?e))
           (let [v (apply f args)]
             (vswap! cache_ assoc args (CacheEntry. instant v))
             v)))))))

(defmacro -compose-set-intersection [n]
  (let [ss (repeatedly n (partial gensym "s__"))
        x  (gensym "x__")
        f (gensym "intersection_")
        body (map
              (fn [i] `([~@(take i ss)]
                       (let [sorted# (-sort-by -count ~(vec (take i ss)))]
                         (if-not (= [~@(take i ss)] sorted#)
                           (apply ~f sorted#)
                           (let [s1# ~(first ss)]
                             (-loop [~x s1# :let [acc# s1#]]
                               (if (and ~@(map (fn [set] `(contains? ~set ~x)) (next (take i ss))))
                                 (recur acc#)
                                 (recur (disj acc# ~x)))
                               acc#))))))
              (range 2 (inc n)))]
    `(fn ~f
       ([] #{})
       ([~(first ss)] ~(first ss))
       ~@body)))

(def -intersection (-compose-set-intersection 8))

(defmacro -compose-vector-intersection [n]
  (let [xs (repeatedly n (partial gensym "xs__"))
        x  (gensym "x__")
        f (gensym "intersection_")
        body (map
              (fn [i] `([~@(take i xs)]
                       (let [sorted# (-sort-by -count ~(vec (take i xs)))]
                         (if-not (= [~@(take i xs)] sorted#)
                           (apply ~f sorted#)
                           (let [xs1# ~(first xs)]
                             (-loop [~x xs1# :let [acc# (transient [])]]
                               (if (and ~@(map (fn [coll] `(-some= ~x ~coll)) (next (take i xs))))
                                 (recur (conj! acc# ~x))
                                 (recur acc#))
                               (-not-empty (persistent! acc#))))))))
              (range 2 (inc n)))]
    `(fn ~f
       ([] #{})
       ([~(first xs)] ~(first xs))
       ~@body)))

(def -vector-intersection (-compose-vector-intersection 8))

(defmacro -compose-vector-diffrence [n]
  (let [xs (repeatedly n (partial gensym "xs__"))
        x  (gensym "x__")
        f (gensym "diffrence_")
        body (map
              (fn [i] `([~@(take i xs)]
                       (-loop [~x ~(first xs) :let [acc# (transient [])]]
                         (if-not (or ~@(map (fn [coll] `(-some= ~x ~coll)) (next (take i xs))))
                           (recur (conj! acc# ~x))
                           (recur acc#))
                         (persistent! acc#))))
              (range 2 (inc n)))]
    `(fn ~f
       ([] #{})
       ([~(first xs)] ~(first xs))
       ~@body)))

(def -vector-diffrence (-compose-vector-diffrence 8))

(defmacro -compose-map-intersection [n]
  (let [ss (repeatedly n (partial gensym "s__"))
        me (gensym "me__")
        k (gensym "k__")
        v (gensym "v__")
        f (gensym "intersection_")
        body (map
              (fn [i] `([~@(take i ss)]
                       (let [sorted# (-sort-by -count ~(vec (take i ss)))]
                         (if-not (= [~@(take i ss)] sorted#)
                           (apply ~f sorted#)
                           (let [s1# ~(first ss)]
                             (-loop [~me s1# :let [acc# s1#]]
                               (let [~k (-k* ~me)
                                     ~v (-v* ~me)]
                                 (if (and ~@(map (fn [m] `(and (contains? ~m ~k) (= ~v (-get* ~m ~k)))) (next (take i ss))))
                                   (recur acc#)
                                   (recur (dissoc acc# ~k))))
                               acc#))))))
              (range 2 (inc n)))]
    `(fn ~f
       ([] #{})
       ([~(first ss)] ~(first ss))
       ~@body)))

(def -map-intersection (-compose-map-intersection 8))

(defmacro -compose-union [n]
  (let [ss (repeatedly n (partial gensym "s__"))
        xs  (repeatedly n (partial gensym "x__"))
        f (gensym "union_")
        body (map
               (fn [i] `([~@(take i ss)]
                        (let [sorted# (-sort-by -count -rcompare ~(vec (take i ss)))]
                          (if-not (= [~@(take i ss)] sorted#)
                            (apply ~f sorted#)
                            (-loop-all [~@(interleave (take (dec i) xs) (take (dec i) (next ss))) :let [acc# (transient ~(first ss))]]
                                       (recur (-> acc# ~@(map (fn [x] `(conj! ~x)) (take (dec i) xs))))
                              (persistent! acc#))))))
               (range 2 (inc n)))]
    `(fn ~f
       ([] #{})
       ([~(first ss)] ~(first ss))
       ~@body)))

(defn -intersect? [xs ys]
  (-loop [x xs :let [acc false]]
    (if (-some (fn [y] (= y x)) ys)
      true
      (recur acc))
    acc))

(def -union (-compose-union 8))

(defmacro -compose-cartesian-product-function
  ([xs acc]
   `(persistent! (-compose-cartesian-product-function ~xs (-ensure-transient ~acc) [])))
  ([[xs & more] acc stack]
   (let [x (gensym "x__")]
     (if (or (seq more) (some? xs))
       `(-loop [~x ~xs :let [acc# ~acc]]
          (recur (-compose-cartesian-product-function ~more acc# ~(conj stack x)))
          acc#)
       `(conj! ~acc [~@stack])))))

(defmacro -compose-cartesian-product [n]
  (let [xss (gensym "xss__")
        xs  (repeatedly n (partial gensym "xs__"))
        acc (gensym "acc__")
        f  (gensym "cartesian-product__")
        body (mapcat (fn [i] `[~i (let [~@(mapcat (fn [j] `[~(nth xs j) (nth ~xss ~j)]) (range 0 i))]
                                   (-compose-cartesian-product-function ~(take i xs) ~acc))]) (range 2 (inc n)))]
    `(fn ~f
       ([~xss] (~f (transient []) ~xss))
       ([~acc ~xss]
        (case (count ~xss)
          0 (persistent! ~acc)
          1 (-first ~xss)
          ~@body)))))

(def -cartesian-product (-compose-cartesian-product 8))

(defmacro -ms*
  ([n] (-ms* n :ms))
  ([n k]
   `(case ~k
     (:ms)
     ~n
     (:sec :secs :s)
     (* ~n 1000)
     (:min :mins :m)
     (* ~n 1000 60)
     (:hour :hours :h)
     (* ~n 1000 60 60)
     (:day :days :d)
     (* ~n 1000 60 60 24)
     (:week :weeks :w)
     (* ~n 1000 60 60 24 7)
     (:month :months)
     (* ~n 1000 60 60 24 365)
     (:year :years :y))))

(defn -ms
  ([n] `(-ms* ~n))
  ([n k]
   (-ms* n k)))

(def -str-builder
  #?(:clj
     (fn
       (^StringBuilder [ ] (StringBuilder.))
       (^StringBuilder [s] (StringBuilder. ^String s)))

     :cljs
     (fn
       ([ ] (goog.string.StringBuffer.))
       ([s] (goog.string.StringBuffer. s)))))

(defn -sb-append
  (^StringBuilder [^StringBuilder sb s] (if s (doto sb (.append s)) sb))
  (^StringBuilder [^StringBuilder sb s & more]
   (.append sb s)
   (-reduce (fn [^StringBuilder acc ^String x] (if x (doto acc (.append x)) acc)) sb more)))

(defn -str-contains? [s x]
  #?(:clj (.contains ^String s ^String x)
     :cljs (not= -1 (.indexOf s x))))

(defn -str-starts-with? [s x]
  #?(:clj (.startsWith ^String s ^String x)
     :cljs (zero? (.indexOf s x))))

(defn -str-repeat [n s]
  #?(:cljs
     (loop [i 0 acc (-str-builder)]
       (if (< i n)
         (recur (inc i) (-sb-append acc s))
         (str acc)))))

#?(:cljs
   (defn -key->prop [k]
     (let [it (-iter (name k))]
       (loop [acc (-str-builder)]
         (if (.hasNext it)
           (let [ch (.next it)]
             (if (= "-" ch)
               (recur (-sb-append acc (.toUpperCase (.next it))))
               (recur (-sb-append acc ch))))
           (str acc))))))

#?(:cljs
   (defn -prop->key [k]
     (let [it (-iter k)]
       (loop [acc (-str-builder)]
         (if (.hasNext it)
           (let [ch (.next it)]
             (if (= ch (.toUpperCase ch))
               (recur (-sb-append acc "-" (.toLowerCase ch)))
               (recur (-sb-append acc ch))))
           (str acc))))))

#?(:cljs
   (defn ->js-props [m]
     (-loop [me m :let [acc (transient {})]]
       (let [k (-k* me)
             v (-v* me)]
         (recur (-assoc!* acc (-key->prop k) (cond (keyword? v) (name v) :else v))))
       (persistent! acc))))

(defn -str-ends-with? [s x]
  #?(:clj (.endsWith ^String s ^String x)
     :cljs (let [sl (.-length s)
                 xl (.-length x)]
             (not= -1 (.lastIndexOf ^String s x (- sl xl))))))

(defn -str-join [sep xs]
  (if-not (= "" sep)
    (let [it (-iter xs)]
      (loop [acc (-str-builder)]
        (if (.hasNext it)
          (if-let [s (.next it)]
            (recur (-sb-append acc s (when (.hasNext it) sep)))
            (recur acc))
          (str acc))))
    (str (-reduce -sb-append (-str-builder) xs))))

(defn -str-join-once [sep xs]
  (let [it1 (-iter xs)]
    (loop [acc (-str-builder (.next it1)) acc-ends-with-sep? false]
      (if (.hasNext it1)
        (if-let [s (.next it1)]
          (let [starts-with-sep? (-str-starts-with? s sep)
                ends-with-sep? (-str-ends-with? s sep)]
            (if acc-ends-with-sep?
              (if starts-with-sep?
                (recur (-sb-append acc (.substring ^String s 1)) ends-with-sep?)
                (recur (-sb-append acc s) ends-with-sep?))
              (if starts-with-sep?
                (recur (-sb-append acc (.substring ^String s 1)) ends-with-sep?)
                (recur (-sb-append acc sep s) ends-with-sep?))))
          (recur acc acc-ends-with-sep?))
        (str acc)))))

(defn -path [& parts]
  (-str-join-once "/" parts))

#?(:cljs
   (defn -random-bytes [size]
     (let [seed (js/Uint8Array. size)]
       (.getRandomValues -crypto seed))))

#?(:cljs
   (let [alphabet (-mapv str "_-0123456789abcdefghijklmnopqrstuvwxyzABCDEFGHIJKLMNOPQRSTUVWXYZ")]
     (defn -nano-id
       ([] (-nano-id 21))
       ([size]
        (let [bytes (-random-bytes size)]
          (loop [i 0 id (-str-builder)]
            (if (< i size)
              (recur (inc i) (-sb-append id (alphabet (bit-and 0x3f (aget bytes i)))))
              (str id))))))))

;; https://github.com/ptaoussanis/encore/blob/master/src/taoensso/encore.cljc#L531
(defmacro -catching
  ([expr                     ] `(-catching ~expr ~'_ nil))
  ([expr err catch]
   `(-if-clj
     (try ~expr (catch Exception ~err ~catch))
     (try ~expr (catch :default  ~err ~catch))))
  ([expr err catch finally]
   `(-if-clj
     (try ~expr (catch Exception ~err ~catch) (finally ~finally))
     (try ~expr (catch :default  ~err ~catch) (finally ~finally)))))

;; https://github.com/ptaoussanis/encore/blob/master/src/taoensso/encore.cljc#L4173
(defmacro -catch-errors [& body]
  `(-catching [(do ~@body) nil] e# [nil e#]))

;; https://github.com/ptaoussanis/encore/blob/master/src/taoensso/encore.cljc#L744
(defn -as-?long [x]
  (cond (number? x) (long x)
        (string? x)
        #?(:clj
           (try (Long/parseLong x)
                (catch NumberFormatException _
                  (try (long (Float/parseFloat x))
                       (catch NumberFormatException _ nil))))
           :cljs
           (let [x (js/parseInt x 10)] (when-not (js/isNaN x) x)))))

;; https://github.com/ptaoussanis/encore/blob/master/src/taoensso/encore.cljc#L754
(defn -as-?double [x]
  (cond (number? x) (double x)
        (string? x)
        #?(:clj
           (try (Double/parseDouble x)
                (catch NumberFormatException _ nil))
           :cljs
           (let [x (js/parseFloat x)] (when-not (js/isNaN x) x)))))

(defn -round
  (^double [^double x]
   #?(:clj (double (Math/round x))
      :cljs (js/Math.round x)))
  (^double [^double x ^long nplaces]
   (if (< 1 nplaces)
     (let [modifier (#?(:clj  Math/pow :cljs js/Math.pow) 10.0 (double nplaces))
           x'       (* x modifier)
           rounded  #?(:clj  (Math/round x')
                       :cljs (js/Math.round x'))]
       (/ rounded modifier))
     (-round x))))

(defn -round2 ^double [^double x]
  #?(:clj  (/ (Math/round    (* x 100.0)) 100.0)
     :cljs (/ (js/Math.round (* x 100.0)) 100.0)))

(defmacro -time-ms
  [& body] `(let [t0# (-now-udt)] ~@body (- (-now-udt*) t0#)))

(defmacro -time-ns
  [& body] `(let [t0# (-now-nano)] ~@body (- (-now-nano*) t0#)))

;; https://github.com/ptaoussanis/encore/blob/master/src/taoensso/encore.cljc#L3259
(defmacro -qb
  ([nlaps form & more] (mapv (fn [form] `(-qb ~nlaps ~form)) (cons form more)))
  ([nlaps form]
   `(let [nlaps# ~nlaps
          [nsets# nlaps#] (if (vector? nlaps#) nlaps# [6 nlaps#])]
      (-round2
       (/ (double
           (-reduce min
                   (for [_# (range nsets#)]
                     (-time-ns (dotimes [_# nlaps#] (do ~form))))))
          1e6)))))

#?(:cljs
   (defn -wrap-promise [t p]
     (-> p (.then (fn [ok] (t (fn [] ok)))) (.catch (fn [err] (t (fn [] (throw err))))))
     t))

(defmacro -wrap-promise-> [t p & body]
  `(let [t# ~t]
     (-> ~p (.then (fn [ok#] (t# (fn [] ok#)))) (.catch (fn [err#] (t# (fn [] (throw err#))))))
     (-> t# ~@body)))

(defn -format
  ^String [^String fmt & args]
  #?(:clj  (String/format fmt (to-array args))
     :cljs (apply gstr/format fmt args)))

(defmacro -do-true [& body]
  `(do ~@body true))

(defn -do-task [t]
  (t (constantly nil) (constantly nil)))

(defn -prn-task [t]
  (t #(prn [:ok %]) #(prn [:err %])))

(defn -tap-task [t]
  (t #(tap> [:ok %]) #(tap> [:err %])))

(defn -add-ns [ns m]
  (persistent!
    (-reduce-kv
      (fn [acc k v] (-assoc!* acc (keyword ns k) v))
      (transient {})
      m)))

(defn -remove-ns [m]
  (persistent!
    (-reduce-kv
      (fn [acc k v] (-assoc!* acc (keyword (name k)) v))
      (transient {})
      m)))

(defn -error? [x]
  #?(:clj  (instance? Throwable x)
     :cljs (instance? js/Error x)))
