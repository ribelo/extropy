(ns ribelo.extropy
  (:refer-clojure :exclude [-first -assoc -assoc! -reduce -remove])
  #?(:cljs (:require-macros [ribelo.extropy :refer [-if-clj -if-cljs -iter* -get* -loop -loop-all -run! -k* -v* -reduce -compose-apply
                                                    -compose-preds -compose-set-intersection -compose-map-intersection
                                                    -compose-vector-intersection -compose-cartesian-product-function
                                                    -compose-union -compose-cartesian-product]]))
  #?(:clj
     (:import
      (java.util Map HashMap HashSet Iterator ArrayDeque ArrayList)
      (java.util.concurrent CountDownLatch))))

#?(:clj (set! *warn-on-reflection* true))

(def -sentinel #?(:clj (Object.) :cljs (js/Object.)))

(defmacro -if-clj  [then & [else]] (if (:ns &env) else then))
(defmacro -if-cljs [then & [else]] (if (:ns &env) then else))

(defn -udt []
  #?(:clj (System/currentTimeMillis) :cljs (.getTime (js/Date.))))

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
  (if (-iter? x) x (-iter x)))

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
         (if (and ~@(map (fn [it] `(.hasNext ~it)) iter-syms))
           (let ~(vec (mapcat (fn [bind it]
                                [bind `(.next ~it)])
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
  (-loop [x xs :let [acc nil]]
    (recur (f x))
    nil))

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
   `(when ~m
      (-if-clj
        (.valAt ~(with-meta m {:tag 'clojure.lang.ILookup}) ~k)
        (~m ~k))))
  ([m k not-found]
   `(when ~m
      (-if-clj
        (.valAt ~(with-meta m {:tag 'clojure.lang.ILookup}) ~k ~not-found)
        (if (and ~m (.has ~m ~k)) (~m ~k) ~not-found)))))

(defn -get
  ([m k]
   (-get* m k))
  ([m k not-found]
   (-get* m k not-found)))

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

(defn -first-key [xs]
  (first (keys xs)))

(defn -first-val [xs]
  (first (vals xs)))

(defmacro -compose-apply [n]
  (let [args (gensym "args__")
        xs (repeatedly n (partial gensym "x__"))
        f (gensym "fn__")
        cases (fn [xs] (mapcat (fn [i] `[~i (~f ~@xs ~@(map (fn [j] `(nth ~args ~j)) (range i)))]) (range 1 (inc n))))
        body (map (fn [i] `([~f ~@(take i xs) ~args]
                           (case (count ~args)
                             0 (~f ~@(take i xs))
                             ~@(cases (take i xs))
                             (apply ~f ~@(take i xs) ~args))))
                  (range n))
        ]
    `(fn  ~@body)))


(def ^{:arglists '([[f args]])} -apply (-compose-apply 16))

(declare -assoc!)

(defn -maybe-transient [xs]
  (if #?(:clj  (instance? clojure.lang.IEditableCollection xs)
         :cljs (satisfies? cljs.core.IEditableCollection xs))
    (.asTransient ^clojure.lang.IEditableCollection xs)
    xs))

(defn -transient? [xs]
  #?(:clj  (instance? clojure.lang.ITransientCollection xs)
     :cljs (satisfies? cljs.core.ITransientCollection xs)))

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
    (if #?(:clj (instance? Iterable xs) :cljs (iterable? xs))
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

(defn -mutable-map
  ([] (-mutable-map {}))
  ([^clojure.lang.IPersistentMap m]
   (volatile! (-recursive-transient m))))

(defn -mutable-vector
  ([] (-mutable-vector []))
  ([^clojure.lang.IPersistentVector xs]
   (volatile! (-recursive-transient xs))))

(defn -mutable-set
  ([] (-mutable-set []))
  ([^clojure.lang.IPersistentSet xs]
   (volatile! (-recursive-transient xs))))

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
     (-apply -comp fs)
     (fn [x] (-> x f8 f7 f6 f5 f4 f3 f2 f1)))))

(defn -assoc
  ([m k v]
   (if m
     #?(:clj  (.assoc ^clojure.lang.Associative m k v)
        :cljs (cljs.core/-assoc m k v))
     {k v}))
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
   (let [m' (or m (transient {}))]
     #?(:clj  (.assoc ^clojure.lang.ITransientAssociative m' k v)
        :cljs (cljs.core/-assoc! m' k v))))
  ([m k1 v1 k2 v2]
   (-> (-assoc! m k1 v1) (-assoc! k2 v2)))
  ([m k1 v1 k2 v2 k3 v3]
   (-> (-assoc! m k1 v1) (-assoc! k2 v2) (-assoc! k3 v3)))
  ([m k1 v1 k2 v2 k3 v3 k4 v4]
   (-> (-assoc! m k1 v1) (-assoc! k2 v2) (-assoc! k3 v3) (-assoc! k4 v4)))
  ([m k1 v1 k2 v2 k3 v3 k4 v4 & kvs]
   (-reduce-kvs -assoc! (-> (-assoc! m k1 v1) (-assoc! k2 v2) (-assoc! k3 v3) (-assoc! k4 v4)) kvs)))

(defn -assoc-in [m [k & ks] v]
  (if ks
    (assoc m k (-assoc-in (-get* m k) ks v))
    (assoc m k v)))

(defn -assoc-in! [m [k & ks] v]
  (if ks
    (-assoc! m k (-assoc-in! (-get* m k) ks v))
    (-assoc! m k v)))

(defn -assoc-some
  ([m k v]
   (if (nil? v) (or m {}) (assoc m k v)))
  ([m k v & kvs]
   (persistent!
     (-reduce-kvs
       (fn [acc k v] (if (nil? v) acc (assoc! acc k v)))
       (transient (-assoc-some m k v))
       kvs))))

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
   (-assoc m k (-apply f (-get* m k) x y z more))))

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
   (-assoc m k (-apply f (-get* m k) x y z more))))

(defn -update-in
  ([m ks f & args]
   (let [up (fn up [m ks f args]
              (let [[k & ks] ks]
                (if ks
                  (-assoc m k (up (-get* m k) ks f args))
                  (-assoc m k (-apply f (-get* m k) args)))))]
     (up m ks f args))))

(defn -update-in!
  ([m ks f & args]
   (let [up (fn up [m ks f args]
              (let [[k & ks] ks]
                (if ks
                  (-assoc! m k (up (-get* m k (transient {})) ks f args))
                  (-assoc! m k (-apply f (-get* m k (transient {})) args)))))]
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

(defn -merge
  ([] {})
  ([m] m)
  ([m1 m2]
   (if m1 (conj m1 m2) m2))
  ([m1 m2 m3]
   (-> (conj (or m1 {}) m1) (conj m2) (conj m3)))
  ([m1 m2 m3 m4]
   (-> (conj (or m1 {}) m1) (conj m2) (conj m3) (conj m4)))
  ([m1 m2 m3 m4 & maps]
   (-reduce conj (-> (conj (or m1 {}) m1) (conj m2) (conj m3) (conj m4)) maps)))

(defn -rcompare
  {:inline (fn [x y] (. clojure.lang.Util compare ~y ~x))}
  [x y]
  (compare y x))

(defn -not-empty [xs]
  (when-not (zero? #?(:clj (count ^java.util.Collection xs) :cljs (-count xs))) xs))

#?(:clj
   (defn -transduce
     ([xform f xs] (-transduce xform f (f) xs))
     ([xform f init xs]
      (let [f (xform f)]
        (-reduce f init xs))))

   :cljs
   (def -transduce transduce))

#?(:clj
   (defn -into
     ([] [])
     ([to] to)
     ([to from]
      (if #?(:clj  (instance? clojure.lang.IEditableCollection to)
             :cljs (instance? IEditableCollection to))
        (with-meta (persistent! (-reduce conj! (transient to) from)) (meta to))
        (-reduce conj to from)))
     ([to xform from]
      (if #?(:clj  (instance? clojure.lang.IEditableCollection to)
             :cljs (instance? IEditableCollection to))
        (with-meta (persistent! (-transduce xform conj! (transient to) from)) (meta to))
        (-transduce xform conj to from))))

   :cljs
   (def -into into))

#?(:clj
   (defn -into!
     ([] [])
     ([to] to)
     ([to from]
      (-reduce conj! (-ensure-transient to) from))
     ([to xform from]
      (-transduce xform conj! (-ensure-transient to) from)))

   :cljs
   (def -into! into))

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
   (-loop [x1 xs1 x2 xs2 x3 xs3 x4 xs4 :let [acc (transient [])]]
     (recur (conj! acc (-apply f x1 x2 x3 x4 colls)))
     (persistent! acc))))

(defn -filterv [pred xs]
  (-loop [x xs :let [acc (transient [])]]
    (if (pred x)
      (recur (conj! acc x))
      (recur acc))
    (persistent! acc)))

(defn -remove [pred xs]
  (-filterv (complement pred) xs))

(defn -keep [pred xs]
  (-loop [x xs :let [acc (transient [])]]
    (if-let [v (pred x)]
      (recur (conj! acc v))
      (recur acc))
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

(defn -sort
  ([xs] (-sort compare xs))
  ([comp xs]
   (let [sorted (doto (-array-list xs) #?(:clj (java.util.Collections/sort comp) :cljs (.sort comp)))]
     (-loop [x sorted :let [acc (transient [])]]
       (recur (conj! acc x))
       (persistent! acc)))))

(defn -sort-by
  ([kfn xs] (-sort-by kfn compare xs))
  ([kfn comp xs]
   (-sort (fn [x y] (.compare ^java.util.Comparator comp (kfn x) (kfn y))) xs)))

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

(defprotocol IExinCollection
  (-add       [_ x] [_ i x])
  (-add-first [_ x])
  (-add-last  [_ x])
  (-clear     [_  ])
  (-contains  [_ x])
  (-put       [  k v])
  (-put-in    [  ks v]))

(extend-protocol IExinCollection
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
            (if-some [v (-apply f x1 x2 x3 more)]
              (do (vswap! cache_ assoc xs v) v)
              (do (vswap! cache_ assoc xs -sentinel) nil))))))))
  ([ttl-ms f]
   (assert (pos? ttl-ms))
   (let [cache_ (volatile! {})]
     (fn [& args]
       (let [instant (-udt)]
         (if-let [?e (-get* cache_ args)]
           (if (> (- instant (.-udt ^CacheEntry ?e)) ttl-ms)
             (let [v (-apply f args)]
               (vswap! cache_ assoc args (CacheEntry. instant v))
               v)
             (.-v ^CacheEntry ?e))
           (let [v (-apply f args)]
             (vswap! cache_ assoc args (CacheEntry. instant v))
             v)))))))

(defmacro -compose-set-intersection [n]
  (let [ss (repeatedly n (partial gensym "s__"))
        x  (gensym "x__")
        f (gensym "intersection_")
        body (map
              (fn [i] `([~@(take i ss)]
                       (let [sorted# (-sort-by count ~(vec (take i ss)))]
                         (if-not (= [~@(take i ss)] sorted#)
                           (-apply ~f sorted#)
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
                       (let [sorted# (-sort-by count ~(vec (take i xs)))]
                         (if-not (= [~@(take i xs)] sorted#)
                           (-apply ~f sorted#)
                           (let [xs1# ~(first xs)]
                             (-loop [~x xs1# :let [acc# (transient [])]]
                               (if (and ~@(map (fn [coll] `(-some (fn [y#] (= y# ~x)) ~coll)) (next (take i xs))))
                                 (recur (conj! acc# ~x))
                                 (recur acc#))
                               (-not-empty (persistent! acc#))))))))
              (range 2 (inc n)))]
    `(fn ~f
       ([] #{})
       ([~(first xs)] ~(first xs))
       ~@body)))

(def -vector-intersection (-compose-vector-intersection 8))

(defmacro -compose-map-intersection [n]
  (let [ss (repeatedly n (partial gensym "s__"))
        me (gensym "me__")
        k (gensym "k__")
        v (gensym "v__")
        f (gensym "intersection_")
        body (map
              (fn [i] `([~@(take i ss)]
                       (let [sorted# (-sort-by count ~(vec (take i ss)))]
                         (if-not (= [~@(take i ss)] sorted#)
                           (-apply ~f sorted#)
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
                        (let [sorted# (-sort-by count -rcompare ~(vec (take i ss)))]
                          (if-not (= [~@(take i ss)] sorted#)
                            (-apply ~f sorted#)
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
