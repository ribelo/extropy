(ns ribelo.extropy
  (:refer-clojure :exclude [iter run! reduce reduce-kv select-keys comp map filter remove keep group-by
                            every? some sort sort-by every-pred memoize format remove-ns])
  #?(:cljs (:require-macros [ribelo.extropy :refer [if-clj if-cljs loop-it time-ms time-ns qb do-true -compose-preds]]))
  #?(:clj
     (:require
      [clojure.core :as core])
     :cljs
     (:require
      [cljs.core :as core]
      [goog.string :as gstr]))
  #?(:clj
     (:import
      (java.util Map HashMap HashSet Iterator ArrayDeque ArrayList)
      (java.lang Iterable))))

#?(:cljs
   (if (exists? js/window)
     (def crypto js/crypto)
     (def crypto (js/require "crypto"))))

#?(:clj (set! *warn-on-reflection* true))

(def sentinel #?(:clj (Object.) :cljs (js/Object.)))

(defmacro if-clj  [then & [else]] (if (:ns &env) else then))
(defmacro if-cljs [then & [else]] (if (:ns &env) then else))

(def clj?  (if-clj  true false))
(def cljs? (if-cljs true false))

(defn now-dt []
  #?(:clj  (java.util.Date.)
     :cljs (js/Date.)))

(defn now-udt []
  #?(:clj  (System/currentTimeMillis)
     :cljs (.getTime (js/Date.))))

#?(:clj
   (defn now-nano ^long [] (System/nanoTime))

   :cljs
   (def now-nano
     (if (and (exists? js/window) (aget js/window "performance"))
       (fn [] (* 1000000 (js/performance.now)))
       (fn [] (* 1000000 (now-udt))))))

(defn iter ^Iterator [^Iterable xs]
  #?(:clj  (.iterator xs)
     :cljs (cljs.core/iter xs)))

(defn iter? [x]
  #?(:clj
     (instance? Iterator x)
     :cljs
     (or (instance? cljs.core/SeqIter x)
         (instance? cljs.core/RangedIterator x)
         (instance? cljs.core/RangeIterator x)
         (instance? cljs.core/IndexedSeqIterator x)
         (instance? cljs.core/PersistentArrayMapIterator x)
         (instance? cljs.core/ArrayNodeIterator x)
         (instance? cljs.core/MultiIterator x)
         (instance? cljs.core/NodeIterator x))))

(defn ensure-iter ^Iterator [x]
  (if (iter? x) x (iter x)))

(defn array-list
  (^ArrayList [] (array-list []))
  (^ArrayList [xs]
   #?(:clj  (ArrayList. ^java.util.Collection xs)
      :cljs (to-array xs))))

(defn native-map
  (^HashSet [] (native-map {}))
  (^HashSet [^Map xs]
   #?(:clj  (HashMap. xs)
      :cljs (reduce-kv (fn [acc k v] (doto acc (aset k v))) (js-obj) xs))))

(defn native-set
  (^HashSet [] (native-set []))
  (^HashSet [^java.util.Collection xs]
   #?(:clj  (HashSet. xs)
      :cljs (js/Set. xs))))

;; https://github.com/rauhs/clj-bench/blob/master/src/clj_bench/loop_it.clj#L14
(defn multi-iter
  (^Iterator [colls]
   ;; Copied from clojure.lang.MultiIterator
   (let [iters ^{:tag "[Ljava.util.Iterator;"} (into-array (mapv ensure-iter colls))]
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
   #?(:clj (clojure.lang.TransformerIterator/createMulti xf (mapv ensure-iter colls))
      :cljs (.create cljs.core/TransformerIterator xf (mapv ensure-iter colls)))))

;; https://github.com/rauhs/clj-bench/blob/master/src/clj_bench/loop_it.clj
(defmacro loop-it
  [bindings body & finish]
  {:pre [(vector? bindings) (even? (count bindings))]}
  (let [has-let? (= :let (last (butlast bindings)))
        it-binds (if has-let? (butlast (butlast bindings)) bindings)
        sym-binds (take-nth 2 it-binds)
        colls (take-nth 2 (rest it-binds))
        iter-syms (repeatedly (count sym-binds) #(gensym "iter-"))]
    `(let ~(vec (mapcat (fn [it coll] [it `(ensure-iter ~coll)])
                        iter-syms colls))
       (loop ~(if has-let? (vec (last bindings)) [])
         (if (and ~@(core/map (fn [it] `(.hasNext ~(with-meta it {:tag 'java.util.Iterator}))) iter-syms))
           (let ~(vec (mapcat (fn [bind it]
                                [bind `(.next ~(with-meta it {:tag 'java.util.Iterator}))])
                              sym-binds iter-syms))
             ~body)
           ~(when finish `(do ~@finish)))))))

(defn run! [f xs]
  (loop-it [x xs]
    (when-not (reduced? (f x))
      (recur))))

(defn reduce
  ([f xs]
   (let [it (iter xs)]
     (if (.hasNext it)
       (loop-it [x it :let [acc (.next it)]]
         (let [acc' (f acc x)]
           (if (reduced? acc')
             @acc'
             (recur acc')))
         acc)
       (f))))
  ([f init xs]
   (loop-it [x xs :let [acc init]]
     (let [acc' (f acc x)]
       (if (reduced? acc')
         @acc'
         (recur acc')))
     acc)))

(defn reduce-kv
  ([f m]
   (let [it (iter m)]
     (if (.hasNext it)
       (loop-it [[k v] it :let [acc (.next it)]]
         (let [acc' (f acc k v)]
           (if (reduced? acc')
             @acc'
             (recur acc')))
         acc)
       (f))))
  ([f init m]
   (loop-it [[k v] m :let [acc init]]
     (let [acc' (f acc k v)]
       (if (reduced? acc')
         @acc'
         (recur acc')))
     acc)))

(defn reduce-kvs
  [rf init kvs]
  (loop-it [x kvs :let [acc init k nil v nil]]
    (if (and k v)
      (recur (rf acc k v) x nil)
      (cond
        (nil? k)
        (recur acc x nil)
        (nil? v)
        (recur (rf acc k x) nil nil)))
    acc))

(defn kw-identical? [x y]
  #?(:clj
     (identical? x y)
     :cljs
     (keyword-identical? x y)))

(defn first-key [xs]
  (first (keys xs)))

(defn first-val [xs]
  (first (vals xs)))

(defn assoc-some
  ([m k v]
   (if (nil? v) (or m {}) (assoc m k v)))
  ([m k v & kvs]
   (persistent!
    (reduce-kvs
     (fn [acc k v] (if (nil? v) acc (assoc! acc k v)))
     (transient (assoc-some m k v))
     kvs))))

(defn assoc-some!
  ([m k v]
   (if (nil? v) (or m (transient {})) (assoc! m k v)))
  ([m k v & kvs]
   (reduce-kvs
    (fn [acc k v] (if (nil? v) acc (assoc! acc k v)))
    (assoc-some! m k v)
    kvs)))

(defn select-keys [m xs]
  (loop-it [k xs :let [acc (transient {})]]
    (recur (assoc-some! acc k (get m k)))
    (persistent! acc)))

(defn transient? [xs]
  #?(:clj  (instance? clojure.lang.ITransientCollection xs)
     :cljs (satisfies? cljs.core.ITransientCollection xs)))

(defn ensure-persisten! [xs]
  (if (transient? xs) (persistent! xs) xs))

(defn ensure-transient ^clojure.lang.ITransientCollection [xs]
  (when (some? xs) (if (transient? xs) xs (transient xs))))

(defn deque
  (^ArrayDeque [] (deque []))
  (^ArrayDeque [^java.util.Collection xs]
   #?(:clj (java.util.ArrayDeque. xs) :cljs (to-array xs))))

(defprotocol IExtropyCollection
  (add       [_ x] [_ i x])
  (add-first [_ x])
  (add-last  [_ x])
  (clear     [_  ])
  (contains  [_ x])
  (put       [  k v])
  (put-in    [  ks v]))

(extend-protocol IExtropyCollection
  #?@(:clj
      [ArrayDeque
       (add-first [this x] (doto this (.addFirst x)))
       (add-last [this x] (doto this (.addLast x)))
       (clear [this] (doto this (.clear)))
       (contains [this x] (.contains this x))

       ArrayList
       (add [this x] (doto this (.add x)))
       (clear [this] (doto this (.clear)))
       (contains [this x] (.contains this x))]

      :cljs
      [array
       (add [this x] (if-not (.find this #(= % x)) (doto this (.push x)) this))
       (add-first [this x] (if-not (.find this #(= % x)) (doto this (.unshift x)) this))
       (add-last [this x] (if-not (.find this #(= % x)) (doto this (.push x)) this))]))

(defn comp
  ([] identity)
  ([f] f)
  ([f g] (fn [x] (f (g x))))
  ([f g h] (fn [x] (f (g (h x)))))
  ([f1 f2 f3 f4] (fn [x] (> x f4 f3 f2 f1)))
  ([f1 f2 f3 f4 f5] (fn [x] (> x f5 f4 f3 f2 f1)))
  ([f1 f2 f3 f4 f5 f6] (fn [x] (> x f6 f5 f4 f3 f2 f1)))
  ([f1 f2 f3 f4 f5 f6 f7] (fn [x] (> x f7 f6 f5 f4 f3 f2 f1)))
  ([f1 f2 f3 f4 f5 f6 f7 f8] (fn [x] (> x f8 f7 f6 f5 f4 f3 f2 f1)))
  ([f1 f2 f3 f4 f5 f6 f7 f8 & fs]
   (comp
    (apply comp fs)
    (fn [x] (> x f8 f7 f6 f5 f4 f3 f2 f1)))))

(defn dissoc-in
  ([m ks]
   (update-in m (pop ks) dissoc (peek ks))))

(defn conj-some
  ([xs x]
   (if-not (or (nil? x) (identical? x sentinel)) (conj xs x) xs))
  ([xs x & more]
   (if-not (or (nil? x) (identical? x sentinel))
     (recur (conj-some xs x) (first more) (next more))
     xs)))

(defn conj-some!
  ([xs x]
   (if-not (or (nil? x) (identical? x sentinel)) (conj! xs x) xs))
  ([xs x & more]
   (if-not (or (nil? x) (identical? x sentinel))
     (recur (conj-some! xs x) (first more) (next more))
     xs)))

(def conjv (fnil conj []))
(def conjv! (fnil conj! (transient [])))
(def conjs (fnil conj #{}))
(def conjs! (fnil conj! (transient #{})))

(defn into-all
  ([to from       ]
   (if-not (transient? to) (into to from) (reduce conj! to from)))
  ([to from & more]
   (persistent!
    (reduce (fn [acc in] (reduce conj! acc in))
      (ensure-transient to)
      (cons from more)))))

(defn rename-keys [m kmap]
  (loop-it [[k v] kmap :let [acc (transient m)]]
    (if-some [x (m k)]
      (recur (dissoc! (assoc! acc v x) k))
      acc)
    (persistent! acc)))

;; https://github.com/ptaoussanis/encore/blob/master/src/taoensso/encore.cljc#L2942
#?(:clj
   (defn rcompare
     {:inline (fn [x y] (. clojure.lang.Util compare ~y ~x))}
     [x y]
     (compare y x))

   :cljs
   (defn rcompare
     [x y]
     (compare y x)))

(defn map
  ([f xs]
   (if xs
     (loop-it [x xs :let [acc (transient [])]]
       (recur (conj! acc (f x)))
       (persistent! acc))
     []))
  ([f xs1 xs2]
   (if (and xs1 xs2)
     (loop-it [x1 xs1 x2 xs2  :let [acc (transient [])]]
       (recur (conj! acc (f x1 x2)))
       (persistent! acc))
     []))
  ([f xs1 xs2 xs3]
   (if (and xs1 xs2 xs3)
     (loop-it [x1 xs1 x2 xs2 x3 xs3 :let [acc (transient [])]]
       (recur (conj! acc (f x1 x2 x3)))
       (persistent! acc))
     []))
  ([f xs1 xs2 xs3 xs4]
   (if (and xs1 xs2 xs3 xs4)
     (loop-it [x1 xs1 x2 xs2 x3 xs3 x4 xs4 :let [acc (transient [])]]
       (recur (conj! acc (f x1 x2 x3 x4)))
       (persistent! acc))
     []))
  ([f xs1 xs2 xs3 xs4 & colls]
   (if (and xs1 xs2 xs3 xs4)
     (loop-it [xs (multi-iter (into [xs1 xs2 xs3 xs4] colls)) :let [acc (transient [])]]
       (recur (conj! acc (apply f xs)))
       (persistent! acc))
     [])))

(defn filter
  ([pred xs]
   (if xs
     (loop-it [x xs :let [acc (transient (empty xs))]]
       (if (pred x)
         (recur (conj! acc x))
         (recur acc))
       (persistent! acc))
     []))
  ([pred xs & colls]
   (if xs
     (loop-it [x (multi-iter (into [xs] colls)) :let [acc (transient (empty xs))]]
       (if (pred x)
         (recur (conj! acc x))
         (recur acc))
       (persistent! acc))
     [])))

(defn remove [pred xs]
  (filter (complement pred) xs))

(defn keep [pred xs]
  (if xs
    (loop-it [x xs :let [acc (transient [])]]
      (if-let [v (pred x)]
        (recur (conj! acc v))
        (recur acc))
      (persistent! acc))
    []))

(defn group-by [f data]
  (loop-it [x data :let [acc (transient {})]]
    (let [k (f x)]
      (recur (assoc! acc k (conjv (get acc k) x))))
    (persistent! acc)))

(defn every? [pred xs]
  (loop-it [x xs :let [acc true]]
    (if (pred x)
      (recur acc)
      false)
    acc))

(defn some [pred xs]
  (loop-it [x xs :let [acc false]]
    (if (pred x)
      true
      (recur acc))
    acc))

(defn some= [x xs]
  (some (fn [y] (= x y)) xs))

(defn intersect? [xs ys]
  (loop-it [x xs :let [acc false]]
    (if (some (fn [y] (= y x)) ys)
      true
      (recur acc))
    acc))

(defn sort
  ([xs] (sort compare xs))
  ([comp xs]
   (let [sorted (doto (array-list xs) #?(:clj (java.util.Collections/sort comp) :cljs (.sort comp)))]
     (loop-it [x sorted :let [acc (transient [])]]
       (recur (conj! acc x))
       (persistent! acc)))))

(defn filter-keys [pred m]
  (loop-it [[k _] m :let [acc (transient m)]]
    (if (pred k)
      (recur acc)
      (recur (dissoc! acc k)))
    (persistent! acc)))

(defn filter-vals [pred m]
  (loop-it [[k v] m :let [acc (transient m)]]
    (if (pred v)
      (recur acc)
      (recur (dissoc! acc k)))
    (persistent! acc)))

(defn sort-by
  ([kfn xs] (sort-by kfn compare xs))
  ([kfn comp xs]
   #?(:clj  (sort (fn [x y] (.compare ^java.util.Comparator comp (kfn x) (kfn y))) xs)
      :cljs (sort (fn [x y] (comp (kfn x) (kfn y))) xs))))

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

(def ^{:arglists '([[& preds]])} every-pred
  #?(:clj  (-compose-preds and 16 4)
     :cljs (fn [preds] (fn [x] (boolean (reduce (fn [_ pred] (or (pred x) (reduced false))) true preds))))))

(def ^{:arglists '([[& preds]])} some-pred
  #?(:clj  (-compose-preds or 16 4)
     :cljs (fn [preds] (fn [x] (boolean (some (fn [pred] (pred x)) preds))))))

(deftype CacheEntry [udt v])

(defn memoize
  ([f]
   (let [cache_ (volatile! {})]
     (fn
       ([]
        (if-some [ov (@cache_ sentinel)]
          (if (identical? ov sentinel) nil ov)
          (if-some [v (f)]
            (do (vswap! cache_ assoc sentinel v) v)
            (do (vswap! cache_ assoc sentinel sentinel) nil))))
       ([x]
        (let [xs [x]]
          (if-some [ov (@cache_ xs)]
            (if (identical? ov sentinel) nil ov)
            (if-some [v (f x)]
              (do (vswap! cache_ assoc xs v) v)
              (do (vswap! cache_ assoc xs sentinel) nil)))))
       ([x1 x2]
        (let [xs [x1 x2]]
          (if-some [ov (@cache_ xs)]
            (if (identical? ov sentinel) nil ov)
            (if-some [v (f x1 x2)]
              (do (vswap! cache_ assoc xs v) v)
              (do (vswap! cache_ assoc xs sentinel) nil)))))
       ([x1 x2 x3]
        (let [xs [x1 x2 x3]]
          (if-some [ov (@cache_ xs)]
            (if (identical? ov sentinel) nil ov)
            (if-some [v (f x1 x2 x3)]
              (do (vswap! cache_ assoc xs v) v)
              (do (vswap! cache_ assoc xs sentinel) nil)))))
       ([x1 x2 x3 & more]
        (let [xs [x1 x2 x3 more]]
          (if-some [ov (@cache_ xs)]
            (if (identical? ov sentinel) nil ov)
            (if-some [v (apply f x1 x2 x3 more)]
              (do (vswap! cache_ assoc xs v) v)
              (do (vswap! cache_ assoc xs sentinel) nil))))))))
  ([ttl-ms f]
   (assert (pos? ttl-ms))
   (let [cache_ (volatile! {})]
     (fn [& args]
       (let [instant (now-udt)]
         (if-some [?e (get cache_ args)]
           (if (> ( instant (.-udt ^CacheEntry ?e)) ttl-ms)
             (let [v (apply f args)]
               (vswap! cache_ assoc args (CacheEntry. instant v))
               v)
             (.-v ^CacheEntry ?e))
           (let [v (apply f args)]
             (vswap! cache_ assoc args (CacheEntry. instant v))
             v)))))))

(defn ms
  ([n] n)
  ([n k]
   (case k
     (:ms)
     n
     (:sec :secs :s)
     (* n 1000)
     (:min :mins :m)
     (* n 1000 60)
     (:hour :hours :h)
     (* n 1000 60 60)
     (:day :days :d)
     (* n 1000 60 60 24)
     (:week :weeks :w)
     (* n 1000 60 60 24 7)
     (:month :months)
     (* n 1000 60 60 24 365)
     (:year :years :y))))



(def str-builder
  #?(:clj
     (fn
       (^StringBuilder [ ] (StringBuilder.))
       (^StringBuilder [s] (StringBuilder. ^String s)))

     :cljs
     (fn
       ([ ] (goog.string.StringBuffer.))
       ([s] (goog.string.StringBuffer. s)))))

(defn sb-append
  (^StringBuilder [^StringBuilder sb s] (if s (doto sb (.append s)) sb))
  (^StringBuilder [^StringBuilder sb s & more]
   (.append sb s)
   (reduce (fn [^StringBuilder acc ^String x] (if x (doto acc (.append x)) acc)) sb more)))

(defn str-contains? [s x]
  #?(:clj (.contains ^String s ^String x)
     :cljs (not= -1 (.indexOf s x))))

(defn str-starts-with? [s x]
  #?(:clj (.startsWith ^String s ^String x)
     :cljs (zero? (.indexOf s x))))

(defn str-repeat [n s]
  (loop-it [i 0 acc (str-builder)]
    (if (< i n)
      (recur (inc i) (sb-append acc s))
      (str acc))))

#?(:cljs
   (defn key->prop [k]
     (let [it (iter (name k))]
       (loop-it [acc (str-builder)]
         (if (.hasNext it)
           (let [ch (.next it)]
             (if (= "-" ch)
               (recur (sb-append acc (.toUpperCase (.next it))))
               (recur (sb-append acc ch))))
           (str acc))))))

#?(:cljs
   (defn prop->key [k]
     (let [it (iter k)]
       (loop-it [acc (str-builder)]
         (if (.hasNext it)
           (let [ch (.next it)]
             (if (= ch (.toUpperCase ch))
               (recur (sb-append acc "-" (.toLowerCase ch)))
               (recur (sb-append acc ch))))
           (str acc))))))

#?(:cljs
   (defn >js-props [m]
     (loop-it [me m :let [acc (transient {})]]
       (let [k (k* me)
             v (v* me)]
         (recur (assoc!* acc (key->prop k) (cond (keyword? v) (name v) :else v))))
       (persistent! acc))))

(defn str-ends-with? [s x]
  #?(:clj (.endsWith ^String s ^String x)
     :cljs (let [sl (.-length s)
                 xl (.-length x)]
             (not= -1 (.lastIndexOf ^String s x ( sl xl))))))

(defn str-join [sep xs]
  (if-not (= "" sep)
    (let [it (iter xs)]
      (loop [acc (str-builder)]
        (if (.hasNext it)
          (if-some [s (.next it)]
            (recur (sb-append acc s (when (.hasNext it) sep)))
            (recur acc))
          (str acc))))
    (str (reduce sb-append (str-builder) xs))))

(defn str-join-once [sep xs]
  (let [it1 (iter xs)]
    (loop [acc (str-builder (.next it1)) acc-ends-with-sep? false]
      (if (.hasNext it1)
        (if-some [s (.next it1)]
          (let [starts-with-sep? (str-starts-with? s sep)
                ends-with-sep? (str-ends-with? s sep)]
            (if acc-ends-with-sep?
              (if starts-with-sep?
                (recur (sb-append acc (.substring ^String s 1)) ends-with-sep?)
                (recur (sb-append acc s) ends-with-sep?))
              (if starts-with-sep?
                (recur (sb-append acc (.substring ^String s 1)) ends-with-sep?)
                (recur (sb-append acc sep s) ends-with-sep?))))
          (recur acc acc-ends-with-sep?))
        (str acc)))))

(defn path [& parts]
  (str-join-once "/" parts))

#?(:cljs
   (defn random-bytes [size]
     (let [seed (js/Uint8Array. size)]
       (.getRandomValues -crypto seed))))

#?(:cljs
   (let [alphabet (map str "_-0123456789abcdefghijklmnopqrstuvwxyzABCDEFGHIJKLMNOPQRSTUVWXYZ")]
     (defn nano-id
       ([] (nano-id 21))
       ([size]
        (let [bytes (random-bytes size)]
          (loop-it [i 0 id (str-builder)]
            (if (< i size)
              (recur (inc i) (sb-append id (alphabet (bit-and 0x3f (aget bytes i)))))
              (str id))))))))

;; https://github.com/ptaoussanis/encore/blob/master/src/taoensso/encore.cljc#L744
(defn as-?long [x]
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
(defn as-?double [x]
  (cond (number? x) (double x)
        (string? x)
        #?(:clj
           (try (Double/parseDouble x)
                (catch NumberFormatException _ nil))
           :cljs
           (let [x (js/parseFloat x)] (when-not (js/isNaN x) x)))))

(defn round
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
     (round x))))

(defn round2 ^double [^double x]
  #?(:clj  (/ (Math/round    (* x 100.0)) 100.0)
     :cljs (/ (js/Math.round (* x 100.0)) 100.0)))

(defmacro time-ms
  [& body] `(let [t0# (now-udt)] ~@body (- (now-udt) t0#)))

(defmacro time-ns
  [& body] `(let [t0# (now-nano)] ~@body (- (now-nano) t0#)))

;; https://github.com/ptaoussanis/encore/blob/master/src/taoensso/encore.cljc#L3259
(defmacro qb
  ([nlaps form & more] (map (fn [form] `(qb ~nlaps ~form)) (cons form more)))
  ([nlaps form]
   `(let [nlaps# ~nlaps
          [nsets# nlaps#] (if (vector? nlaps#) nlaps# [6 nlaps#])]
      (round2
       (/ (double
           (reduce min
                   (for [_# (range nsets#)]
                     (time-ns (dotimes [_# nlaps#] (do ~form))))))
          1e6)))))

(defn format
  ^String [^String fmt & args]
  #?(:clj  (String/format fmt (to-array args))
     :cljs (apply gstr/format fmt args)))

(defmacro do-true [& body]
  `(do ~@body true))

(defn do-task [t]
  (t (constantly nil) (constantly nil)))

(defn prn-task [t]
  (t #(prn [:ok %]) #(prn [:err %])))

(defn tap-task [t]
  (t #(tap> [:ok %]) #(tap> [:err %])))

(defn add-ns [ns m]
  (persistent!
    (reduce-kv
      (fn [acc k v] (assoc! acc (keyword ns k) v))
      (transient {})
      m)))

(defn remove-ns [m]
  (persistent!
    (reduce-kv
      (fn [acc k v] (assoc! acc (keyword (name k)) v))
      (transient {})
      m)))
