(ns au.com.seasoft.general.dev
  #?(:clj (:refer-clojure :exclude [assert]))
  (:require
    [clojure.string :as str]
    [au.com.seasoft.general.types :as gt]
    [cljs.reader :as reader]
    #?(:clj
       [clojure.pprint :as pprint])))

#?(:cljs (enable-console-print!))

#?(:cljs (def log-pr js/console.log)
   :clj  (def log-pr println))

;;
;; About instrumentation and probing that is really useful during development. Lots
;; of intentional crashing (i.e. asserts) here, but also instrumentation things like
;; warnings and pretty printing. Eventually we will elide the crashing out during runtime
;; and make into macros in case the asserts are heavy. For nuts and bolts of that (elide
;; crashing and macros) see a similar library:
;; https://github.com/astoeckley/clojure-assistant
;; When this application becomes 'really professional' then perhaps there will be no calls
;; to this ns, as spec and proper logging framework do the same thing.
;; That's why we call it 'dev'
;;

;;
;; If it is a map (when it shouldn't be) I don't usually want the 'has elements in it'
;; test to pass. Usually a map is a single item, so it is the wrong type to be asking `seq` of.
;; Use this rather than seq
;;
(defn least-one? [x]
  (and (gt/n-able? x) (seq x)))

;;
;; This means it is not {}!
;;
(defn filled-map? [x]
  (and (map? x) (seq x)))

;;
;; Theoretical (only) problem with this is no way of stopping a crash in production.
;; (because clojure.core/*assert* not available)
;; Another problem is speed, b/c args are being evaluated.
;; In reality the asserts will be working in production and that is fine.
;; devtools likes data structures not strings, that's why here using a vector not a string
;;
(defn assert-not-macro-1 [pred-res & args]
  (when (not pred-res)
    #?(:cljs (throw (js/Error. ["Assert failed" args]))
       :clj  (throw (AssertionError. ["Assert failed" args])))))

(declare pp-str)

(defn err [& args]
  (apply log-pr "PROBLEM:" args)
  #?(:clj (let [err-obj (Error. (str "Correct err Problem" args))]
            (spit "err.txt" (pp-str err-obj))
            (throw err-obj))))

;; Same as err, just writes to a different file
(defn assert-err [& args]
  (apply log-pr "PROBLEM:" args)
  #?(:clj (let [err-obj (Error. (str "Correct assert Problem" args))]
            (spit "assert.txt" (pp-str err-obj))
            (throw err-obj))))

(defn assert-not-macro [pred-res & args]
  (when (not pred-res)
    (assert-err args)))

;;
;; Didn't implement in cljs but could have:
;; https://code.thheller.com/blog/shadow-cljs/2019/10/12/clojurescript-macros.html
;; Reason didn't is that can't write to file from the browser, so no point.
;; But advantage would have been no need have separate config.
;;

;; Unfortunately I'm seeing \n in the file. Can replicate like this:
;; (spit "test.txt" "\\n\\nHi\\nnew line")
#?(:clj
   (defmacro assert
     "Evaluates expr and throws an exception if it does not evaluate to
     logical true Additionally writes the stack trace to a file."
     {:added "1.0"}
     ([x]
      (when clojure.core/*assert*
        `(when-not ~x
           (let [err-obj# (AssertionError. (str "Assert failed: " (pr-str '~x)))]
             (spit "assert.txt" (pp-str err-obj#))
             (throw err-obj#)))))
     ([x message]
      (when clojure.core/*assert*
        `(when-not ~x
           (let [err-obj# (AssertionError. (str "Assert failed: " ~message "\n" (pr-str '~x)))]
             (spit "assert.txt" (pp-str err-obj#))
             (throw err-obj#)))))))

(defn err-empty
  ([x]
   (assert x "Can't check if empty when nil")
   (assert (gt/n-able? x))
   (assert (seq x) "Can't assign empty")
   x)
  ([msg x]
   (assert x "Can't check if empty when nil")
   (assert (gt/n-able? x))
   (assert (seq x) (str "Can't assign empty, msg: " msg))
   x))

;;
;; Using apply to get devtools to format it properly
;;
(defn warn [& args]
  (assert (seq args) "Don't call warn with no args i.e. say something")
  (assert (some? (first args)) "Don't warn on nothing i.e. say something")
  (apply log-pr "WARN:" args))

(defn warn-off [& args]
  (assert (seq args) "Don't call warn with no args i.e. say something")
  (assert (some? (first args)) "Don't warn on nothing i.e. say something"))

;;
;; Will have to use this instead of asserts in all our components
;;
(defn assert-warn [pred-res & args]
  (when (not pred-res)
    (warn args)))

(defn assert-warn-off [pred-res & args])

(defn sleep [t]
  #?(:clj  (Thread/sleep t)
     :cljs (constantly nil)))

(defn chk-v! [v]
  (assert v)
  (assert (gt/n-able? v) v)
  (assert (seq v) v)
  (assert (every? (complement nil?) v) v))

;;
;; Fixing a terrible discovery that `(get-in {} nil)` will
;; just go on merrily, leading to bugs difficult to track down
;;
(defn get-inn
  ([st v]
   (chk-v! v)
   (clojure.core/get-in st v))
  ([st v default]
   (chk-v! v)
   (clojure.core/get-in st v default)))

;;
;; TODO
;; These two functions are only for clj, so ought to move them
;;

(def width 180)

;;
;; Can be cause of invisible performance problems when used in conjunction with log-off
;;
#?(:clj  (defn pp-str
           ([n x]
            (binding [pprint/*print-right-margin* n]
              (-> x pprint/pprint with-out-str)))
           ([x]
            (pp-str width x)))
   :cljs (def pp-str identity))

;;
;; TODO
;; pp can just be a pass thru not matter the args
;; TODO
;; Need do rest as well...
;;

#?(:clj  (defn pp
           ([n x]
            (binding [pprint/*print-right-margin* n]
              (-> x pprint/pprint)))
           ([x]
            (pp width x)))
   :cljs (def pp println))

;;
;; Hide the clutter briefly
;;
(defn pp-hide
  ([n x])
  ([x]))

(def hard-error? true)

(defn err-warn [predicate-res & msg]
  (if-not predicate-res
    (if hard-error?
      (assert false msg)
      (do
        (apply log-pr "WARNING:" msg)
        predicate-res))
    predicate-res))

(defn first-no-less [xs]
  (assert (least-one? xs) "Don't even have one: purposeful crash")
  ;; Get a crash like this, but easier to debug with your own message!
  (nth xs 0))

(defn type-and-value [value]
  (if (-> value fn? not)
    ["type" (type value) "value" value]
    []))

;;
;; name - of the thing we are asserting on
;; value - of the thing we are asserting on
;; Could do with being a macro...
;;
#?(:cljs (defn assert-info [name value]
           (into [name "nil?" (nil? value) "fn?" (fn? value)] (type-and-value value)))
   :clj  (defn assert-info [name value]
           (into [name "nil?" (nil? value) "fn?" (fn? value)] (type-and-value value))))

(defn- -one-only [f xs msgs]
  (f (= [true false] ((juxt
                        #(boolean (seq (take 1 %)))
                        #(boolean (seq (take 1 (drop 1 %))))) xs))
     (if (nil? xs)
       ["Expect to be one exactly, but got a nil, MSGs:" msgs]
       ["Expect to be one exactly, but got:" xs (map type xs) ", MSGs:" msgs]))
  (first xs))

;; Has to use a macro here, o/wise get 'Can't take value of macro' error
(defn one-only [xs & msgs]
  (-one-only assert-not-macro xs msgs))

(defn one-only-warn [xs & msgs]
  (-one-only assert-warn xs msgs))

(defn- -first-no-more [assert-f xs]
  (assert-f (gt/n-able? xs) ["Not n-able" xs])
  (assert-f (= nil (second xs))
            (str "Only supposed to be one. However:\nFIRST:\n" (first xs) "\nSECOND:\n" (second xs)))
  (first xs))

(defn first-no-more [xs]
  (-first-no-more assert-not-macro xs))

(defn first-no-more-warn [xs]
  (-first-no-more assert-warn xs))

(defn summarize [x]
  (cond
    (map? x) (let [counted (count x)]
               (if (> counted 5)
                 (str counted " map-entries...")
                 (->> x
                      (map (fn [[k v]]
                             [k (summarize v)]))
                      (into {}))))
    (coll? x) (let [counted (count x)]
                (if (> counted 5)
                  (str counted " items...")
                  x))
    :otherwise x))

(declare log-here)
(defn chk-dup-hof []
  (log-here "Going to be checking dups")
  (let [keys-atom (atom #{})]
    (fn [key]
      (let [keys @keys-atom]
        (when (keys key)
          (err "dup key" key ", already collected" (count keys) "min" (apply min keys) "max" (apply max keys)))
        (swap! keys-atom conj key)))))

;;
;; Order a map's keys randomly, to prove that need an ordered map
;;
(defn jumble-up-map [m]
  (assert (map? m))
  (->> (loop [in (mapv identity m)
              out []]
         (if (seq in)
           (let [picked (rand-nth in)]
             (recur (vec (remove #(= % picked) in)) (conj out picked)))
           out))
       (into {})))

(defn safe-first [x]
  (try
    (first x)
    (catch #?(:cljs :default :clj Throwable) _
      (err "Cannot first on " x)
      nil)))

(defn safe-second [x]
  (try
    (second x)
    (catch #?(:cljs :default :clj Throwable) _
      (err "Cannot second on " x)
      nil)))

;;
;; One day of debugging with Pathom hiding the error - lets always use this...
;;
(defn safe-select-keys
  ([m ks err-empty?]
   (assert (map? m) ["First arg to select-keys must be a map" (type m) m])
   (assert (vector? ks) ["Second arg to select-keys must be a vector" (type ks) ks])
   (let [res (select-keys m ks)]
     (when (and err-empty? (empty? res))
       ;; If an empty map is okay just put false as third arg
       (err "select-keys returning empty map, keys not found" ks "\n" (pp-str m)))
     res))
  ([m ks]
   (safe-select-keys m ks true)))

(defn safe-dissoc
  ([m & ks]
   (assert (map? m) ["First arg to dissoc must be a map" (type m) m])
   (assert (seq ks) ["Doesn't make sense to be dissoc-ing nothing" (type ks) ks])
   (assert (not (-> ks first vector?)) ["You usually dissoc a varargs list of keywords (so don't wrap in vector)" (first ks)])
   (let [res (apply dissoc m ks)]
     res)))

(defn safe-dissoc-noop
  ([m & ks]
   (assert (map? m) ["First arg to dissoc must be a map" (type m) m])
   (assert (seq ks) ["Doesn't make sense to be dissoc-ing nothing" (type ks) ks])
   (assert (not (-> ks first vector?)) ["You usually dissoc a varargs list of keywords (so don't wrap in vector)" (first ks)])
   m))

(defn safe-get [m k]
  (assert (map? m) ["First arg to get expected to be a map" m])
  (assert (seq m) ["Doesn't make sense to get from an empty map" m])
  (assert k ["Doing a lookup on a map with nil is probably unintended" k (keys m)])
  (let [res (get m k)]
    (if (some? res)
      res
      (throw (ex-info "Could not find key in map" {:key k :map-keys (keys m)})))))

(declare probe->)
(declare probe->>)
(declare probe->off)
(declare probe->>off)

(defn probe-count-on [xs]
  (log-pr "COUNT" (count xs))
  xs)

(defn probe-count-off [xs]
  xs)

(defn probe-first-on [xs]
  (log-pr "FIRST" (pp-str (first xs)))
  xs)

(defn probe-first-off [xs]
  xs)

(defn probe-first-n-on [n xs]
  (log-pr "Try take" n "from" (count xs) "\n" (pp-str (take n xs)))
  xs)

(defn probe-first-n-off [n xs]
  xs)

(defn prob-err-nil
  ([x]
   (assert x "Can't assign nil (or false)")
   x)
  ([x & msg]
   (assert x (apply str "Can't assign nil (or false), msg: " msg))
   x))

(defn probe-must-be-fn [f msg]
  (fn [x]
    (assert (not (f x)) (str msg ", got: <" x ">"))
    x))

;;
;; Using apply to get devtools to format it properly
;;
(defn- log-here [& args]
  (apply log-pr args))

;;
;; Using -on means we could turn it off, or into debug stuff.
;; Using these conventions means it is easy to search for the few that are -on,
;; due to latest forgotten debugging foray...
;; -on What currently debugging, s/only be a few in whole project
;; -off Might be useful again. Unfortunately many of these!
;; -a Will always be useful. Can be many.
;;
(def log-on log-here)
;; -a is short for 'always'
(def log-a log-here)

;; Macro not so necessary as there s/only be a few of these, and the message is usually small (unlike with pp or p-str)
;; Ha 166 usages and growing! So macro would be good.
#?(:cljs (defn log-off [& _])
   :clj  (defmacro log-off [& _]))

(defn probe-f-on [f x & msgs]
  (apply log-pr (f x) "<--" msgs)
  x)

(defn probe-f-off [f x & msgs]
  x)

(defn probe->off
  ([x]
   x)
  ([x & msgs]
   x))

(defn probe->
  "Messages go at the end, suitable for use with -> threaded macro where first param position is important"
  ([x]
   (-> x
       pp)
   x)
  ([x & msgs]
   (apply log-pr x "<--" msgs)
   x))

(defn probe->>off
  ([x]
   x)
  ([msg x]
   x))

(defn probe->>
  "Message goes at the beginning, suitable for use with ->> threaded macro where last param position is important"
  ([x]
   (-> x
       pp)
   x)
  ([msg x]
   (log-pr x "<--" msg)
   x))


;;
;; Sometimes I forget and use it, then get thrown out of JVM. This fixes that problem!
;;
(defn log [& args]
  (err "Don't use log, instead use log-on, to say: " (str/join ", " args)))