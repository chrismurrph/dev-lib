(ns au.com.seasoft.general.types
  (:require [clojure.spec.alpha :as s]))

;;
;; My own invention, is it a list or a vector?
;; Guards against: UnsupportedOperationException nth not supported on this type: PersistentArrayMap
;; , which happens if you try nth on a map:
;; `(nth {1 2} 0)`
;; `(let [[a] {1 2}] a)`
;; , the destructuring case being the common bug
;;
;; If spec s/coll-of fails for a map then can get rid of this all together. O/wise SO question...
;;
(def n-able? (every-pred coll? (complement map?)))

;;
;; My understanding of empty? didn't include nil (or maps particularly).
;; I didn't realize that in Clojure nil is fine/preferred over an empty coll,
;; so empty? should (and does) return true for nil.
;; This fn will find a lot of use with guardrails.
;; Use s/nilable here
;;
(defn empty-not-nil? [x]
  (and (n-able? x) (empty? x)))

(def filled-map? (every-pred map? seq))

;; Using fn? once resulted in this:
;; BUG: Internal error in expound or clojure spec.
;; So using anyfin? as a workaround
(defn anyfin? [_]
  true)

