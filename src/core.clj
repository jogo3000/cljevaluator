(ns core
  (:require [clojure.edn :as edn]
            [clojure.math :as math]))


(defonce safe-operations (atom '#{* + / -}))


(def precedence '[* / + -])

(defn preferred-over? [op1 op2]
  (let [[p1 p2]
        (map #(reduce-kv (fn [j i v]
                           (if (= v %) i j)) -1 precedence)
             [op1 op2])]
    (< p1 p2)))

(defn parse-expression [s]
  (edn/read-string (str "(" s ")")))

(defn listify [v]
  (if (list? v) v (list v)))

(defn emit
  ([x] x)
  ([f x]
   (when-not (contains? @safe-operations f)
     (throw (ex-info (str f " is not a safe operation!") {:context (list f x)})))
   (list f (apply emit (listify x))))
  ([x operator y] (list operator
                        (apply emit (listify x))
                        (apply emit (listify y))))
  ([x operator y & [op? & xs]]
   (if-not (preferred-over? op? operator)
     (let [expr (emit x operator y)]
       (emit (list op? expr (apply emit xs))))

     (emit y op? (first xs) operator (apply emit (cons x (rest xs)))))))

(comment
  ;; Testing emit
  (apply emit '(1))

  (apply emit '(1 + 1))

  (apply emit '(f (1)))

  (apply emit '(x + y + z))

  (apply emit '(1 + 2 + 3 + 4 + 5))

  (apply emit '(x + y * z + w))

  (apply emit '(3 *  (1 + 2))))


(defmacro defexpression [expr-name & args]
  (swap! safe-operations conj expr-name)
  `(defn ~expr-name ~@args))


(defexpression NELIÖJUURI [x]
  (clojure.math/sqrt x))

(defn my-eval [expr]
  (-> expr
      parse-expression
      ((partial apply emit))
      eval))


(comment

  ;; Testing eval
  (my-eval "- 1")
  ;; => -1

  (my-eval "1 + 1 + 1")
  ; => 3

  (my-eval "1 + 2 * 3")
  ;; => 7

  (my-eval "NELIÖJUURI(2 + 2)")
  ;; => 2.0

  (my-eval "PAHAFUNKTIO(2 + 3)")
  ;; => NOT A SAFE OPERATION ERROR
  )
