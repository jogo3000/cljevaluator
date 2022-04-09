(ns core
  (:require [clojure.edn :as edn]
            [clojure.math :as math]))

(defonce safe-operations (atom '#{* + / -}))

(def precedence '[* / + -])

(defn preferred-over? [op1 op2]
  (let [[p1 p2]
        (map #(.indexOf precedence %)
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
  ([x operator y]
   (list operator
         (apply emit (listify x))
         (apply emit (listify y))))
  ([fn-or-x val-or-operator fn-or-y & [val-or-operator-2 & xs :as xsall]]
   (cond
     (symbol? fn-or-x)
     (let [f fn-or-x
           v val-or-operator]
       (apply emit (concat (emit f (emit v))) (cons fn-or-y xsall)))

     (symbol? fn-or-y)
     (let [x fn-or-x
           op val-or-operator
           f fn-or-y
           z val-or-operator-2]
       (apply
        emit
        (list x op
              (apply
               emit
               (apply
                emit
                (cons (list f (apply
                               emit
                               (listify z))) xs))))))

     :else
     (let [x fn-or-x
           op val-or-operator
           y fn-or-y
           op-2 val-or-operator-2]
       (if-not (preferred-over? op-2 op)
         (let [expr (emit x op y)]
           (emit (list op-2 expr (apply emit xs))))

         (emit y op-2 (first xs) op (apply emit (cons x (rest xs)))))))))

(defn emit-expression [s]
  (apply emit (parse-expression s)))


(defmacro defexpression [expr-name & args]
  (swap! safe-operations conj expr-name)
  `(defn ~expr-name ~@args))


(defexpression NELIÖJUURI [x]
  (clojure.math/sqrt x))

(defn my-eval [expr]
  (eval (emit-expression expr)))


(comment
  ;; Testing emit
  (apply emit '(1))

  (apply emit '(1 + 1))

  (apply emit '(f (1)))

  (apply emit '(x + y + z))

  (apply emit '(1 + 2 + 3 + 4 + 5))

  (apply emit '(x + y * z + w))

  (apply emit '(3 *  (1 + 2)))


  ;; Testing eval
  (my-eval "- 1")
  ;; => -1

  (my-eval "1 + 1 + 1")
  ;; => 3

  (my-eval "1 + 2 * 3")
  ;; => 7

  (my-eval "(1 + 2) * 3")
  ;; => 9

  (my-eval "NELIÖJUURI(2 + 2)")
  ;; => 2.0

  (my-eval "PAHAFUNKTIO(2 + 3)")
  ;; => NOT A SAFE OPERATION ERROR
  )
