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
  ([f-or-x args-or-y]
   (cond
     (symbol? f-or-x)
     (let [f f-or-x
           args args-or-y]
       (when-not (contains? @safe-operations f)
         (throw (ex-info (str f " is not a safe operation!") {:context (list f args)})))
       (list f (apply emit (listify args))))
     :else
     (list (apply emit (listify f-or-x))
           (apply emit (listify args-or-y)))))
  ([x operator y]
   (list operator
         (apply emit (listify x))
         (apply emit (listify y))))
  ([fn-or-x x-or-operator fn-or-y & [y-or-operator & xs :as xsall]]
   (cond
     (and (symbol? fn-or-x) (list? x-or-operator))
     (let [f fn-or-x
           f-args x-or-operator]
       (apply emit (concat (emit f (emit f-args))) (cons fn-or-y xsall)))

     (and (symbol? fn-or-y) (list? y-or-operator))
     (let [x fn-or-x
           op x-or-operator
           f fn-or-y
           y y-or-operator]
       (apply
        emit
        (list x op
              (apply
               emit
               (apply
                emit
                (cons (list f (apply
                               emit
                               (listify y))) xs))))))

     :else
     (let [x fn-or-x
           op x-or-operator
           y fn-or-y
           op-2 y-or-operator]
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

(defexpression POW [x y]
  (clojure.math/pow x y))

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
