(ns core-test
  (:require [clojure.test :refer [deftest is testing]]
            [core :refer :all]))

(deftest basic-operations
  (is (= -1 (my-eval "- 1")))

  (is (= 3 (my-eval "1 + 1 + 1")))

  (is (= 7 (my-eval "1 + 2 * 3")))

  (is (= 4 (my-eval "1 + (3 * 1)")))

  (is (= 9 (my-eval "(1 + 2) * 3"))))

(deftest custom-expressions
  (is (= 2.0 (my-eval "NELIÖJUURI(4)")))

  (is (= 2.0 (my-eval "NELIÖJUURI(2 + 2)"))))

(deftest custom-expressions-interop
  (is (= 3.0 (my-eval "1 + NELIÖJUURI(4)")))
  (is (= 3.0 (my-eval "1 + NELIÖJUURI(2 + 2)")))

  (is (= 3.0 (my-eval "NELIÖJUURI(2 + 2) + 1")))

  (is (= 4.0 (my-eval "1 + NELIÖJUURI(2 + 2) + 1"))))

(deftest unsafe-operations-are-forbidden
  (is (thrown? clojure.lang.ExceptionInfo
               (my-eval "PAHAFUNKTIO(2 + 3)"))))
