
(ns book.util-test
  (:require 
            [clojure.test :refer [deftest testing is]]))
;; --- VAR block ----
(defn VAR [name]
  {:pre [(keyword? name)]}
  (list ::var name))

(defn VAR? [expr]
  (and (seq? expr) (= (first expr) ::var)))
;; --- VAR block ----

;; --- AND operator block ----
(defn AND [expr & rest-expr]
  (let [operands (distinct (cons expr rest-expr))
        not-and-operands (filter #(not= (first %) ::and) operands)
        and-operands (reduce concat '() (map rest (filter #(= (first %) ::and) operands)))
        operands (distinct (concat not-and-operands and-operands))]
    (if (> (count operands) 1)
      (cons ::and operands)
      expr)))

(defn rest-of-and [and_expr]
  {:pre [(= ::and (first and_expr)) (<= 2 (count (rest and_expr)))]}
  (def rest-of-expr (rest (rest and_expr)))
  (apply AND rest-of-expr))

(defn AND? [expr]
  (and (seq? expr) (= (first expr) ::and)))

;; ---- test ----
(def and-test-expr-1 (AND (AND (VAR :x) (VAR :y)) (AND (VAR :x) (VAR :y)) (AND (VAR :p) (VAR :q)) (VAR :z)))
(def and-test-expr-2 (AND (VAR :z) (VAR :x) (VAR :y) (VAR :p) (VAR :q)))
(is (= and-test-expr-1 and-test-expr-2))
;; --- AND operator block ----


;; --- OR operator block ----
(defn OR [expr & rest-expr]
  (let [operands (distinct (cons expr rest-expr))
        not-or-operands (filter #(not= (first %) ::or) operands)
        or-operands (reduce concat '() (map rest (filter #(= (first %) ::or) operands)))
        operands (distinct (concat not-or-operands or-operands))]
    (if (> (count operands) 1)
      (cons ::or operands)
      expr)))

(defn rest-of-or [or_expr]
  {:pre [(= ::or (first or_expr)) (<= 2 (count (rest or_expr)))]}
  (def rest-of-expr (rest (rest or_expr)))
  (apply OR rest-of-expr))

(defn OR? [expr]
  (and (seq? expr) (= (first expr) ::or)))

;; ---- test ----
(def or-test-expr-1 (OR (OR (VAR :x) (VAR :y)) (OR (VAR :x) (VAR :y)) (OR (VAR :p) (VAR :q)) (VAR :z)))
(def or-test-expr-2 (OR (VAR :z) (VAR :x) (VAR :y) (VAR :p) (VAR :q)))
(is (= or-test-expr-1 or-test-expr-2))
;; --- OR operator block ----



;; ---- NOT operator block ----
(defn NOT? [expr]
  (and (seq? expr) (= (first expr) ::not)))

(defn rest-of-not [not_expr]
  {:pre [(= ::not (first not_expr))]}
  (first (rest not_expr)))

(defn NOT [expr]
  (if (NOT? expr)
    (rest-of-not expr)
    (cons ::not (list expr))))

;; ---- test ----
(def not-test (NOT (NOT (VAR :x))))
(is (= not-test (VAR :x)))
;; ---- NOT operator block ----


(defn EXPR? [expr]
  (cond
    (VAR? expr) true
    (NOT? expr) true
    (OR? expr) true
    (AND? expr) true
    :else false))

(is (EXPR? (VAR :x)))
(is (EXPR? (OR (VAR :x) (VAR :y))))
(is (EXPR? (AND (VAR :x) (VAR :y))))
(is (EXPR? (NOT (VAR :x))))
(is (not (EXPR? 1)))


;; ---- привести выражение к строке ----
(defn str-expr [expr]
  {:pre [(EXPR? expr)]}
  (
   cond 
   (VAR? expr) (second expr)
   (AND? expr) (str "(" (subs (reduce #(str "&" (str-expr %2) %1) "" (rest expr)) 1) ")")
   (OR? expr) (str "(" (subs (reduce #(str "|" (str-expr %2) %1) "" (rest expr)) 1) ")") 
   (NOT? expr) (str "-" (str-expr (rest-of-not expr)))
  ))
;; ---- привести выражение к строке ----


;; ---- взять операнды выражения ----
(defn get-operands [expr]
  {:pre [(EXPR? expr) (not (VAR? expr))]}
  (rest expr))

;; ---- test ----
(is (= 
     (get-operands (AND (VAR :x) (VAR :y) (VAR :z)))
     (list (VAR :x) (VAR :y) (VAR :z))))
;; ---- взять операнды выражения ----



;; ---- пробросить отрицание в конец дерева ----
(defn apply-not [expr not-flag]
  {:pre [(EXPR? expr) (boolean? not-flag)]}
  (cond
    (VAR? expr) (if not-flag
                  (NOT expr) 
                  expr)
    (AND? expr) (if not-flag 
                  (apply OR (map #(apply-not %1 not-flag) (get-operands expr))) 
                  (apply AND (map #(apply-not %1 not-flag) (get-operands expr)))) 
    (OR? expr) (if not-flag 
                  (apply AND (map #(apply-not %1 not-flag) (get-operands expr)))
                  (apply OR (map #(apply-not %1 not-flag) (get-operands expr)))) 
    (NOT? expr) (apply-not (rest-of-not expr) (not not-flag))))

;; ---- test 1 ----
(def apply-not-test-1 (NOT (VAR :x)))
(str-expr (apply-not apply-not-test-1 false))
(is (= 
     (apply-not (NOT (VAR :x)) false)
     (NOT (VAR :x))))

;; ---- test 2 ----
(def apply-not-test-2 (NOT (AND (VAR :x) (VAR :y) (VAR :z))))
(apply-not apply-not-test-2 false)
(str-expr apply-not-test-2)
(str-expr (apply-not apply-not-test-2 false))
(is (= 
     (apply-not (NOT (AND (VAR :x) (VAR :y) (VAR :z))) false)
     (OR (NOT (VAR :x)) (NOT (VAR :y)) (NOT (VAR :z)))))


;; ---- test 3 ----
(def apply-not-test-3 (AND (NOT (OR (VAR :x) (VAR :y))) (VAR :t)))
(str-expr apply-not-test-3)
(str-expr (apply-not apply-not-test-3 false))
(is (=
     (apply-not (AND (NOT (OR (VAR :x) (VAR :y))) (VAR :t)) false)
     (AND (VAR :t) (NOT (VAR :x)) (NOT (VAR :y)))))
;; ---- пробросить отрицание в конец дерева ----


;; ---- применить "и-дистрибутивность" ----
;; 
;; or-expr = (X or Y)
;; and-operand = Z
;; (X or Y) and Z -> (X and Z) or (Y and Z)

(defn and-distr [or-expr and-operand]
  {:pre [(OR? or-expr) (EXPR? and-operand)]}
  (apply OR (map #(AND and-operand %1) (get-operands or-expr))))

;; ---- test ----
(def or-exp (OR (VAR :x1) (VAR :x2) (VAR :x3) (VAR :x4)))
(str-expr (AND or-exp (VAR :y)))
(str-expr (and-distr or-exp (VAR :y)))
(is (=
     (and-distr or-exp (VAR :y))
     (OR (AND (VAR :y) (VAR :x1)) (AND (VAR :y) (VAR :x2)) (AND (VAR :y) (VAR :x3)) (AND (VAR :y) (VAR :x4)))))
;; ---- применить "и-дистрибутивность" ----



;; ---- рекурсивно применить "и-дитрибутивность" ----
(defn apply-distr [expr]
  (cond
    (VAR? expr) expr ;; Одна переменная уже является ДНФ
    (NOT? expr) expr ;; Отрицание уже ДНФ т.к. до этого мы пробросили его к переменным
    (OR? expr) (apply OR (map apply-distr (get-operands expr))) ;; OR на ферхнем уровне уже ДНФ, нужно только привести к ДНФ его операнды

    (AND? expr) ;; AND не ДНФ (почти, за исключением случаев x1&x2&...&xn)
    (let [or-operand (first (filter OR? (get-operands expr)));; ищем первый OR операнд что бы запихнуть в него всё остальное
          other-operands (filter #(not= or-operand %1) (get-operands expr));; вытаскиваем остальные операнды
          ]

      (if (not= or-operand nil);; если нет OR операнда, то там не может быть AND операнда из-за конструктора, следовательно там только VAR и NOT
        (recur (reduce and-distr or-operand other-operands)) ;; выражение внутри уже возвращает OR, следовательно осталось его подать в apply-distr
        expr))))

;; ---- test ----
(def distr-test-1 (AND (VAR :x) (VAR :y) (VAR :z)))
(str-expr distr-test-1)
(str-expr (apply-distr distr-test-1))
(is (=
     (apply-distr distr-test-1)
     distr-test-1))


(def distr-test-2 (AND (OR (VAR :x) (VAR :y) (VAR :z)) (VAR :t) (VAR :p)))
(str-expr distr-test-2)
(str-expr (apply-distr distr-test-2))

(def distr-test-3 (AND (OR (VAR :x) (VAR :y)) (OR (VAR :z) (VAR :t)) (OR (VAR :q) (VAR :p))))
(str-expr distr-test-3)
(str-expr (apply-distr test))
;; ---- рекурсивно применить "и-дитрибутивность" ----


;; ---- Сделать красиво ----
(defn make-DNF [expr]
  {:pre  [(EXPR? expr)]}
  (apply-distr (apply-not expr false)))
;; ---- Сделать красиво ----



(def dnf-test-3 (AND (NOT (OR (VAR :x) (VAR :y))) (OR (VAR :z) (VAR :t)) (OR (VAR :q) (VAR :p))))
(str-expr dnf-test-3)
(str-expr (make-DNF dnf-test-3))
