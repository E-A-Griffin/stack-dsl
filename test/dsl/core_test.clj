(ns dsl.core-test
  (:require [dsl.core :as sut]
            [clojure.test :as t]
            [clojure.string :as str]
            [clojure.walk :as walk])
  (:import java.util.Calendar))

;; Test stack function declarations
(sut/defstackfn f
  [!a !b !c]
  !a
  !b
  (invoke> + 2)
  !v1+
  !c
  !c
  <pop>
  2
  (invoke> * 2)
  !v2+
  (invoke> = 2)
  (if>
    !v1
    !v2
    (invoke> - 2)
   else>
    "false!!"
    (invoke> println 1)
    <pop>
    !v1
    !v2
    (invoke> * 2)))

(sut/defstackfn g
  [!vo !vp]
  !vo
  !vp
  (invoke> inc 1)
  (invoke> max 2)
  "The maximum value is:"
  (invoke> println 2)
  0)

(sut/defstackfn sum-input
  [!n0 !n1 !n2 !n3 !n4]
  !n0 !n1 !n2 !n3 !n4
  (invoke> + 5))

(sut/defstackfn sum-input-lambda
  [!n0 !n1 !n2 !n3 !n4]
  !n0 !n1 !n2 !n3 !n4
  (invoke> #(reduce + %&) 5))

(sut/defstackfn reverse-input
  [!n0 !n1 !n2 !n3 !n4]
  !n0 !n1 !n2 !n3 !n4
  (invoke> list 5))

;; Checks if !element is one of predefined constants
(sut/defstackfn concat-constants
  [!element?]
  3
  "a"
  :key
  true
  (invoke> hash-set 4)
  !element?
  (invoke> #(%2 %1) 2)
  (if>
      " on stack"
   else>
      " not on stack")
  !element?
  (invoke> (comp println str) 2))

;; Note that function can be defined but produces an error when invoked with
;; any argument
(sut/defstackfn undeclared-var
  [!a !b]
  !a
  !c ;; Semantic Error: attempt at pushing undeclared variable on stack
  !b)

(sut/defstackfn shadowing
  [!a !b]
  !a
  <prn-state>
  (local [!a 25 !b 4]
             !a
             <prn-state>)
  <prn-state>)

(sut/defstackfn recursive-f
  [!o]
  !o
  (invoke> zero? 1)
  (if>
    true
   else>
    !o
    (invoke> prn 1)
    !o
    (invoke> dec 1)
    (invoke> recursive-f 1)))

;; Fibonacci (showcases binary recursion)
(sut/defstackfn fib
  [!n]
  !n
  (invoke> #(> % 1) 1)
  (if>
    !n
    (invoke> dec 1)
    (invoke> fib 1)
    !n
    (invoke> (comp dec dec) 1)
    (invoke> fib 1)
    (invoke> + 2)
   else>
    !n))

;; Fizzbuzz
(sut/defstackfn fizzbuzz
  []
  (local [!fizz-buzz (stackfn [!in]
                              !in
                              (invoke> (fn [x]
                                         (let [no-rem?
                                               (comp zero? (partial mod x))]
                                           (cond-> ""
                                             (no-rem? 3) (str "fizz")
                                             (no-rem? 5) (str "buzz"))))
                                       1))]
         (while> [0 int?]
          (invoke>
           (fn []
             (print
              "Enter a non-negative integer (enter anything else to exit): "))
             0)
          <pop>
          (invoke> read-line 0)
          (invoke> (fn str->int [s]
                     (if (re-matches #"\d+" s)
                       (read-string (re-matches #"\d+" s))))
                   1)
          <dup>
          (if>
            (invoke> !fizz-buzz 1)
            (invoke> println 1))
          <pop>)))

;; Make a map: Takes a list of keys and a list of values and constructs
(sut/defstackfn make-a-map
  [!ks !vs]
  !vs
  !ks
  (invoke> zipmap 2))

;; Make a map from interleaved keys and values
(sut/defstackfn make-a-map-varargs
  [& !ks+vs]
  !ks+vs
  (invoke> (partial partition 2) 1)
  (invoke> (partial map vec) 1)
  (invoke> (partial into {}) 1))

;; Loops
(sut/defstackfn prn-n->100
  [!n]
  (doseq> (range !n 100)
              (invoke> prn 1)))

(sut/defstackfn prn-&-return-n->100
  [!n]
  (foreach> (range !n 100)
            <dup>
            (invoke> prn 1)
            <pop>)
  (invoke> list 0)
  !l+
  <pop>
  (<do
    !l
    (invoke> conj 2)
    !l+
    <pop>
    while> number?)
  !l)

(sut/defstackfn prn-n->0
  [!n]
  (until> [!n zero?]
          <dup>
          (invoke> prn 1)
          <pop>
          (invoke> dec 1))
  <pop>)

(sut/defstackfn prn-&-return-n->0
  [!n]
  !n
  (invoke> pos? 1)
  (if>
    !n
    (invoke> inc 1)
    !n+
    <pop>
    (foreach> (->> !n (range 1) reverse)
              <dup>
              (invoke> prn 1)
              <pop>)
    (invoke> list 0)
    !l+
    <pop>
    (<do
     !l
     (invoke> conj 2)
     !l+
     <pop>
     while> number?)
    !l
   else>
    (invoke> list 0)))

(sut/defstackfn nested-for-summation
  [!outer !middle !inner]
  0
  !sum+
  <pop>
  (foreach> (range !outer)
            !i+
            <pop>
            (foreach> (range !middle)
                      !j+
                      <pop>
                      (foreach> (range !inner)
                                !i
                                !j
                                !sum
                                (invoke> + 4)
                                !sum+)))
  !sum)

;; Test multi-arity
(sut/defstackfn nested-doseq-summation
  ([!outer !middle !inner]
   0
   !sum+
   <pop>
   (doseq> (range !outer)
           !i+
           (doseq> (range !middle)
                   !j+
                   (doseq> (range !inner)
                           !i
                           !j
                           !sum
                           (invoke> + 4)
                           !sum+)))
   !sum)
  ([!middle !inner]
   !inner
   !middle
   1
   (invoke> nested-doseq-summation 3))
  ([!inner]
   !inner
   1
   (invoke> nested-doseq-summation 2))
  ([]
   1
   (invoke> nested-doseq-summation 1)))

(sut/defstackfn break-and-continue
  [!n]
  !n
  (until> zero?
          (invoke> (fn [n] (- n 2)) 1)
          <dup>
          (invoke> neg? 1)
          (if>
           0
           continue
           "this shouldn't print"
           (invoke> println 1)))
  (while> zero?
    <dup>
    (invoke> (complement pos?) 1)
    (if>
     1
     continue
     0
     else>
     0))
  (until> zero?
          ;; Should only iterate once
          (foreach> (range 1000)
                    (invoke> zero? 1)
                    (if>
                      0
                      break
                      1)))
  (<do
     (doseq> (range 1000)
             (invoke> (complement pos?) 1)
             (if>
               false
               break
               13))
   while> true?)
  !n)

;; Test internal state
(sut/defstackfn state [& !vargs]
  (local [!a :a
          !b :b
          !c :c]
         !a
         (local [!a "a"
                 !b "b"
                 !c "c"]
                !b
                (local [!a true
                        !b false]
                       !a
                       (local [!a 0
                               !b 1
                               !c 2]
                              !vargs
                              !c
                              <prn-state>)))))

(sut/defstackfn closures [!x]
  (local [!f0 (stackfn [!y] !x !y (invoke> vector 2) <prn-state>)
          !f1 (stackfn [!z] !z (invoke> !f0 1) <prn-state>)
          !f2 (stackfn [!a] !a !x (invoke> + 2) (invoke> !f1 1) <prn-state>)
          !fv (stackfn [& !args]
                       !args
                       (invoke> (partial apply +) 1)
                       (invoke> !f2 1) <prn-state>)]
         1 2 3 4 5
         (invoke> !fv 5)
         <prn-state>))

;; Tic-tac-toe
;; Great program for testing a very wide variety of features of DSL but
;; automating testing given the need for user input and the random nature of
;; the AI's moves makes this challenging, thus testing here has been performed
;; manually and the resultant function has been left in as a somewhat practical
;; example program
(sut/defstackfn tic-tac-toe
  []
  (local  [!prn-board (stackfn [!board]
                               (doseq> !board
                                       " "
                                       (invoke> #(apply str (repeat 8 %)) 1)
                                       (invoke> println 2)))
           !construct-init-board (stackfn []
                                          (foreach>
                                           (reverse (partition 3 (range 10)))
                                           (invoke> (partial apply vector) 1))
                                          (invoke> vector 3))
           !get-available-spaces (stackfn [!board]
                                          !board
                                          (invoke> (comp
                                                    (partial filter
                                                             number?)
                                                    flatten) 1))
           !update-game-board (stackfn [!board !x? !idx]
                                       !x?
                                       (if>
                                         "x"
                                        else>
                                         "o")
                                       !idx
                                       !board
                                       (invoke> (fn [board idx mark]
                                                  (assoc-in board
                                                   [(quot idx 3) (mod idx 3)]
                                                   mark)) 3))
           !ai-turn (stackfn [!available-spaces !board]
                             !available-spaces
                             (invoke> count 1)
                             (invoke> rand-int 1)
                             !available-spaces
                             (invoke> nth 2)
                             false
                             !board
                             (invoke> !update-game-board 3))
           !column? (stackfn [!n0 !n1 !n2]
                             !n2 !n1 !n0
                             (invoke> (fn [n0 n1 n2]
                                        (apply = (map #(mod % 3)
                                                      (list n0 n1 n2))))
                                      3))
           !row? (stackfn [!n0 !n1 !n2]
                          (invoke> (fn [] (list 0 1 2)) 0)
                          (invoke> (fn [] (list 3 4 5)) 0)
                          (invoke> (fn [] (list 6 7 8)) 0)
                          !n2 !n1 !n0
                          (invoke> list 3)
                          (invoke> sort 1)
                          (invoke> (fn [ns case0 case1 case2]
                                     (or (= ns case0)
                                         (= ns case1)
                                         (= ns case2)))
                                   4))
           !diagonal? (stackfn [!n0 !n1 !n2]
                               (invoke> (fn [] (list 0 4 8)) 0)
                               (invoke> (fn [] (list 2 4 6)) 0)
                               !n2 !n1 !n0
                               (invoke> list 3)
                               (invoke> sort 1)
                               (invoke> (fn [ns case0 case1]
                                          (or (= ns case0)
                                              (= ns case1))) 3))
           !win? (stackfn [!ns]
                          ;; Unpack each element of !ns onto stack
                          (foreach> !ns
                                    (invoke> identity 1))
                          (invoke> !diagonal? 3)
                          (if>
                            true
                           else>
                            (foreach> !ns
                                    (invoke> identity 1))
                            (invoke> !row? 3)
                            (if>
                              true
                             else>
                              (foreach> !ns
                                        (invoke> identity 1))
                              (invoke> !column? 3))))
           ;; Return indices of xs placed on gameboard
           !get-xs (stackfn [!board]
                            !board
                            (invoke> flatten 1)
                            (invoke> (fn [board]
                                       (keep-indexed #(if (= %2 "x") %1)
                                                     board)) 1))
           ;; Return indices of os placed on gameboard
           !get-os (stackfn [!board]
                            !board
                            (invoke> flatten 1)
                            (invoke> (fn [board]
                                       (keep-indexed #(if (= %2 "o") %1)
                                                     board)) 1))
           ;; Get a cartesian product
           !cart (stackfn [!ns]
                          !ns
                          (invoke> (fn [ns]
                                     (into #{}
                                           (filter some?
                                                   (for [i ns
                                                         j ns
                                                         k ns]
                                                     (if (distinct? i j k)
                                                       #{i j k}))))) 1))
           ;; Returns "x" if x won, "o" if o won, and nil if game not yet
           ;; finished
           !get-winner (stackfn [!board]
                                !board
                                (invoke> !get-xs 1)
                                (invoke> !cart 1)
                                !cart-xs+
                                (foreach> !cart-xs
                                          <dup>
                                          (invoke> set? 1)
                                          (if>
                                            (invoke> !win? 1)
                                            !x-won?+
                                            !x-won?
                                            (if> "x" break)))
                                (invoke> (partial = "x") 1)
                                (if>
                                  "x"
                                 else>
                                  !board
                                  (invoke> !get-os 1)
                                  (invoke> !cart 1)
                                  !cart-os+
                                  (foreach> !cart-os
                                            <dup>
                                            (invoke> set? 1)
                                            (if>
                                              (invoke> !win? 1)
                                              !o-won?+
                                              !o-won?
                                              (if>
                                                "o"
                                                break)))
                                (invoke> (partial = "o") 1)
                                (if> "o" else> false)))
           !prompt-user (stackfn prompt-user [!board]
                                 !board
                                 (invoke> !get-available-spaces 1)
                                 <dup>
                                 "Input a number"
                                 "for your next move: "
                                 (invoke> #(println %2 %3 %1) 3)
                                 <pop>
                                 !board
                                 (invoke> !prn-board 1)
                                 <pop>
                                 input>
                                 (invoke> (fn str->int [s]
                                            (if (re-matches #"\d+" s)
                                               (read-string
                                                (re-matches #"\d+" s)))) 1)
                                 !in+
                                 (invoke> #(some #{%1} %2) 2)
                                 <dup>
                                 (if>
                                   true
                                   !board
                                   (invoke> !update-game-board 3)
                                   !board+
                                   <dup>
                                   (invoke> !get-available-spaces 1)
                                   ;; If a draw happens, it's always after the
                                   ;; player's move and before the AI's move
                                   <dup>
                                   (invoke> empty? 1)
                                   (if>
                                     ;; Check if x won
                                     !board
                                     (invoke> !get-winner 1)
                                     <dup>
                                     (invoke> string? 1)
                                     (if>
                                       "WINNER IS:"
                                       "!!!!!!!!!!!!!!!"
                                       (invoke> #(println %2 %3 %1) 3)
                                        !board
                                       (invoke> !prn-board 1)
                                      else>
                                       "Draw!"
                                       (invoke> println 1))
                                    else>
                                     (invoke> !ai-turn 2)
                                     "AI's move: "
                                     (invoke> println 1)
                                       <pop>
                                       !board+
                                       (invoke> !prn-board 1)
                                       !board
                                       (invoke> !get-winner 1)
                                       <dup>
                                       (invoke> string? 1)
                                       (if>
                                         "WINNER IS:"
                                         "!!!!!!!!!!!!!!!"
                                         (invoke> #(println %2 %3 %1) 3)
                                         !board
                                         (invoke> !prn-board 1)
                                        else>
                                         <pop>
                                         !board))
                                  else> ;; Check if space unavailable
                                   !in
                                   (invoke> number? 1)
                                   (if>
                                     "Invalid space entered!"
                                     (invoke> println 1)
                                     !board
                                     (invoke> prompt-user 1))))]

          "Welcome to Tic-Tac-Toe!"
          (invoke> println 1)
          <pop>
          (invoke> !construct-init-board 0)
          (<do
           (invoke> !prompt-user 1)
           while> vector?)))

;; Test java-interop
(sut/defstackfn get-timezone
  []
  (.static> (new java.util.GregorianCalendar) getTimeZone))

;; Split !args of integers up based on even/odd parity, maintaining originally
;; provided ordering for each partition
(sut/defstackfn partition-by-parity [& !args]
             !args
             (invoke> (partial filter even?) 1)
             (invoke> (fn [coll] (new java.util.LinkedList coll)) 1)
             !evens+
             <pop>
             !args
             (invoke> (partial filter odd?) 1)
             (invoke> (fn [coll] (new java.util.LinkedList coll)) 1)
             !odds+
             <pop>
             !args
             (invoke>
              (fn [coll] (new java.util.ArrayDeque (count coll))) 1)
             !deque+
             false
             !evens-empty+
             !odds-empty+
             <pop>
             (until> true?
                     !evens
                     (.> isEmpty)
                     !evens-empty+
                     (if>
                      else>
                       (.var> !evens pollLast)
                       !polled+
                       (.var> !deque addFirst !polled))
                     !odds
                     (.> isEmpty)
                     (if>
                       !evens-empty
                      else>
                       (.var> !odds poll)
                       !polled+
                       (.var> !deque addLast !polled)))
             (.var> !deque toArray))

;; To simulate the error handling of defstackfn without dealing with trying to
;; catch clojure.lang.compiler$compilerexception, simply check
;; validate-stack-fn-args instead
(t/deftest type-check-defstackfn
  (t/are [msg declaration] (thrown-with-msg? IllegalArgumentException
                                             (re-pattern msg)
                                             declaration)
    (str "defstackfn expects its first argument to be a function name "
         "specified as a symbol,\n"
         "but instead defstackfn received " (type "bad-name-type"))
    (sut/validate-stack-fn-args "bad-name-type" ['!a '!b '!c] nil)

    (str "defstackfn expects its second argument to be a vector of "
         "parameters that the\n"
         "declared function expects but instead "
         "defstackfn received " (type '(!a !b !c)))
    (sut/validate-stack-fn-args 'good-name '(!a !b !c) nil)

    (str "defstackfn expects each variable to be prefixed with '!' "
         "but received the following\n"
         "variables that do not start with '!':\n\t"
         (str/join ", " ["a" "c"]))
    (sut/validate-stack-fn-args 'good-name ['a '!b 'c] nil)))

;; Basic error checking tests for stack function f
(defn produce-err-msg [qualified-name n-args]
  (str "Wrong number of args (" n-args ") passed to: " qualified-name))

(def produce-t-err-msg (partial produce-err-msg 'dsl.core-test/f))

(t/deftest test-f-arity
  (t/are [n-args invocation] (try invocation
                                  false
                                  (catch clojure.lang.ArityException er
                                    (=
                                       (ex-message er)
                                       (produce-t-err-msg n-args))))

    0 (f)
    1 (f 0)
    2 (f 0 1)
    4 (f 0 1 2 3)))

(t/deftest test-bad-stackfn-declarations
  (let [make-syn-err (fn [s] (str "Syntax error macroexpanding " s " at "))
        syn-err-fn (make-syn-err "clojure.core/fn")
        syn-err-stackfn (make-syn-err "dsl.core/stackfn")
        syn-err-local (make-syn-err "dsl.core/local")
        syn-err-> (make-syn-err "clojure.core/->")
        conform-spec-err-fn "Call to clojure.core/fn did not conform to spec."
        stack-params-err
        (str "stackfn requires that all parameter names start with '!', but "
             "the following was given as parameter name\\(s\\): ")
        param-decl-vec-err "stackfn requires parameter declaration be a vector"
        local-var-err "local requires local variables to start with '!'"
        single-arity-param-err
        (str "stackfn requires first argument to a single-arity stackfn "
             "declaration be a vector of parameters")
        invalid-stack-expr-err
        (str/join "\n"
                  ["A valid stack function expects each expr to be one of:"
                   "\\(declared\\) variable \\(including arguments\\) e.g. !a"
                   "assignment to a new or predeclared variable e.g. !a\\+"
                   "a constant \\(numeric, string, keyword, or boolean\\) e\\.g\\. 7"
                   "one of the following forms: "
                   "\tlocal, <do, if>, <prn-state>, <clear-stack>, until>, while>, <dup>, \\.>, <push-constant>, input>, doseq>, invoke>, \\.static>, <pop>, stackfn, foreach>"
                   "but received "])]
    (t/are [stackfn-decl nested-ex-type syntax-err-msg
            nested-ex-err-msg]
        (and
         (t/is (thrown-with-msg?
                clojure.lang.Compiler$CompilerException
                (re-pattern syntax-err-msg)
                (walk/macroexpand-all stackfn-decl)))
         (let [nested-ex (ex-cause (try
                                     (walk/macroexpand-all stackfn-decl)
                                     false
                                     (catch
                                         clojure.lang.Compiler$CompilerException
                                         e
                                       e)))]
           (and (instance? nested-ex-type nested-ex)
                (not-empty
                 (re-find
                  (re-pattern nested-ex-err-msg)
                  (ex-message nested-ex))))))
      ;; NOTE: Using alias here makes testing in the REPL a pain, if these tests
      ;; fail after changing the namespace stackfn is defined in, these tests
      ;; need to change as well to accomodate this
      '(dsl.core/stackfn (!x !x))
      IllegalArgumentException
      syn-err-fn
      param-decl-vec-err

      '(dsl.core/stackfn ([x] x))
      IllegalArgumentException
      syn-err-fn
      (str stack-params-err "x")

      '(dsl.core/stackfn [x] x)
      IllegalArgumentException
      syn-err-fn
      (str stack-params-err "x")

      '(dsl.core/stackfn ([!x y !z a] x))
      IllegalArgumentException
      syn-err-fn
      (str stack-params-err "y, a")

      (nth (macroexpand '(dsl.core/stackfn named
                                           ([] true)
                                           ([!b] !b)
                                           ([!b !c] !c !b (invoke> + 2))
                                           ([!x y !z a & args] x))) 2)
      IllegalArgumentException
      syn-err-fn
      (str stack-params-err "y, a, args")

      '(dsl.core/stackfn [!x y !z a & args] x)
      IllegalArgumentException
      syn-err-fn
      (str stack-params-err "y, a, args")

      '(dsl.core/stackfn named (!x !x))
      IllegalArgumentException
      syn-err-fn
      param-decl-vec-err

      '(dsl.core/stackfn named !x !x)
      IllegalArgumentException
      syn-err-stackfn
      single-arity-param-err

      '(dsl.core/stackfn "bad-name" !x !x)
      IllegalArgumentException
      syn-err-stackfn
      single-arity-param-err

      '(dsl.core/stackfn named ([!x y !z a] x))
      IllegalArgumentException
      syn-err-fn
      (str stack-params-err "y, a")

      '(dsl.core/stackfn named [!x y !z a] x)
      IllegalArgumentException
      syn-err-fn
      (str stack-params-err "y, a")

      ;; First macroexpand will succeed producing
      ;; (def named (dsl.core/stackfn !x !x))
      ;; the binding (dsl.core/stackfn !x !x) should then produce two exceptions
      ;; upon macroexpansion
      (nth (macroexpand '(dsl.core/defstackfn named !x !x)) 2)
      IllegalArgumentException
      syn-err-stackfn
      single-arity-param-err

      (nth (macroexpand '(dsl.core/defstackfn named (!x !x))) 2)
      IllegalArgumentException
      syn-err-fn
      param-decl-vec-err

      (nth (macroexpand '(dsl.core/defstackfn named ([x] x))) 2)
      IllegalArgumentException
      syn-err-fn
      (str stack-params-err "x")

      (nth (macroexpand '(dsl.core/defstackfn named [x] x)) 2)
      IllegalArgumentException
      syn-err-fn
      (str stack-params-err "x")

      (nth (macroexpand '(dsl.core/defstackfn named [!x y !z a & args] x)) 2)
      IllegalArgumentException
      syn-err-fn
      (str stack-params-err "y, a, args")

      (nth (macroexpand '(dsl.core/defstackfn named ([!x y !z a & args] x))) 2)
      IllegalArgumentException
      syn-err-fn
      (str stack-params-err "y, a, args")

      (nth (macroexpand '(dsl.core/defstackfn named
                           ([] true)
                           ([!b] !b)
                           ([!b !c] !c !b (invoke> + 2))
                           ([!x y !z a & args] x))) 2)
      IllegalArgumentException
      syn-err-fn
      (str stack-params-err "y, a, args")

      (nth (macroexpand '(dsl.core/defstackfn named
                           ([!x & !y & !z] !z))) 2)
      clojure.lang.ExceptionInfo
      syn-err-fn
      conform-spec-err-fn

      (nth (macroexpand '(dsl.core/stackfn named
                                           ([!x & !y & !z] !z))) 2)
      clojure.lang.ExceptionInfo
      syn-err-fn
      conform-spec-err-fn

      (nth (macroexpand '(dsl.core/stackfn named
                                           ([!x !y & & !z] !z)
                                           ([!x & !y & !z] !z))) 2)
      clojure.lang.ExceptionInfo
      syn-err-fn
      conform-spec-err-fn

      (nth (macroexpand '(dsl.core/stackfn named
                                           ([!x !y & !z] !z)
                                           ([!x & !y & !z] !z))) 2)
      clojure.lang.ExceptionInfo
      syn-err-fn
      conform-spec-err-fn

      ;; Test syntax errors able to be caught at function declaration
      '(dsl.core/stackfn named [!x] (local [y (stackfn [!x] !x)]))
      IllegalArgumentException
      syn-err-local
      local-var-err

      '(dsl.core/stackfn named [!x] (local [!y (stackfn [!x] !x)
                                            !x (stackfn [!y] !y)
                                            z (stackfn [!a] !a)]))
      IllegalArgumentException
      syn-err-local
      local-var-err

      '(dsl.core/stackfn named [!x] x)
      IllegalArgumentException
      syn-err->
      (str invalid-stack-expr-err "x")

      '(dsl.core/stackfn named [!x] prn)
      IllegalArgumentException
      syn-err->
      (str invalid-stack-expr-err "prn")

      '(dsl.core/stackfn named [!x] !x (invoke> prn 1) clojure.core/inc)
      IllegalArgumentException
      syn-err->
      (str invalid-stack-expr-err "clojure.core/inc")

      '(dsl.core/stackfn named [!x] !x (invoke> prn 1) '!x)
      IllegalArgumentException
      syn-err->
      (str invalid-stack-expr-err "\\(quote !x\\)")

      '(dsl.core/stackfn named [!x] !x (invoke> prn 1) {})
      IllegalArgumentException
      syn-err->
      (str invalid-stack-expr-err "\\{\\}")

      '(dsl.core/stackfn named [!x] {})
      IllegalArgumentException
      syn-err->
      (str invalid-stack-expr-err "\\{\\}")

      '(dsl.core/stackfn named [!x] !x (invoke> prn 1) '())
      IllegalArgumentException
      syn-err->
      (str invalid-stack-expr-err '())

      '(dsl.core/stackfn named [!x] '())
      IllegalArgumentException
      syn-err->
      (str invalid-stack-expr-err '())

      '(dsl.core/stackfn named [!x] [])
      IllegalArgumentException
      syn-err->
      (str invalid-stack-expr-err "\\[\\]")

      '(dsl.core/stackfn named [!x] [!x])
      IllegalArgumentException
      syn-err->
      (str invalid-stack-expr-err "\\[!x\\]")

      '(dsl.core/stackfn named [!x] #{})
      IllegalArgumentException
      syn-err->
      (str invalid-stack-expr-err "#\\{\\}")

      '(dsl.core/stackfn named [!x] (range 10))
      IllegalArgumentException
      syn-err->
      (str invalid-stack-expr-err "\\(range 10\\)")

      '(dsl.core/stackfn named [!x] (range !x))
      IllegalArgumentException
      syn-err->
      (str invalid-stack-expr-err "\\(range !x\\)")

      '(dsl.core/stackfn ([!x] (range !x)))
      IllegalArgumentException
      syn-err->
      (str invalid-stack-expr-err "\\(range !x\\)")

      (nth (macroexpand '(dsl.core/defstackfn named ([!x] (range !x)))) 2)
      IllegalArgumentException
      syn-err->
      (str invalid-stack-expr-err "\\(range !x\\)")

      (nth (macroexpand '(dsl.core/defstackfn named ([!x] (fn [!x] !x)))) 2)
      IllegalArgumentException
      syn-err->
      (str invalid-stack-expr-err "\\(fn \\[!x\\] !x\\)")

      (nth (macroexpand '(dsl.core/defstackfn named
                           ([] 35)
                           ([!x] !x 35 (fn [!x] !x)))) 2)
      IllegalArgumentException
      syn-err->
      (str invalid-stack-expr-err "\\(fn \\[!x\\] !x\\)"))))

(t/deftest test-stackfn-runtime-errors
  (t/are [stackfn-decl args ex-type ex-err-msg]
      (thrown-with-msg?
       ex-type
       (re-pattern ex-err-msg)
       (apply stackfn-decl args))
      ;; NOTE: Using alias here makes testing in the REPL a pain, if these tests
      ;; fail after changing the namespace stackfn is defined in, these tests
      ;; need to change as well to accomodate this
      (sut/stackfn [!x] !y) [3]
      IllegalArgumentException
      "Variable !y undeclared"

      (sut/stackfn ([!x] !y)) [3]
      IllegalArgumentException
      "Variable !y undeclared"

      (sut/stackfn
       ([] 3)
       ([!x] !y)) [3]
      IllegalArgumentException
      "Variable !y undeclared"

      (sut/defstackfn bad-stackfn ([!x] !y)) ["3"]
      IllegalArgumentException
      "Variable !y undeclared"

      (sut/defstackfn bad-stackfn [!x] !y) ["3"]
      IllegalArgumentException
      "Variable !y undeclared"
      ))


;; Need two tests, one for side effect (printing), the other for
;; returned result
;;
;; f tests
(t/deftest test-provided-result
  (t/is (= 24 (f 1 2 4))))

(t/deftest test-provided-side-effect
  (t/is (= "false!!\n"
           (with-out-str (f 1 2 4)))))

(t/deftest test-true-branch
  (t/is (= 0 (f 3 3 3))))

(t/deftest test-true-branch-no-side-effect
  (t/is (= ""
           (with-out-str (f 3 3 3)))))

;; g tests
(t/deftest test-provided-result
  (t/is (zero? (g 0 1))))

(t/deftest test-provided-side-effect
  (t/is (= "The maximum value is: 2\n"
           (with-out-str (g 0 1)))))

(t/deftest test-provided-side-effect
  (t/is (= "The maximum value is: 28\n"
           (with-out-str (g 13 27)))))

;; sum-input & sum-input-lambda
(t/deftest test-sum-inputs
  (t/are [x y z] (= x y z)
    15 (sum-input 1 2 3 4 5) (sum-input-lambda 1 2 3 4 5)
    0  (sum-input 0 -1 1 2 -2) (sum-input-lambda 0 -1 1 2 -2)))

;; reverse-input
(t/deftest test-rev-input
  (t/are [coll] (= (apply reverse-input coll) (reverse coll))
    (range 5)
    (range -3 2)
    "a str"))

;; concat-constants
(t/deftest test-concat-constants-members
  (t/are [e?] (and (= nil (concat-constants e?))
                   (= (with-out-str (concat-constants e?))
                      (str e? " on stack\n")))
    3
    "a"
    :key
    true))

(t/deftest test-concat-constants-not-members
  (t/are [e?] (and (= nil (concat-constants e?))
                   (= (with-out-str (concat-constants e?))
                      (str e? " not on stack\n")))
    4
    "b"
    :val
    false))

;; undeclared-var
(t/deftest test-undeclared-var
  (t/are [x y]
      (thrown-with-msg? IllegalArgumentException
                          #"Variable !c undeclared"
                          (undeclared-var x y))
    0 0
    1 1
    2 2))

;; TODO: Add test using this
(comment (dsl.core/local ['(10) '({})]
                [!stackfn (dsl.core/stackfn [!x]
                                            (dsl.core/local [!stackfn
                                                             (dsl.core/stackfn
                                                              [!x] !x
                                                              (dsl.core/invoke>
                                                               dec 1))]
                                                            !x (dsl.core/invoke>
                                                                !stackfn 1)))]
                (dsl.core/until>
                 zero?
                 dsl.core/peek-state>
                 (dsl.core/invoke> !stackfn 1))))

;; Test shadowing
(t/deftest test-shadowing-result
  (t/is (= (shadowing 3 5)
           25)))

(t/deftest test-shadowing-side-effect
  (t/is
   (=
    "[(3) ({!a 3, !b 5})]\n[(25 3) ({!a 25, !b 4} {!a 3, !b 5})]\n[(25 3) ({!a 3, !b 5})]\n"
    (with-out-str (shadowing 3 5)))))

;; Test recursion
(t/deftest test-recursive-f-result
  (t/are [x] (true? x)
    (recursive-f 0)
    (recursive-f 1)
    (recursive-f 2)
    (recursive-f 3)
    (recursive-f 4)
    (recursive-f 5)))

(t/deftest test-recursive-f-side-effects
  (t/are [x y] (= x (with-out-str y))
    "" (recursive-f 0)
    "1\n" (recursive-f 1)
    "2\n1\n" (recursive-f 2)
    "3\n2\n1\n" (recursive-f 3)
    "4\n3\n2\n1\n" (recursive-f 4)
    "5\n4\n3\n2\n1\n" (recursive-f 5)))

(t/deftest test-fib-results
  (t/is (= (for [i (range 23)]
             (fib i))
           '(0 1 1 2 3 5 8 13 21 34 55 89 144 233 377 610 987 1597 2584
             4181 6765 10946 17711))))

;; Test map functions
(t/deftest test-make-a-map-results
  (t/are [x y] (= x
                  (apply make-a-map y)
                  (apply make-a-map-varargs (apply interleave y)))
    {:k0 0 :k1 1 :k2 2 :k3 3 :k4 4} '((:k0 :k1 :k2 :k3 :k4) (0 1 2 3 4))
    {"k0" "0" "k1" "1" "k2" "2" "k3" "3" "k4" "4"}
    '(("k0" "k1" "k2" "k3" "k4") ("0" "1" "2" "3" "4"))
    {:dec dec :inc inc :+ + :- -} (list '(:dec :inc :+ :-)
                                        (list dec inc + -))))

;; Test loops
(t/deftest test-prn-n->100-result
  (t/are [n] (nil?
              ;; Reduce amount of printing to screen
              (sut/suppress-io (prn-n->100 n)))
    1 3 5 7 13 29 31 40 50 98 99))

(t/deftest test-prn-n->100-side-effects
  (t/are [n] (= (with-out-str (doseq [i (range n 100)] (prn i)))
                (with-out-str (prn-n->100 n))
                (with-out-str (prn-&-return-n->100 n)))
    1 3 5 7 13 29 31 40 50 98 99))

(t/deftest test-prn-&-n->100-result
  (t/are [n] (= (for [i (range n 100)] i)
                ;; Reduce amount of printing to screen
                (sut/suppress-io (prn-&-return-n->100 n)))
    1 3 5 7 13 29 31 40 50 98 99))

(t/deftest test-prn-n->0-result
  (t/are [n] (nil?
              ;; Reduce amount of printing to screen
              (sut/suppress-io (prn-n->0 n)))
    0 1 2 3 8 23 24 32 41 88 88))

(t/deftest test-prn-n->0-side-effects
  (t/are [n] (= (with-out-str (doseq [i (reverse (range 1 (inc n)))] (prn i)))
                (with-out-str (prn-n->0 n))
                (with-out-str (prn-&-return-n->0 n)))
    1 3 5 7 13 29 31 40 50 98 99))

(t/deftest test-prn-&-n->0-result
  (t/are [n] (= (for [i (reverse (range 1 (inc n)))] i)
                ;; Reduce amount of printing to screen
                (sut/suppress-io (prn-&-return-n->0 n)))
    0 1 2 3 8 23 24 32 41 88 88))

(t/deftest test-nested-loop-summation-result
  (t/are [outer middle inner] (= (reduce +
                                         (for [i (range outer)
                                               j (range middle)
                                               k (range inner)]
                                           (+ i j k)))
                                 (nested-for-summation outer middle inner)
                                 (nested-doseq-summation outer middle inner))
    1 1 1
    1 1 2
    1 1 3
    1 1 4
    1 1 5
    1 2 1
    1 2 2
    1 2 3
    1 2 4
    1 2 5
    1 3 1
    1 3 2
    1 3 3
    1 3 4
    1 3 5
    1 4 1
    1 4 2
    1 4 3
    1 4 4
    1 4 5
    2 1 1
    2 1 2
    2 1 3
    2 1 4
    2 1 5
    2 2 1
    2 2 2
    2 2 3
    2 2 4
    2 2 5
    2 3 1
    2 3 2
    2 3 3
    2 3 4
    2 3 5
    2 4 1
    2 4 2
    2 4 3
    2 4 4
    2 4 5))

(t/deftest test-nested-doseq-summation-result
  (t/are [inner] (do (prn inner)
                   (= (apply + (range inner))
                    (nested-doseq-summation 1 1 inner)
                    (nested-doseq-summation 1 inner)
                    (nested-doseq-summation inner)))
    1 2 3 4 5 6 7 8 9 10 11 12 13 14 15 16 17 18 19 20 21 22 23 24 25
    26 27 28 29 30 31 32 33 34 35 36 37 38 39 40 41 42 43 44 45 46 47 48 49
    50 51 52 53 54 55 56 57 58 59 60 61 62 63 64 65 66 67 68 69 70 71 72 73 74))

(t/deftest test-break-and-continue-result
  (t/are [x] (= (break-and-continue x)
                x)
    0 1 2 3 4 5 6 7 8 9 10 11 12 13))

(t/deftest test-break-and-continue-side-effects
  (t/are [x] (= (with-out-str (break-and-continue x))
                "")
    0 1 2 3 4 5 6 7 8 9 10 11 12 13))

(t/deftest test-state-result
  (t/are [x] (= (apply state x)
                2)
    '(true)
    '(false)
    '(nil)
    '(1 2 3 4 5 6)
    '((7 8))
    '(:k)))

(t/deftest test-state-side-effects
  (let [gen-string (fn [x] (str "[(2 "
                                x
                                " true \"b\" :a) ({!a 0, !b 1, !c 2} "
                                "{!a true, !b false} "
                                "{!a \"a\", !b \"b\", !c \"c\"} "
                                "{!a :a, !b :b, !c :c} {!vargs "
                                x
                                "})]\n"))]
    (t/are [x] (= (with-out-str (apply state x))
                  (gen-string x))
      '(true)
      '(false)
      '(nil)
      '(1 2 3 4 5 6)
      '((7 8))
      '(:k))))

(t/deftest test-closures-result
  (t/are [x] (= [(+ x 15) x]
                (closures x))
    0 1 2 3 4 5 6 7 8 9 10 11
    300 301 302 303 304 305 306 307))

(t/deftest test-closures-side-effects
  (let [function-tag? (fn [s] (and ;; Simple approximation at parsing out clj
                                   ;; evaluated anonymous function tags
                               (str/starts-with? s "#function[")
                               (str/includes? s "eval")
                               (str/includes? s "fn--")
                               (str/ends-with? s "]")))
        function-kv? (fn [s k-str] (let [[k v] (str/split s #"[ ,]")]
                                     (and (= k k-str)
                                          (function-tag? v))))
        f0-valid? (fn [s x]
                    (let [[lbracket stack _ sym-tbls rbracket]
                          (str/split s #"[\(\)]")
                          stack (read-string stack)]
                          (and (= lbracket "[")
                               (= stack [(+ 15 x) x])
                               (= sym-tbls
                                  (str "{!y " (+ 15 x) "} {} {!x " (str x)
                                       "}"))
                               (= rbracket "]"))))
        f1-valid? (fn [s x]
                    (let [[lbracket stack _ sym-tbls rbracket]
                          (str/split s #"[\(\)]")
                          stack (read-string stack)
                          sym-tbls (remove str/blank?
                                           (str/split sym-tbls #"[\{\}]"))
                          ]
                          (and (= lbracket "[")
                               (= stack [(+ 15 x) x])
                               (= (count sym-tbls) 3)
                               (= (first sym-tbls) (str "!z " (+ 15 x)))
                               (function-kv? (second sym-tbls) "!f0")
                               (= (nth sym-tbls 2) (str "!x " x))
                               (= rbracket "]"))))
        f2-valid? (fn [s x]
                    (let [[lbracket stack _ sym-tbls rbracket]
                          (str/split s #"[\(\)]")
                          stack (read-string stack)
                          sym-tbls (remove str/blank?
                                           (str/split sym-tbls #"[\{\}]"))
                          ]
                          (and (= lbracket "[")
                               (= stack [(+ 15 x) x])
                               (= (count sym-tbls) 3)
                               (= (first sym-tbls) (str "!a " 15))
                               (map function-kv?
                                    (str/split (second sym-tbls) #",")
                                    '("!f0" "!f1"))
                               (= (nth sym-tbls 2) (str "!x " x))
                               (= rbracket "]"))))
        fv-valid? (fn [s x]
                    (let [[lbracket stack _ sym-tbls0 sym-tbls1 sym-tbls2
                           rbracket]
                          (str/split s #"[\(\)]")
                          stack (read-string stack)
                          sym-tbls (remove str/blank?
                                           (str/split (str sym-tbls0
                                                           sym-tbls1
                                                           sym-tbls2)
                                                      #"[\{\}]"))]
                          (and (= lbracket "[")
                               (= stack [(+ 15 x) x])
                               (= (count sym-tbls) 3)
                               (= (first sym-tbls) "!args 5 4 3 2 1")
                               (map function-kv?
                                    (str/split (second sym-tbls) #",")
                                    '("!f0" "!f1" "!f2"))
                               (= (nth sym-tbls 2) (str "!x " x))
                               (= rbracket "]"))))
        valid? (fn [strs x]
                 (let [results (map
                                #(%2 %1 x)
                                strs
                                [f0-valid? f1-valid? f2-valid? fv-valid?])]
                   (every? true? results)))]
    (t/are [x] (valid? (str/split-lines (with-out-str (closures x))) x)
      0 1 2 3 4 5 6 7 8 9 10 11
      300 301 302 303 304 305 306 307)))

(t/deftest test-tic-tac-toe-result
  (t/are [n] (nil?
              (binding [*in* (new java.io.BufferedReader
                                  (new java.io.StringReader (str n "\nq\n")))]
                  (tic-tac-toe)))
    0 1 2 3 4 5 6 7 8 \a \b \c \d "e" "f" "g" true false :k))

(t/deftest test-tic-tac-toe-side-effects-0-turns
  (let [comp-str (str/join
                  "\n"
                  ["Welcome to Tic-Tac-Toe!"
                   "Input a number (0 1 2 3 4 5 6 7 8) for your next move: "
                   "         [0 1 2]"
                   "         [3 4 5]"
                   "         [6 7 8]"
                   ""])]
    (t/are [x]
        (= comp-str
           (with-out-str
             (binding [*in* (new java.io.BufferedReader
                                 (new java.io.StringReader (str x "\nq\n")))]
               (tic-tac-toe))))
           \a \b \c \d "e" "f" "g" true false :k)))

(t/deftest test-tic-tac-toe-side-effects-1-turn
  (t/are [x]
        (let [prefix (str/join
                "\n"
                ["Welcome to Tic-Tac-Toe!"
                 "Input a number (0 1 2 3 4 5 6 7 8) for your next move: "
                 "         [0 1 2]"
                 "         [3 4 5]"
                 "         [6 7 8]"
                 ""])
              split-intro-ai-move (fn [s] (str/split s #"AI's move: "))
              side-effect
              (with-out-str
                (binding [*in*
                          (new java.io.BufferedReader
                               (new java.io.StringReader (str x "\nq\n")))]
                  (tic-tac-toe)))
              [intro ai-move-&-prompt] (split-intro-ai-move side-effect)
              ai-move-&-prompt-lines (rest (str/split-lines ai-move-&-prompt))
              post-ai-gameboard
              (map-indexed vector
                           (flatten (map read-string
                                         (take 3 ai-move-&-prompt-lines))))
              post-ai-gameboard-second-print
              (map-indexed vector
                           (flatten (map read-string
                                         (drop 4 ai-move-&-prompt-lines))))
              xs (filter (comp (partial = "x") str second) post-ai-gameboard)
              os (filter (comp (partial = 'o) second) post-ai-gameboard)
              o (first (first os))
              min-placed (min o x)
              max-placed (max o x)
              ;; List of available spaces for user's next turn
              free-list (into (into (range (inc max-placed) 9)
                                    (reverse (range (inc min-placed)
                                                    max-placed)))
                              (reverse (range min-placed)))
              prompt-free-list (-> ai-move-&-prompt-lines
                                   (nth 3)
                                   (str/split #"Input a number ")
                                   second
                                   (str/split #" for your next move: ")
                                   first
                                   read-string)]
          (and (= intro prefix)
               ;; Check that both times the updated gameboard is print, the
               ;; output is identical
               (= post-ai-gameboard post-ai-gameboard-second-print)
               (= (count xs) (count os) 1)
               ;; Check that the "x" is actually placed in the correct
               ;; location
               (= (first (first xs)) x)
               ;; Check that the "o" is placed in bounds in a location not yet
               ;; occupied (i.e. not where "x" is)
               (not= o x)
               (>= o 0)
               (<= o 8)
               ;; Check that the user is prompted for the right list of
               ;; available spaces
               (= free-list (map first (filter (comp number? second)
                                               post-ai-gameboard))
                  prompt-free-list)
               ))
           0 1 2 3 4 5 6 7 8))

(defn run [& args]
  (t/run-tests 'dsl.core-test))
