(ns dsl.core
  (:require [clojure.string :as str]
            [clojure.core.reducers :as r]))

;; TODO: Throw compiler error on wrong number of args to invoke> prior to
;; evaluation

;; Stack predicates
(defn stack-var?
  "True iff v is a var"
  [v]
  (and (symbol? v)
       (identical? (first (name v)) \!)))

(defn var-assignment?
  "True iff v is a var to be assigned the top of stack value"
  [v]
  (and (stack-var? v)
       (identical? (last (name v)) \+)))

(defn stack-invokable?
  "True iff op is invokable and arity is a non-negative integer"
  [op arity]
  (and (ifn? op)
       (int? arity)
       (>= arity 0)))

(defn constant?
  "True iff c is a valid stack constant"
  [c]
  ((some-fn number? string? keyword? boolean?) c))

(defn correct-stackfn-syntax?
  "True iff name and args conform to correct syntax for defstackfn declaration"
  [name args]
  (and (symbol? name)
       (vector? args)
       (every? stack-var? args)))

;; Stack helper functions
(defn assignment-var-to-reg
  "Strips + from end of assignment var symbol"
  [sym]
  (symbol (apply str (drop-last (name sym)))))

(defn get-first-matching-sym-tbl
  "Get first sym-tbl that contains var"
  [sym-tbls var]
  (reduce #(if (and (not (empty? %2))
                    (contains? %2 var))
             (reduced %2)) nil sym-tbls))

(defn validate-stack-fn-args
  "Helper method for defstackfn, expected to be only called inside defstackfn,
   defined globally for accessibility in testing ns"
  [name args body]
  (if-not (correct-stackfn-syntax? name args)
    (throw
     ;; correct-stackfn-syntax? only checks three things, so we either produce
     ;; an error indicating the wrong type for the name, for the args, or for
     ;; the syntax of the arguments
     (IllegalArgumentException.
      (if-not (symbol? name)
        (str "defstackfn expects its first argument to be a function name "
             "specified as a symbol,\n"
             "but instead defstackfn received " (type name))
        (if-not (vector? args)
          (str "defstackfn expects its second argument to be a vector of "
               "parameters that the\n"
               "declared function expects but instead "
               "defstackfn received " (type args))
          ;; Report each var that contains a syntax error
          (let [bad-vars (filter (complement stack-var?) args)]
            (str "defstackfn expects each variable to be prefixed with '!' "
                 "but received the following\n"
                 "variables that do not start with '!':\n\t"
                 (str/join ", " (map str bad-vars))))))))))

(defn pop-n
  "Repeat pop n times (returning the resulting stack)"
  [n stack]
  (if (or (zero? n)
          (empty? stack))
    stack
    (recur (dec n) (pop stack))))

(defn wrap-loop-keywords
  "Wrap break and continue as (<push-constant> break) and
  (<push-constant> continue). Performs basic dead code elimination by removing
  code after continue or break, within a branch. Since if> shouldn't allow
  break or continue outside a loop, this separate function is only invoked
  to parse an if>'s body prior to evaluation when an if> is parsed inside a
  loop."
  [exprs]
  (let [kw-s #{'continue 'break}
        reduce-f (fn [acc cur] (if (kw-s cur)
                                 (reduced (conj acc cur))
                                 (conj acc cur)))
        exprs
        (apply (partial reduce conj)
               (map (partial reduce reduce-f [])
                    (split-with (partial not= 'else>)
                                exprs)))]
    (for [expr exprs]
      (cond
        (kw-s expr) (list '<push-constant> (list `quote expr))
        (= 'if> (and (coll? expr)
                     (first expr))) (cons (first expr)
                                          (wrap-loop-keywords
                                           (rest expr)))
        :else expr))))

;; Stack functions
(defn <pop>
  "Pop top of stack, returning stack after pop"
  [[stack sym-tbls]]
  [(pop stack) sym-tbls])

(defn <push>
  "Push var's associated value onto stack, returning stack after push"
  [[stack sym-tbls] var]
  ;; Need to check that var is actually a member of a sym-tbl
  (let [sym-tbl (get-first-matching-sym-tbl sym-tbls var)]
    (if-not sym-tbl
      (throw
       (IllegalArgumentException.
        (str "Variable " var " undeclared")))
      [(conj stack (sym-tbl var)) sym-tbls])))

(defn <push-constant>
  "Push c onto stack, returning stack after push"
  [[stack sym-tbls] c] [(conj stack c) sym-tbls])

(defn <assign>
  "Assign a var the top value of the stack. If var has already been declared
   then update value, otherwise assoc var with value on localmost sym-tbl"
  [[stack sym-tbls] var]
  (let [sym-tbl (or (get-first-matching-sym-tbl sym-tbls var)
                    (first sym-tbls))
        sym-tbls-v (into [] sym-tbls)
        sym-tbl-idx (.indexOf sym-tbls-v sym-tbl)]
    [stack (into '()
                 (rseq (assoc-in sym-tbls-v [sym-tbl-idx
                                             (assignment-var-to-reg var)]
                                 (peek stack))))]))

(defn <prn-state>
  "Print stack and symbol tables"
  [stack+sym-tbls]
  (do
    (prn stack+sym-tbls)
    stack+sym-tbls))

(defn <dup>
  "Push top of stack onto top of stack:
   (x0 x1 x2 ... xn) -> (x0 x0 x1 x2 ... xn)"
  [[stack sym-tbls]]
  [(conj stack (peek stack)) sym-tbls])

(defn <clear-stack>
  "Clear current stack"
  [[stack sym-tbls]]
  ['() sym-tbls])

(defn input>
  "Takes user input"
  [[stack sym-tbls]]
  (let [in (read-line)]
  [(conj stack in) sym-tbls]))

(defn invoke>
  "Invokes (apply op (take arity stack))"
  [[stack sym-tbls] op arity]
  (let [sym-tbl-match (if (stack-var? op)
                        (get-first-matching-sym-tbl sym-tbls op))
        sym-tbl-match (if sym-tbl-match (sym-tbl-match op))
        f (condp stack-invokable? arity
            sym-tbl-match (eval sym-tbl-match)
            op op
            (throw
             (IllegalArgumentException.
              (if-not (or (ifn? op)
                          (ifn? sym-tbl-match))
                (str "invoke> expects its first argument to be invokable but "
                     "received type " (type op))
                (str "invoke> expects its second argument to be an integer >= "
                     "0 but received {type: " (type arity) ", value: " arity
                     "}")))))]

     (if (> arity (count stack))
       (throw (IllegalArgumentException.
               (str "invoke> called with arity " arity " despite stack only "
                    "containing " (count stack) " elements")))
       (let [args            (take arity stack)
             remaining-stack (pop-n arity stack)]
         [(conj remaining-stack (apply f args)) sym-tbls]))))

(declare .var> .static> local if> .> <do until> local <do until> while>
         foreach> doseq> stackfn)

;; Sets used as (membership) predicate functions
(def invokable-stack-syms
  (let [vars '(invoke> <pop> if> .> <prn-state> local <do until>
                       while> foreach> doseq> <dup> <clear-stack> stackfn
                       input> <push-constant> .static> .var>)]
    (zipmap vars (map (comp symbol resolve) vars))))

(def syntactic-keywords #{'else>
                          'break
                          'continue})

;; Helper macros
;; Private macro taken from clojure.core
(defmacro ^{:private true} assert-args
  [& pairs]
  `(do (when-not ~(first pairs)
         (throw (IllegalArgumentException.
                 (str (first ~'&form) " requires " ~(second pairs) " in "
                      ~'*ns* ":" (:line (meta ~'&form))))))
     ~(let [more (nnext pairs)]
        (when more
          (list* `assert-args more)))))

(defmacro lquote
  "Helper macro to keep a list quoted. Useful for preventing things like
   ['(1 2 3 4) '({})] -> [(1 2 3 4) ({})] which can cause clojure to try to
   evaluate the list as a function."
  [quoted]
  `(list 'quote ~quoted))

(defmacro construct-eval
  "Helper macro to construct a list to be evaluated post-macro expansion"
  [expr stack+sym-tbl]
  `(eval (list ~expr (lquote ~stack+sym-tbl))))

(defmacro construct-eval->
  "Helper macro to construct a list to be evaluated post-macro expansion"
  [expr stack+sym-tbl]
  `(eval (list '-> (lquote ~stack+sym-tbl) ~expr)))

(defmacro do-until-continue-or-break
  "Execute each expr in body, sequentially, until either all exprs are
   executed or an expr returns 'continue' or 'break'"
  [stack+sym-tbls & body]
  `(let [continue-or-break-s# '~#{'continue 'break}
         [cur-expr# & rem-exprs#] '~body]
     (loop [stack+sym-tbls# ~stack+sym-tbls
            cur-expr# (first '~body)
            rem-exprs# rem-exprs#]
       (let [cur-expr# (if (and (coll? cur-expr#)
                                (= `if> (first cur-expr#)))
                         (cons (first cur-expr#)
                              (wrap-loop-keywords (rest cur-expr#)))
                         cur-expr#)]
       (cond
         (= cur-expr# '~'continue) stack+sym-tbls#
         (= cur-expr# '~'break) (<push-constant> stack+sym-tbls# '~'break)
         (empty? rem-exprs#) (if (symbol? cur-expr#)
                               (construct-eval cur-expr# stack+sym-tbls#)
                               (construct-eval-> cur-expr# stack+sym-tbls#))
         :else (recur
                (if (symbol? cur-expr#)
                  (construct-eval cur-expr# stack+sym-tbls#)
                  (construct-eval-> cur-expr# stack+sym-tbls#))
                (first rem-exprs#)
                (rest rem-exprs#)))))))

(defmacro parse-stack-exprs*
  "Takes a body of expressions and reformats them to a format that can be
   parsed and interpreted by a stack fn"
  [in-local? stack+sym-tbl body]
  ;; Allow for empty stackfns or stackfns with single nil expr
  (if-not
      (nil? body)
    (let [valid-options
          (str
           "(declared) variable (including arguments) e.g. !a\n"
           "assignment to a new or predeclared variable e.g. !a+\n"
           "a constant (numeric, string, keyword, or boolean) e.g. 7\n"
           "one of the following forms: \n\t"
           (str/join ", " (keys invokable-stack-syms)))
          err-msg
          (str "A valid stack function expects each expr to be one of:\n"
               valid-options
               "\nbut received ")]
      `(let [stack+sym-tbls# ~stack+sym-tbl]
         (map (fn [arg#]
                (let [fn-name# (if (coll? arg#)
                                (try (name (first arg#))
                                                  (catch Exception e# e#)))]
                (cond
                  (var-assignment? arg#) `(<assign> '~arg#)
                  (stack-var? arg#) `(<push> '~arg#)
                  (constant? arg#) `(<push-constant> ~arg#)
                  (and (= fn-name# (name 'invoke>))
                       (stack-var? (second arg#)))
                  `(invoke> '~(second arg#)
                            (nth '~arg# 2))
                  (= fn-name# (name 'stackfn))
                  (if ~in-local?
                    `(stackfn* '~(second stack+sym-tbls#) ~@(rest arg#))
                    `(push-stackfn ~@(rest arg#)))

                  (invokable-stack-syms
                   (if (coll? arg#)
                     (first arg#) arg#))
                  (if (coll? arg#)
                    (apply list (invokable-stack-syms (first arg#))
                           (rest arg#))
                    (invokable-stack-syms arg#))
                  (syntactic-keywords arg#) arg#
                  :else (throw (IllegalArgumentException.
                                (str ~err-msg (if (nil? arg#)
                                                "nil"
                                                arg#)))))))
                 ~body)))))

(defmacro parse-stack-exprs
  [body]
  `(parse-stack-exprs* false ['() '({})] ~body))

(defmacro quote-local-bindings
  "Quote locally defined variables prior to evaluation to prevent issues
   with local lexical scope being lost inside context of
   (loop ... (recur ...)). invoke> is set up to check symbol tables for a
   match, but invoke> is a function and so its arguments are evaluated prior
   to being passed to it, hence quoting local vars."
  [stack+sym-tbls body]
  `(map (fn [arg#]
          (if (and
               (coll? arg#)
               (= (first arg#) `invoke>)
               (not (empty? (get-first-matching-sym-tbl
                             (second ~stack+sym-tbls)
                             (second arg#)))))
            (list `invoke> ((get-first-matching-sym-tbl
                             (second ~stack+sym-tbls)
                             (second arg#))
                            (second arg#)) (nth arg# 2))
            arg#))
        '~body))

(defmacro suppress-io
  "Suppress input and output when executing body"
  [& body]
  `(let [os# (new java.io.StringWriter)
         is# (new java.io.BufferedReader (new java.io.StringReader ""))]
     (binding [*out* os#
               *in* is#]
       ~@body)))

(defmacro sym-tbl-resolve-s-expr
  "Takes an s-expression and replaces any symbols found in sym-tbls with
   their associated value"
  [sym-tbls s-expr]
  `(let [replace-val-if-found# (fn [e#]
                                 (if-let [sym-tbl#
                                          (get-first-matching-sym-tbl ~sym-tbls
                                                                      e#)]
                                   (sym-tbl# e#)
                                   e#))]
     (let [res# (if (coll? '~s-expr)
                  (map replace-val-if-found#
                       '~s-expr)
                  (replace-val-if-found# '~s-expr))]
       ;; Try to evaluate resultant form, if not, return the quoted form
       (try (eval res#)
            (catch Exception e#
              ;; If every element of collection is a valid constant then this
              ;; is also fine, but if any elements are not, return string to
              ;; be rethrown
              (if-not (every? (some-fn constant? stack-var?) res#)
                (str '~s-expr)
                res#))))))

(defmacro coll-loop
  "Generic helper macro for common functionality between foreach> and doseq>"
  [loop-name keep-stack? stack+sym-tbls coll & body]
  (let [coll `(sym-tbl-resolve-s-expr (second ~stack+sym-tbls) ~coll)
        coll `(suppress-io ~coll)]
    (let [check-is-coll
          `(if-not
               (coll? ~coll)
             ;; Note that while the first argument is
             ;; loop-name, this is abstracted away from the
             ;; end user and so coll is considered
             ;; the first argument for error reporting
             (do (prn "type of thrown 'coll': " (type ~coll))
               (throw (IllegalArgumentException.
                     ;; Try to provide better error messages
                     ;; for understandable syntax errors
                     (if (and (string? ~coll)
                              (some-> ~coll
                                      (str/split #"\(")
                                      second
                                      (str/split #" ")
                                      first
                                      symbol
                                      invokable-stack-syms))
                       (str (-> ~coll
                                (str/split #"\(")
                                second
                                (str/split #" ")
                                first)
                            " cannot be used as the first "
                            "argument to " '~loop-name ". The "
                            "first argument needs to either be "
                            "a collection of constant literals "
                            "(e.g. [1 2 3 4 5])\nor a Clojure s-expression "
                            "that evaluates to a collection (e.g. (range 1 5). "
                            "Predefined variables are also valid in context "
                            "of either of the two aforementioned forms\n(e.g. "
                            "(range !n !m) => (range 3 5)).\n")
                       (str '~loop-name
                            " expects its first argument to be a collection "
                            "but received " ~coll))))))
          cur `(first ~coll)
          rem `(rest ~coll)
          stack+sym-tbls `(update ~stack+sym-tbls 0 (fn [x#] (conj x# ~cur)))
          parsed-exprs (parse-stack-exprs body)
          f (if keep-stack? identity (fn [x]
                                       (if (#{'continue 'break}
                                            (peek x))
                                         x
                                         (pop x))))]
        ;; Ensure that pred can be invoked on top of stack or sym-tbl
      `(do ~check-is-coll
           (loop [stack+sym-tbls# ~stack+sym-tbls
                  rem# ~rem]
           ;; Base cases are that either
           ;; collection is exhausted or
           ;; top-of-stack = break
           (let [cur-top# ;; Since we push the next element of coll onto the
                          ;; stack during recur, if break or continue are on
                          ;; the stack, they're the second element
                 (second (first stack+sym-tbls#))
                 stack+sym-tbls# (if (= cur-top# '~'continue)
                                   (let [old-top# (peek (first stack+sym-tbls#))]
                                     (update stack+sym-tbls#
                                             0 (comp (fn [x#]
                                                       (conj x# old-top#))
                                                     pop pop)))
                                   stack+sym-tbls#)]
             (cond
               (= cur-top# '~'break) (<pop> (<pop> stack+sym-tbls#))
               (empty? rem#)
               (update (do-until-continue-or-break stack+sym-tbls#
                                                   ~@parsed-exprs)
                       0 (comp (fn [x#]
                                 (if (#{'~'continue '~'break}
                                      (peek x#))
                                   (pop x#)
                                   x#)) ~f))
               :else (recur
                      (update
                       (do-until-continue-or-break
                        stack+sym-tbls#
                        ~@parsed-exprs)
                       0 #(conj (~f %) (first rem#)))
                      (rest rem#)))))))))

(defmacro non-coll-loop
  "Generic helper macro for common functionality between while> and until>"
  [loop-name pred-true? stack+sym-tbls maybe-top+pred & body]
  (let [check-isnt-stackfn `(if (and
                                 (list? ~maybe-top+pred)
                                 (= (some-> ~maybe-top+pred
                                            str
                                            (str/split #"\(")
                                            second
                                            (str/split #" ")
                                            first)
                                    "stackfn"))
                              (throw (IllegalArgumentException.
                                      ;; Try to provide better error messages
                                      ;; for understandable syntax errors
                                      (str "stackfns cannot be used as "
                                           "the predicate function for "
                                           '~loop-name ". The predicate "
                                           "function must either be a "
                                           "Clojure function (e.g. zero?) "
                                           "or a predefined stack variable "
                                           "(e.g. !v)."))))
        pred `(if (vector? ~maybe-top+pred)
                (second ~maybe-top+pred)
                ~maybe-top+pred)
        stack+sym-tbls `(if (vector? ~maybe-top+pred)
                          (update ~stack+sym-tbls 0
                                  (fn [x#]
                                    (conj x# (first ~maybe-top+pred))))
                          ~stack+sym-tbls)
        pred-is-var? `(stack-var? ~pred)
        parsed-exprs (parse-stack-exprs body)
        pred-test-fn (if pred-true? not identity)]
    ;; Ensure that pred can be invoked on top of stack or sym-tbl
    `(do ~check-isnt-stackfn
         (if-not ((some-fn ifn? stack-var?) ~pred)
           ;; Note that while the first argument is stack+sym-tbl, this is
           ;; abstracted away from the end user and so maybe-top+pred is considered
           ;; the first argument for error reporting
           (throw (IllegalArgumentException.
                   (str '~loop-name
                        " expects its first argument to be one of:\n"
                        "a predicate function or a predefined variable\n"
                        "a 2 element vector, where the first element is the new "
                        "top of the stack and the second element is a predicate "
                        "function, or a predefined variable\n"
                        "but received " ~maybe-top+pred)))
           (loop [stack+sym-tbls# ~stack+sym-tbls]
             ;; Base cases are that either
             ;; (pred top-of-stack) = false or
             ;; top-of-stack = break
             (let [stack+sym-tbls# (if (= (peek (first stack+sym-tbls#))
                                          '~'continue)
                                     (update stack+sym-tbls# 0 pop)
                                     stack+sym-tbls#)
                   cur-top# (peek (first stack+sym-tbls#))]
               (cond
                 (= cur-top# '~'break) (<pop> stack+sym-tbls#)
                 (~pred-test-fn
                  (if ~pred-is-var?
                    ;; By allowing for variables to be used as predicate functions
                    ;; we need to keep the predicate a symbol until we know for
                    ;; sure it refers to an actual function and not a variable
                    ;; name
                    (get-first-matching-sym-tbl (second stack+sym-tbls#)
                                                '~pred)
                    (~pred cur-top#)))
                 stack+sym-tbls#
                 :else (recur
                        (do-until-continue-or-break
                         stack+sym-tbls#
                         ~@parsed-exprs)))))))))

(defmacro throw-on-stackfn
  "Throw exception when stackfn declaration used where a regular function is
   expected. Useful for giving user more helpful error messages on invalid
   syntax."
  [loop-name maybe-top+pred]
  (if (and
       (list? maybe-top+pred)
       (= (some-> maybe-top+pred
                  str
                  (str/split #"\(")
                  second
                  (str/split #" ")
                  first)
          "stackfn"))
    (throw (IllegalArgumentException.
            ;; Try to provide better error messages
            ;; for understandable syntax errors
            (str "stackfns cannot be used as "
                 "the predicate function for "
                 loop-name ". The predicate "
                 "function must either be a "
                 "Clojure function (e.g. zero?)\n"
                 "or a predefined stack variable "
                 "(e.g. !v).")))
    maybe-top+pred))

;; Stack macros (exposed to end user)

;;TODO: Need macro that call stackfn*, pushes result on stack and returns
;; stack+sym-tbl

(defmacro push-stackfn
  "Creates new stackfn with initial symtbls appended, then pushes this on
   stack, with updated stack+sym-tbls returned"
  [stack+sym-tbls & fdecl]
  `(let [[stack# sym-tbls#] ~stack+sym-tbls]
     [(conj stack# (stackfn* sym-tbls# ~@fdecl)) sym-tbls#]))

(defmacro stackfn*
  "Returns new stackfn with initial symtbls appended"
  [sym-tbls & fdecl]
  (let [[prefix fdecl] (if (symbol? (first fdecl))
                         [`(fn ~(first fdecl)) (next fdecl)]
                         [`(fn) fdecl])]
  `(~@prefix
    ~@(for [cur-decl (if (vector? (first fdecl))
                       (list fdecl)
                       fdecl)]
         (let [[args & body] cur-decl]
           (let [local-stack (list)
                 ;; Check that args are members of a vector
                 type-check-arglist (if-not (vector? args)
                                      (throw (IllegalArgumentException.
                                              (str
                                               `stackfn
                                               " requires parameter "
                                               "declaration be a vector"))))
                 ;; Filter '&' symbol here
                 args-no-& (filterv (partial not= '&) args)
                 ;; Check that args are lexically valid
                 bad-args (remove stack-var? args-no-&)
                 throw-on-bad (if-not (empty? bad-args)
                                (throw (IllegalArgumentException.
                                        (str `stackfn " requires "
                                             "that all parameter names start "
                                             "with '!', but the following was "
                                             "given as parameter name(s): "
                                             (str/join ", "
                                                       (map str bad-args))))))
                 sym-tbls `(conj ~sym-tbls
                                 (zipmap '~args-no-& [~@args-no-&]))
                 var-fns (parse-stack-exprs body)]
             `(~args
               (let [sym-tbls# ~sym-tbls]
                 (-> [~local-stack ~sym-tbls]
                     ~@var-fns
                     first
                     peek)))))))))

(defmacro stackfn
  "Returns new stackfn"
  [& fdecl]
  ;; Need to check for issues with single-arity definition, otherwise user
  ;; will receive cryptic error message
  (let [type-check-single-arity (assert-args
                                 (or (or (vector? (first fdecl))
                                         (list? (first fdecl)))
                                     (and (> (count fdecl) 1)
                                          (symbol? (first fdecl))
                                          (or (vector? (second fdecl))
                                              (list? (second fdecl)))))
                                 (str
                                  "first argument to a single-arity stackfn "
                                  "declaration be a vector of "
                                  "parameters or a function name, specified "
                                  "as a symbol,\nfollowed by a vector of "
                                  "parameters"))]
    `(stackfn* '() ~@fdecl)))

(defmacro defstackfn
  "Define new stack function globally"
  [name & fdecl]
  (list `def name
        `(stackfn ~@fdecl)))

(defmacro local
  "Define a local scope inside a stackfn"
  [stack+sym-tbls bindings & body]
  (assert-args
   (vector? bindings) "a vector for its binding"
   (even? (count bindings)) "an even number of forms in binding vector"
   (every? stack-var? (flatten (take-nth 2 bindings)))
   "local variables to start with '!'")
  (let [stack+sym-tbls `(update ~stack+sym-tbls 1 (fn [sym-tbls#]
                                                    (conj sym-tbls# {})))
        eager-loop `(loop [stack+sym-tbls# ~stack+sym-tbls
                           [[sym# value#] & rem-pairs#]
                           '~(partition 2 bindings)]
                      (let [binding# {(list `quote sym#)
                                      (cond
                                        ;; Case: value is stackfn
                                        (and (list? value#)
                                             (= (name (first value#))
                                                "stackfn"))
                                        (let [[head# & rem#] (next value#)
                                              named?# (symbol? head#)
                                              multi-arity?# (list?
                                                             (if named?#
                                                               (first rem#)
                                                               head#))]
                                          ;; 4 cases based on all combinations
                                          ;; of named/not-named and
                                          ;; single/multi arity
                                          (cond
                                            (and
                                             named?#
                                             multi-arity?#)
                                            (cons `stackfn*
                                                  (cons
                                                   (list
                                                     `quote
                                                     (second stack+sym-tbls#))
                                                   (cons head#
                                                         rem#)))
                                            (and
                                             named?#
                                             (not multi-arity?#))
                                            (cons `stackfn*
                                                  (cons
                                                   (list
                                                     `quote
                                                     (second stack+sym-tbls#))
                                                   (list head#
                                                         rem#)))
                                            (and
                                             (not named?#)
                                             multi-arity?#)
                                            (cons `stackfn*
                                                  (cons
                                                   (list
                                                    `quote
                                                    (second stack+sym-tbls#))
                                                   (next value#)))
                                            (and
                                             (not named?#)
                                             (not multi-arity?#))
                                            (list `stackfn*
                                                  (list `quote
                                                        (second stack+sym-tbls#))
                                                     (next value#))))
                                        (constant? value#) value#
                                        :else
                                        (throw
                                         (IllegalArgumentException.
                                          (str "Local binding must bind a "
                                               "(possibly destructured) var "
                                               "to a stackfn or a constant, but "
                                               "instead bound to " value#))))}
                            stack+sym-tbls# [(first stack+sym-tbls#)
                                             (conj
                                              (pop (second stack+sym-tbls#))
                                              (conj
                                               (peek (second stack+sym-tbls#))
                                               (eval binding#)))]]
                        (if (empty? rem-pairs#)
                          stack+sym-tbls#
                          (recur stack+sym-tbls#
                                 rem-pairs#))))
        var-fns (parse-stack-exprs body)]
    `(-> ~eager-loop
          ~@var-fns
          (update 1 pop))))

(defmacro if>
  "Pops top of stack;
   if top is falsey => branch to else>
   otherwise continue normal execution until else>, at which point jump to end
   of s-expression"
  [stack+sym-tbl & exprs]
  (assert-args
   ;; Ensure there is either 0 or 1 else> branches
   (< (count (filter (partial = 'else>) exprs)) 2)
   "exactly 0 or 1 else> branches")
  (let [not-else>? #(not= % 'else>)]
      `(let [stack+sym-tbl# ~stack+sym-tbl
             res# (first (first stack+sym-tbl#))]
         (if res#
           (-> stack+sym-tbl# <pop>
                ~@(parse-stack-exprs (take-while not-else>?
                                                 exprs)))
            (-> stack+sym-tbl# <pop>
                ~@(parse-stack-exprs (rest
                                      (drop-while not-else>?
                                                  exprs))))))))
(defn lookup-args
  "Replace stack vars with their respective symbol tables values,
   if applicable"
  [sym-tbls args]
  (let [args (map (fn [x] (if-let [sym-tbl
                                       (get-first-matching-sym-tbl
                                        sym-tbls x)]
                              (sym-tbl x) x)) args)]
    args))

(defmacro .static>
  "Invoke static virtual method (from inside stack function body)"
  [stack+sym-tbls class-name method-name & args]
  `(update ~stack+sym-tbls 0
           (fn [stack#]
             (conj stack#
                   (eval (cons `. (cons '~class-name
                                        (cons '~method-name
                                              (lookup-args
                                               (second ~stack+sym-tbls)
                                               '~args)))))))))

(defn helper
  [obj method & args]
  (prn method)
  (. obj method))

(defmacro .>
  "Invoke virtual method (from inside stack function body). Note that
   mutations to stack elements will not mutate their associated sym-tbls vars,
   use .var> to mutate sym-tbls vars"
  [stack+sym-tbls method-name & args]
  `(update ~stack+sym-tbls 0
           (fn [stack#]
             (let [top# (peek stack#)]
               (conj stack#
                     (eval (cons `. (cons top#
                                    (cons '~method-name
                                          (lookup-args
                                           (second ~stack+sym-tbls)
                                           '~args))))))))))

(defmacro use-dot-then-return [obj method-name & args]
        `[(. ~obj ~method-name ~@args) ~obj])

(defmacro .var>
  "Invoke virtual method on var ∈ sym-tbls (from inside stack function body)"
  [stack+sym-tbls var method-name & args]
  `(let [[stack# sym-tbls#] ~stack+sym-tbls
         sym-tbl# (get-first-matching-sym-tbl
                                    (second ~stack+sym-tbls)
                                    '~var)
         sym-tbls-v# (into [] sym-tbls#)
         sym-tbl-idx#
         (first (keep-indexed (fn [idx# m#] (if (contains? m# '~var) idx#))
                                            sym-tbls-v#))
         obj# (sym-tbl# '~var)
         [res# obj#] (eval (cons `use-dot-then-return
                          (cons obj# (cons '~method-name
                                          (lookup-args
                                           (second ~stack+sym-tbls)
                                           '~args)))))]
     [(conj stack# res#) (into '()
                               (rseq (assoc-in sym-tbls-v# [sym-tbl-idx#
                                                            '~var]
                                               obj#)))]))


;; Loops
(defmacro foreach>
  "Executes the expressions in body until coll is exhausted. Each iteration
   the next element in the coll is pushed on the stack."
  [stack+sym-tbls coll & body]
  `(coll-loop foreach> true ~stack+sym-tbls ~coll ~@body))

(defmacro doseq>
  "Like foreach> but pops each element of collection from stack immediately
   after iteration in which element is used."
  [stack+sym-tbls coll & body]
  `(coll-loop doseq> false ~stack+sym-tbls ~coll ~@body))

(defmacro until>
  "Executes the expressions in body until (pred top-of-stack) is true.
   until> syntax comes in 2 forms:
     (until> pred body)
     (until> [new-top pred] body)
  The first form expects a predicate function as the first argument, while the
  second form expects a 2 element vector where the first member of the vector
  is the new top to be pushed onto the stack while the second member of the
  vector is the predicate function.

  An already defined variable !var-name may also be supplied as the predicate
  function, in which case the predicate function is essentially
    (or (nil? !var-name)
        (false? !var-name))

  ex:
   ;; Pushes 10 onto stack and then iterates 10 times
   (until> [10 zero?]
    (invoke> dec 1))
   ;; Squares top of stack until it's greater than 500
   (until> (fn [n] (> n 500))
    (invoke> (fn square [n] (* n n)) 1))"
  [stack+sym-tbls maybe-top+pred & body]
  `(non-coll-loop ~'until> false ~stack+sym-tbls
                  (throw-on-stackfn ~'until> ~maybe-top+pred) ~@body))

(defmacro while>
  "Like until> but executes body until (pred top-of-stack) is false (or nil)."
  [stack+sym-tbls maybe-top+pred & body]
  `(non-coll-loop ~'while> true ~stack+sym-tbls
                  (throw-on-stackfn ~'while> ~maybe-top+pred)
                                    ~@body))

(defmacro <do
  "Executes body of loop then checks test condition. Still pushes new top
   to stack, if applicable."
  [stack+sym-tbls & body]
  (let [[body [loop-type maybe-top+pred]] (split-at (- (count body) 2) body)
        [maybe-top pred] (if (vector? maybe-top+pred)
                           maybe-top+pred
                           [nil maybe-top+pred])
        stack+sym-tbls (if maybe-top
                         `[(conj (first ~stack+sym-tbls)
                                 (first ~maybe-top+pred))
                           (second ~stack+sym-tbls)]
                         stack+sym-tbls)
        parsed-exprs (parse-stack-exprs body)
        qualified-loop (invokable-stack-syms loop-type)]
    `(let [init-call-result# (-> ~stack+sym-tbls ~@parsed-exprs)]
       (-> init-call-result# (~qualified-loop ~pred ~@body)))))
