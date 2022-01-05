(ns geo.translate-midje
  (:import (java.io PushbackReader Reader BufferedReader StringReader))
  (:require [clojure.pprint :refer (pprint) :as pprint]
            [clojure.tools.namespace :as ns]
            [clojure.core.match :refer (match)]
            [clojure.string :as str]
            [clojure.zip :as zip]
            [bultitude.core :as bultitude]
            [clojure.java.io :refer [reader]]))

(def ^:dynamic *current-file* nil)
(def ^:dynamic *current-ns* nil)

;; Taken from
;; https://github.com/technomancy/swank-clojure/blob/master/src/swank/commands/basic.clj#L142
(defn destroy-ns
  [ns]
  (doseq [sym (keys (ns-refers ns))]
    (ns-unmap ns sym))
  (doseq [a (keys (ns-aliases ns))]
    (ns-unalias ns a))
  (doseq [a (keys (ns-publics ns))]
    (ns-unmap ns a)))
;;;;;;;;;;;;

(defn regex? [x]
  (instance? java.util.regex.Pattern x))

(defn list! [s]
  {:post [(list? %)]}
  (apply list s))

(def arrow-symbols #{'=> '=not=> '=deny=> '=contains=>})

(defn arrow? [[f1 f2 f3]]
  (contains? arrow-symbols f2))

(defn get-arrow [[f1 f2 f3 :as f]]
  (when (arrow? f)
    f2))

(defn translate-regex-arrow
  [[f1 f2 f3]]
  (match [f1 f2 f3]
         [actual '=> re] `(~'is (~'re-find ~re ~actual))
         [actual '=not=> re] `(~'is-not (~'re-find ~re ~actual))
         [actual '=deny=> re] `(~'is-not (~'re-find ~re ~actual))))

(defn translate-contains-arrow [[f1 f2 f3]]
  {:post [(do (assert % (str "failed: " f1 " " f2 " " f3)) true)]}
  (let [[_ contents & options] f3]
    (match [f1 f2 contents]
           [actual '=> (_ :guard string?)] `(~'is (~'=> ~f3 ~actual))
           [actual '=> (_ :guard map?)] `(~'is (~'submap? ~contents ~actual))
           [actual '=> (_ :guard regex?)] `(~'is (~'re-find ~contents ~actual))
           [actual '=not=> (_ :guard string?)] `(~'is-not (~'re-find ~(re-pattern contents) ~actual))
           [actual '=not=> (_ :guard regex?)] `(~'is-not (~'re-find ~contents ~actual))
           [actual arrow contents] `(~'is (~arrow ~f3 ~actual)))))

(defn form-is-fn?
  "Resolves a form, and returns truthy if it evaluates to a fn"
  [f]
  (when (symbol? f)
    (binding [clojure.core/*ns* (or *current-ns* clojure.core/*ns*)]
      (let [resolved (resolve f)]
        (and resolved (or (fn? resolved)
                          (and (var? resolved)
                               (fn? @resolved))))))))

(defn translate-normal-arrow [[f1 f2 f3]]
  (match [f1 f2 f3]
         [actual '=> true] `(~'is ~actual)
         [actual '=> 'truthy] `(~'is ~actual)
         [actual '=> 'anything] `(~'is (~'anything ~actual))
         [actual '=> false] `(~'is-not ~actual)
         [actual '=> 'falsey] `(~'is-not ~actual)
         [actual '=> (f :guard form-is-fn?)] `(~'is (~f ~actual))
         [actual '=> 0] `(~'is (~'zero? ~actual))
         [actual '=> nil] `(~'is (~'nil? ~actual))
         [actual '=> expected] `(~'is (~'= ~expected ~actual))
         [actual '=contains=> (expected :guard map?)] `(~'is (~'submap? ~expected ~actual))
         [actual '=not=> expected] `(~'is (~'not= ~expected ~actual))))

(defn translate-throws [[f1 f2 f3]]
  (let [exception-form (rest f3)]
    (condp = (count exception-form)
      0 `(~'is (~'thrown? Exception ~f1))
      1 `(~'is (~'thrown? ~@exception-form ~f1))
      2 `(~'is (~'thrown-with-msg? ~@exception-form ~f1))
      :else (assert false))))

(defn translate-arrow [[f1 f2 f3 :as f]]
  {:post [(do (assert % (str "failed: " f1 " " f2 " " f3)) true)]}
  (let [contains-form? (or (and (seq? f3) (= 'contains (first f3)))
                           (= '=contains=> f2))
        thrown? (and (list? f3)
                     (= 'throws (first f3)))]
    (cond
      (regex? f3) (translate-regex-arrow f)
      contains-form? (translate-contains-arrow f)
      thrown? (translate-throws f)
      :else (translate-normal-arrow f))))

(defn replace-arrow
    "If the head of the form is (foo) => (bar), return clojure.test
  equivalent. Returns either the updated form, or the original, if no
  match"
    [lst]
    (loop [ret []
           lst lst]
      (if (seq lst)
        (if (arrow? lst)
          (let [actual (first lst)
                expected (get lst 2)]
            (recur (conj ret (translate-arrow lst)) (drop 3 lst)))
          (recur (conj ret (first lst)) (rest lst)))
        (list! ret))))

(defn require-ns
  "Total hack to make sure the ns form is already evaluated, so we can resolve fns later"
  [form]
  (when (and (list? form)
             (= 'ns (first form)))
    (binding [clojure.core/*ns* clojure.core/*ns*]
      ;; don't let the ns form stomp on our current ns
      (eval form)))
  form)

(defn remove-expect [form]
  (if (and (list? form) (= 'expect (first form)))
    (do
      (assert (= 2 (count form)))
      (second form))
    form))

(defn replace-setup-test-dbs [form]
  (cond
    (contains? #{'(test/setup-test-dbs)
                 '(setup-test-dbs)
                 '(test/test-ns-setup)
                 '(test-ns-setup)} form) '(test/test-fixtures)
                 (and (list? form)
                      (= 'test/midje-fixtures (first form))) `(test/test-fixtures ~@(rest form))
                      (and (list? form)
                           (= 'wd/setup-tests (first form))) `(wd/webdriver-fixtures ~@(rest form))
                           (and (list? form)
                                (= 'webdriver/setup-tests (first form))) `(webdriver/webdriver-fixtures ~@(rest form))
                                :else form))

(defn munge-name [name]
  (-> name
      (str/replace " " "-")
      (str/replace "." "")
      (str/replace "`" "")
      (str/replace "'" "")
      (str/replace "(" "")
      (str/replace ")" "")
      (str/replace "/" "")
      (str/replace "," "-")
      (str/replace "[" "")
      (str/replace "]" "")
      (str/replace #"^(\d+)" "_$1")
      (str/replace #"^:" "")
      (str/replace #"-+" "-")))

(defn replace-fact
  "If the form is (fact ...) replace w/ (deftest ...)"
  [form & rest]
  (if (= 'fact (first form))
    (let [[fact name & body] form
          [prefix] rest]
      (list! `(~'deftest ~(symbol (munge-name (clojure.string/trim (str prefix " " name)))) ~@body)))
    form))

(defn replace-facts
  "If the form is (facts 'foo' (fact 'bar' ...)), replace with (deftest foo-bar ...).
  If the form is (facts 'foo' ...), replace with (fact 'foo' ...) to be picked up by later step"
  [form]
  (if (= 'facts (first form))
    (let [[fact name & body] form]
      (if (= 'fact (first (first body)))
        (map #(replace-fact % name) body)
        `(~'fact ~name ~@body)))
    form))

(defn replace-midje-sweet [form]
  (if (= 'midje.sweet form)
    'clojure.test
    form))

(defn replace-future-fact [form]
  (if (= 'future-fact (first form))
    `(~'comment ~(replace-fact `(~'fact ~@(rest form))))
    form))

(defn cast-coll
  "Convert one seq class into another. Use it to handle (into (empty list) '(1 2 3)) being dumb"
  [cls form]
  (condp = cls
    clojure.lang.PersistentList (list* form)
    clojure.lang.PersistentVector (vec form)))

(defn munge-form [form]
  (let [form (-> form
                 (replace-setup-test-dbs)
                 (replace-midje-sweet)
                 (require-ns))
        form-class (class form)]
    (cond
      (or (list? form)
          (vector? form)) (-> form
                              (replace-facts)
                              (replace-fact)
                              (replace-future-fact)
                              (replace-arrow)
                              (remove-expect)
                              (->> (map #(munge-form %))
                                   (cast-coll form-class)))
          :else form)))

(defn walk [form]
  (munge-form form))

(defn pprint-test [alis]
  (if (next alis)
    (let [[defn-sym defn-name & stuff] alis
          [doc-str stuff] (if (string? (first stuff))
                            [(first stuff) (next stuff)]
                            [nil stuff])
          [attr-map stuff] (if (map? (first stuff))
                             [(first stuff) (next stuff)]
                             [nil stuff])]
      (pprint/pprint-logical-block :prefix "(" :suffix ")"
                                   ((pprint/formatter-out "~w ~1I~@_~w") defn-sym defn-name)
                                   (if doc-str
                                     ((pprint/formatter-out " ~_~w") doc-str))
                                   (if attr-map
                                     ((pprint/formatter-out " ~_~w") attr-map))
                                   ;; Note: the multi-defn case will work OK for malformed defns too
                                   (cond
                                     (vector? (first stuff)) (#'pprint/single-defn stuff (or doc-str attr-map))
                                     :else (#'pprint/multi-defn stuff (or doc-str attr-map)))))
    (#'pprint/pprint-simple-code-list alis)))

(defn set-public! [sym]
  (def sym sym)
  )

(defn indent [forms]
  (with-out-str
    (binding [pprint/*code-table* (assoc (deref (var pprint/*code-table*))
                                         'with-redefs #'pprint/pprint-let
                                         'deftest pprint-test)]
      (doseq [f forms]
        (pprint/write f :dispatch pprint/code-dispatch)
        (println "\n")))))

(defn file! [f]
  (if (instance? java.io.File f)
    f
    (java.io.File. f)))

(defn file->ns [file]
  (first (bultitude/namespaces-in-dir (file! file))))

;; Taken from https://github.com/flatland/useful/blob/develop/src/flatland/useful/io.clj#L12
(defprotocol PushbackFactory
  (^{:added "1.4"} pushback-reader [x] "Creates a PushbackReader from an object."))

(extend-protocol PushbackFactory
  PushbackReader
  (pushback-reader [this]
    this)

  Reader
  (pushback-reader [this]
    (PushbackReader. this))

  Object
  (pushback-reader [this]
    (pushback-reader (reader this))))

(let [sentinel (Object.)
      valid? #(not (identical? % sentinel))]
  (defn read-seq
  "Read a lazy sequence of Clojure forms from an input reader."
  [str]
  (let [in (pushback-reader (BufferedReader. (StringReader. str)))]
    (take-while valid?
                (repeatedly #(read in false sentinel))))))
;;;;;;

(defn remove-facts
  "If the form is (facts 'foo' ..), just return the inner elements"
  [forms]
  (->> forms
       (mapcat (fn [form]
                 (if (= 'facts (first form))
                   (do
                     (let [[facts name & body] form]
                       body))
                   [form])))))

(defn transform [file]
  (let [ns (file->ns file)]
    (binding [*current-file* file
              *current-ns* ns]
      (-> file
          (slurp)
          (read-seq)
          (list!)
          (walk)
          (remove-facts)
          (indent)
          ))))

(defn do-rewrite [file]
  (-> file
      transform
      (->> (spit file))))

(defn successful-test-results? [results]
  (and (-> results :error zero?)
       (-> results :fail zero?)))

(defn test-rewrite [file]
  (let [ns (file->ns file)]
    (do-rewrite file)
    (destroy-ns ns)
    (require ns :reload)
    (let [results (clojure.test/test-ns ns)]
      (when (successful-test-results? results)
        (println "Success!" file)))))

(defn midje? [file]
  (or (re-find #"midje.sweet" (slurp file))
      (re-find #"\(fact" (slurp file))))

(defn rewrite-all []
  (let [files (->> (java.io.File. "test/")
                   (ns/find-clojure-sources-in-dir )
                   (filter midje?))]
    (doseq [f (take 1 files)]
      (try
        (test-rewrite f)
        (catch Throwable t
          (.printStackTrace t)
                    (println "FAILED:" f))))))
