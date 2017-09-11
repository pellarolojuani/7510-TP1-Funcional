(ns logical-interpreter)

(defn valid-fact-syntax
  "Comprueba si el fact syntax es valido"
  [query]
  (do
    (def result true)
    (if (= (clojure.string/blank? query) false) () (def result false))
    (if (= (clojure.string/index-of query "(") nil) (def result false) ())
    (if (= (clojure.string/index-of query ")") nil) (def result false) ())
    result
    )
  ) 

(defn get-facts-from-database
  "Obtiene un vector con todos los facts trimeados y sin punto"
  [database]
  (do
    (def the-answer (clojure.string/split-lines database))
    (def result4 [])
    (try
      (doseq [x the-answer]
        (def parsed_line (clojure.string/replace (clojure.string/trim x) #"\." ""))
        (if (.contains parsed_line ":-")
          ()
          (do
            (def aux (clojure.string/replace (clojure.string/trim x) #"\." ""))
            (if (= (clojure.string/blank? aux) true) ()
              (do
                (if (= (valid-fact-syntax aux) false) (throw (Exception. "not fact valid")) (def result4 (conj result4 aux)))
              ))
          )
        )
      )
      (catch Exception e (def result4 nil)))
    result4
  )
)

(defn get-rules-from-database
  "Obtiene un vector con todos los facts trimeados y sin punto"
  [database]
  (do
    (def the-answer (clojure.string/split-lines database))
    (def result [])
    (doseq [x the-answer]
      (def parsed_line (clojure.string/replace (clojure.string/trim x) #"\." ""))
      (if (.contains parsed_line ":-") (def result (conj result (clojure.string/replace (clojure.string/trim x) #"\." ""))) ())
      )
    result
    )
  )

(defn analize-facts
  "Analize facts"
  [parsed_facts query]
  (do
    (def result (some #(= query %) parsed_facts))
    (if (= result true) true false)
  )
)

(defn get_rule_name_from_query
  "Obtiene el rule name from query"
  [query]
  (do
    (subs query 0 (clojure.string/index-of query "("))
    )
  )

(defn get_rule_by_name
  "Obtiene la rule segun el nombre"
  [parsed_rules rule_name]
  (do
    (def rule nil)
    (doseq [x parsed_rules]
      (if (= (clojure.string/starts-with? x rule_name) true) (def rule x) ())
      )
    rule
    )
  )

(defn get_params_from_fact
  "Obtiene los parametros de un fact"
  [query]
  (do
      (def subquery (subs query (+ (clojure.string/index-of query "(") 1) (clojure.string/index-of query ")")))
      (def subquery_parts (clojure.string/split subquery #","))
      (def result [])
      (doseq [x subquery_parts]
        (def parsed_line (clojure.string/trim x))
        (def result (conj result parsed_line))
        )
      result
    )
  )

(defn get_new_facts_from_rule
  "Obtiene los facts de la rule segun el query"
  [rule query]
  (do
    (def rule_parts (clojure.string/split rule #":-"))
    (def query_param (get_params_from_fact query))
    (def rule_param (get_params_from_fact (get rule_parts 0)))
    (def result (get rule_parts 1))
    (doseq [x rule_param]
      (def result (clojure.string/replace result x (get query_param (.indexOf rule_param x))))
    )
    (def result2 [])
    (def new_facts_cont (count (re-seq #"\)" result)))
    (loop [x new_facts_cont]
      (when (> x 0)
        (def aux (subs result 0 (+ (clojure.string/index-of result ")") 1)))
        (def result2 (conj result2 (clojure.string/trim aux)))
        (try
        (def result (subs result (+ (clojure.string/index-of result ")") 2)))
        (catch Exception e ()))
        (recur (- x 1))))
    result2
    )
  )

(defn match_facts
  "Obtiene los parametros de un fact"
  [new_facts_from_rule parsed_facts]
  (do
      (def matches 0)
      (doseq [x new_facts_from_rule]
        (if (= (some #(= x %) parsed_facts) true) (def matches (+ matches 1)) ())
      )
      (if (= (count new_facts_from_rule) matches) true false)
    )
  )

(defn process_rule
  "Procesa rule"
  [rule query parsed_facts]
  (do
    (def new_facts_from_rule (get_new_facts_from_rule rule query))
    (def result (match_facts new_facts_from_rule parsed_facts))
    result
    )
  )

(defn analize-rules
  "Analize rules"
  [parsed_facts parsed_rules query]
  (do
    (def rule_name (get_rule_name_from_query query))
    (def rule (get_rule_by_name parsed_rules rule_name))
    (if (= rule nil)
      false
      (process_rule rule query parsed_facts)
    )
  )
)

(defn evaluate-query-core
  "Core del metodo evaluate query"
  [database query]
  (do
    (def parsed_facts (get-facts-from-database database))
    (def parsed_rules (get-rules-from-database database))
    (if (nil? parsed_facts) nil (do
      (if (nil? parsed_rules) nil (do
        (if (= parsed_facts nil) () (def result false))
        (if (= (clojure.string/blank? query) false) () (def result false))
        (def is_fact (analize-facts parsed_facts query))
        (if (= is_fact true)
          is_fact
          (do
            (def match_rule (analize-rules parsed_facts parsed_rules query))
            match_rule
            )
          )
        ))
      )
    )
  )
)

(defn evaluate-query
  "Returns true if the rules and facts in database imply query, false if not. If
  either input can't be parsed, returns nil"
  [database query]
  (do
    (if (= (valid-fact-syntax query) true) (evaluate-query-core database query) nil)
  )
)

