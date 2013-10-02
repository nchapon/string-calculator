(ns string-calculator.core
  (:use [midje.sweet]))


(defn parse-different-delimiters
  [terms]
  (when-first [matcher (re-seq #"^(//(.)\n).*$" terms)]
    (vector
     (subs terms (count (second matcher)))
     (last matcher))))

(defn parse-terms
  [terms]
  (let [s-terms (parse-different-delimiters terms)]
    (cond
     (not (empty? s-terms)) s-terms
     :else (vector terms "[,\n]"))))

(defn parse-numbers
  [numbers sep]
  (map #(Integer/parseInt %) (.split numbers sep)))

(defn add
  [terms]
  (cond (not (empty? terms))
    (let [[numbers sep] (parse-terms terms)]
      (->> (parse-numbers numbers sep)
            (reduce +)))
    :else 0))

(fact "For empty string returns 0"
  (add "") => 0)

(fact "When only one numbers returns it"
  (add "1") => 1)

(fact "Add two numbers"
  (add "1,2") => 3)

(fact "Add an unknow amount of numbers"
  (add "1,2,3") => 6)

(fact "Handle newline between numbers (instead of commas)"
  (add "1\n2,3") => 6)

(fact "When following input is not ok"
  (add "1\n,") => 1)


(fact "Get the separator from string"
  (parse-terms "//;\n1;2") => ["1;2" ";"])

(fact "When no line separator"
  (parse-terms "1,2,3") => ["1,2,3" "[,\n]"])


(fact "Support different delimiters"
  (add "//;\n1;2;3") => 6)
