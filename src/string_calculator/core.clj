(ns string-calculator.core
  (:use [midje.sweet]))


(defn split-different-delimiters
  [terms]
  (when-first [matcher (re-seq #"^(//(.)\n).*$" terms)]
    (vector
     (subs terms (count (second matcher)))
     (last matcher))))

(defn parse-terms
  [terms]
  (let [s-terms (split-different-delimiters terms)]
    (cond
     (not (empty? s-terms)) s-terms
     :else (vector terms "[,\n]"))))

(defn parse-numbers
  [numbers sep]
  (let [numbers (map #(Integer/parseInt %) (.split numbers sep))]
    (if-let [negatives (seq (filter neg? numbers))] ;; filter does not returns nil
      (throw (IllegalArgumentException.
              (str "Negatives not allowed " (apply str
                                                   (interpose " " negatives)))))
      numbers)))

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

;.;. Excellence is not an act but a habit. -- Aristotle
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

(fact
  "Callind add with a negative number will throw an exception
   'negatives not allowed' - and the negative that was passed.
    If there are multiple negatives, show all of them in the exception message."
  (add "-1,2") => (throws IllegalArgumentException "Negatives not allowed -1")
  (add "-1,-2,3,-4,5") => (throws IllegalArgumentException "Negatives not allowed -1 -2 -4"))
