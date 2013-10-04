(ns string-calculator.core
  (:use [midje.sweet]))

(defn parse-multiple-delimiters [s]
  (if-let [multiple-delimiters (vec (.split s "\\]\\["))]
    multiple-delimiters
    s))

(defn read-delimiters [s]
  "Returns a vector containing one or many delimiters"
  (let [matcher (re-seq #"(^\[(.*)\]$)" s)]
    (cond
     (not (empty? matcher)) (parse-multiple-delimiters (last (first matcher)))
     :else [s])))

(defn escape-special-characters
  [s]
  (clojure.string/escape
   s
   {\* "\\*"})) ;; need to escape others characters

(defn split-different-delimiters
  [terms]
  (when-first [matcher (re-seq #"^(//(.+)\n).*$" terms)]
    (vector
     (subs terms (count (second matcher)))
     (read-delimiters (last matcher)))))

(defn parse-terms
  [terms]
  (let [s-terms (split-different-delimiters terms)]
    (cond
     (not (empty? s-terms)) s-terms
     :else (vector terms (vector "," "\n")))))

(defn parse-numbers
  [numbers sep]
  (let [numbers (map #(Integer/parseInt %) (filter not-empty (.split numbers sep)))]
    (if-let [negatives (seq (filter neg? numbers))] ;; filter does not returns nil
      (throw (IllegalArgumentException.
              (str "Negatives not allowed " (apply str
                                                   (interpose " " negatives)))))
      (filter #(<= % 1000) numbers))))


(defn delimiters->regexp
  [delimiters]
  (if (> (count delimiters) 1)
    (str "["
         (apply str (map #(escape-special-characters %) delimiters))
         "]")
    (apply str (map #(escape-special-characters %) delimiters))))


(defn add
  "Add numbers from terms"
  [terms]
  (cond (not (empty? terms))
    (let [[numbers delimiters] (parse-terms terms)
          re (delimiters->regexp delimiters)]
      (->> (parse-numbers numbers re)
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

(fact "Escape special characters if necessary"
  (escape-special-characters ",") => ","
  (escape-special-characters "*") => "\\*")

(fact "Parse terms in a vector [numbers [sep1 sep2]]"
  (parse-terms "1,2,3") => ["1,2,3" ["," "\n"]]
  (parse-terms "//;\n1;2") => ["1;2" [";"]]
  (parse-terms "//[***]\n1***2") => ["1***2" ["***"]])

(fact "Support different delimiters"
  (add "//;\n1;2;3") => 6)

(fact
  "Callind add with a negative number will throw an exception
   'negatives not allowed' - and the negative that was passed.
    If there are multiple negatives, show all of them in the exception message."
  (add "-1,2") => (throws IllegalArgumentException "Negatives not allowed -1")
  (add "-1,-2,3,-4,5") => (throws IllegalArgumentException "Negatives not allowed -1 -2 -4"))

(fact "Numbers bigger than 1000 should be ignored, so adding 2 + 1001  = 2"
  (add "2,1001") => 2
  (add "1001,1002") => 0
  (add "1001") => 0)

(fact "Delimiters can be of any length with the following format"
  (add "//[***]\n1***2***3") => 6)

(fact "Allow multiple delimiters like this:  '//[delim1][delim2]\n'"
   (add "//[*][%]\n1*2%3") => 6)


;.;. Good code is its own best documentation. -- Steve McConnell
(fact "Make sure we can handle multiple delimiters with length longer than one char"
  (add "//[*][%%]\n1*2%%3") => 6)


(fact "Read delimiters in a vector"
  (read-delimiters ",") => [","]
  (read-delimiters "[***]") => ["***"]
  (read-delimiters "[x][y]") => ["x" "y"]
  (read-delimiters "[x][yy]") => ["x" "yy"])
