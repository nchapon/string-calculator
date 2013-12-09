(ns string-calculator.core
  (:use [clojure.string :only [join escape]]))

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
  (escape
   s
   {\* "\\*"})) ;; need to escape more characters

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

(defn format-exception
  [negatives]
  (str "Negatives not allowed " (join  " " negatives)))

(defn to-numbers
  [numbers sep]
  (let [numbers (map #(Integer/parseInt %) (filter not-empty (.split numbers sep)))
        negatives (seq (filter neg? numbers))]
    (if (not-empty negatives)
      (throw (IllegalArgumentException. (format-exception negatives)))
      numbers)))


(defn delimiters->regexp
  "Convert delimiters to valid regexep"
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
      (->> (filter #(<= % 1000) (to-numbers numbers re))
            (reduce +)))
    :else 0))
