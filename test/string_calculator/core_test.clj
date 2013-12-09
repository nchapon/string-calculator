(ns string-calculator.core-test
  (:require [midje.sweet :refer :all]
            [string-calculator.core :refer :all]))

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

(fact "Support different delimiters"
  (add "//;\n1;2;3") => 6)

(fact
  "Calling add with a negative number will throw an exception
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

(fact "Make sure we can handle multiple delimiters with length longer than one char"
  (add "//[*][%%]\n1*2%%3") => 6)

(fact "Escape special characters if necessary"
  (escape-special-characters ",") => ","
  (escape-special-characters "*") => "\\*")

(fact "Parse terms in a vector [numbers [sep1 sep2]]"
  (parse-terms "1,2,3") => ["1,2,3" ["," "\n"]]
  (parse-terms "//;\n1;2") => ["1;2" [";"]]
  (parse-terms "//[***]\n1***2") => ["1***2" ["***"]])

(fact "Read delimiters in a vector"
  (read-delimiters ",") => [","]
  (read-delimiters "[***]") => ["***"]
  (read-delimiters "[x][y]") => ["x" "y"]
  (read-delimiters "[x][yy]") => ["x" "yy"])
