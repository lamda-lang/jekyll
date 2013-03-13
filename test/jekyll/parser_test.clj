(ns jekyll.parser-test
  (:use clojure.test
        jekyll.parser
        midje.sweet))


(fact (parse "ab") => {:Document ['({:String \a} {:String \b}) :$]})