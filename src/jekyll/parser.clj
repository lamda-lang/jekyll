(ns jekyll.parser
  (:use com.lithinos.amotoen.core :only [pegs lpegs]))

(def grammar {
   :Document [:String :$]
   :String (lpegs  '| "ab")})