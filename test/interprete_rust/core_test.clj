(ns interprete-rust.core-test
  (:require [clojure.test :refer :all]
            [interprete-rust.core :refer :all]))

(deftest listar-test
  (testing "Listar") 
  (is 
   (= (with-out-str (listar (list 'fn 'main (symbol "(") (symbol ")") (symbol "{") 'println! (symbol "(") "Hola, mundo!" (symbol ")") (symbol "}"))))
      "fn main ( ) \n{\n  println! ( \"Hola, mundo!\" ) \n}\n\r\n")))