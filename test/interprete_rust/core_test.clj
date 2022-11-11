(ns interprete-rust.core-test
  (:require [clojure.test :refer :all]
            [interprete-rust.core :refer :all]))

(deftest listar-test
  (testing "Listar") 
  (is 
   (= (with-out-str (listar (list 'fn 'main (symbol "(") (symbol ")") (symbol "{") 'println! (symbol "(") "Hola, mundo!" (symbol ")") (symbol "}"))))
      "fn main ( ) \n{\n  println! ( \"Hola, mundo!\" ) \n}\n\r\n")))

(deftest agregar-ptocoma-test
  (testing "Agregar punto y coma")
  (is
   (= (agregar-ptocoma (list 'fn 'main (symbol "(") (symbol ")") (symbol "{") 'if 'x '< '0 (symbol "{") 'x '= '- 'x (symbol ";") (symbol "}") 'renglon '= 'x (symbol ";") 'if 'z '< '0 (symbol "{") 'z '= '- 'z (symbol ";") (symbol "}") (symbol "}") 'fn 'foo (symbol "(") (symbol ")") (symbol "{") 'if 'y '> '0 (symbol "{") 'y '= '- 'y (symbol ";") (symbol "}") 'else (symbol "{") 'x '= '- 'y (symbol ";") (symbol "}") (symbol "}")))
      (list 'fn 'main (symbol "(") (symbol ")") (symbol "{") 'if 'x '< '0 (symbol "{") 'x '= '- 'x (symbol ";") (symbol "}") (symbol ";") 'renglon '= 'x (symbol ";") 'if 'z '< '0 (symbol "{") 'z '= '- 'z (symbol ";") (symbol "}") (symbol "}") 'fn 'foo (symbol "(") (symbol ")") (symbol "{") 'if 'y '> '0 (symbol "{") 'y '= '- 'y (symbol ";") (symbol "}") 'else (symbol "{") 'x '= '- 'y (symbol ";") (symbol "}") (symbol "}")))))

(deftest palabra-reservada?-test
  (testing "Palabra reservada")
  (is
   (= (palabra-reservada? 'while) true))
  (is
   (= (palabra-reservada? 'if) true))
  (is
   (= (palabra-reservada? 'else) true))
  (is
   (= (palabra-reservada? 'until) false))
  (is
   (= (palabra-reservada? 13) false)))

(deftest identificador?-test
  (testing "Identificador")
  (is
   (= (identificador? 'boolean) true))
  (is
   (= (identificador? 'bool) false))
  (is
   (= (identificador? 'e120) true))
  (is
   (= (identificador? '12e0) false)))

; user=> (dump '[[POPREF 2] [PUSHFI 2] MUL [PUSHFI 1] ADD NEG])
; 0 [POPREF 2]
; 1 [PUSHFI 2]
; 2 MUL
; 3 [PUSHFI 1]
; 4 ADD
; 5 NEG
; nil
; user=> (dump '[HLT])
; 0 HLT
; nil
; user=> (dump nil)
; 0 nil
; nil

(deftest dump-test
  (testing "Dump")
  (is
    (= (with-out-str(dump '[[POPREF 2] [PUSHFI 2] MUL [PUSHFI 1] ADD NEG]))
        "0 [POPREF 2]\n1 [PUSHFI 2]\n2 MUL\n3 [PUSHFI 1]\n4 ADD\n5 NEG\n"))
  (is
    (= (with-out-str(dump '[HLT]))
        "0 HLT\n"))
  (is
    (= (with-out-str(dump nil))
        "0 nil\n")))