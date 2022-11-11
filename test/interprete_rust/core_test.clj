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

(deftest ya-declarado-localmente?-test
  (testing "Ya declarado localmente")
  (is
   (= (ya-declarado-localmente? 'Write [[0] [['io ['lib '()] 0] ['Write ['lib '()] 0] ['entero_a_hexa ['fn [(list ['n (symbol ":") 'i64]) 'String]] 2]]]) true))
  (is
   (= (ya-declarado-localmente? 'Read [[0] [['io ['lib '()] 0] ['Write ['lib '()] 0] ['entero_a_hexa ['fn [(list ['n (symbol ":") 'i64]) 'String]] 2]]]) false))
  (is
   (= (ya-declarado-localmente? 'Write [[0 1] [['io ['lib '()] 0] ['Write ['lib '()] 0] ['entero_a_hexa ['fn [(list ['n (symbol ":") 'i64]) 'String]] 2]]]) true))
  (is
   (= (ya-declarado-localmente? 'Write [[0 2] [['io ['lib '()] 0] ['Write ['lib '()] 0] ['entero_a_hexa ['fn [(list ['n (symbol ":") 'i64]) 'String]] 2]]]) false)))

(deftest cargar-const-en-tabla-test
  (testing "Cargar const en tabla")
  (is
   (= (cargar-const-en-tabla [(symbol ";") (list 'fn 'main (symbol "(") (symbol ")") (symbol "{") 'println! (symbol "(") "{}" (symbol ",") 'TRES (symbol ")") (symbol "}")) ['use 'std (symbol "::") 'io (symbol ";") 'const 'TRES (symbol ":") 'i64 (symbol "=") 3] 8 [[0] [['io ['lib '()] 0]]] 0 [['CAL 0] 'HLT] []])
       [(symbol ";") (list 'fn 'main (symbol "(") (symbol ")") (symbol "{") 'println! (symbol "(") "{}" (symbol ",") 'TRES (symbol ")") (symbol "}")) ['use 'std (symbol "::") 'io (symbol ";") 'const 'TRES (symbol ":") 'i64 (symbol "=") 3] 8 [[0] [['io ['lib '()] 0]]] 0 [['CAL 0] 'HLT] []]))
  (is
    (= (cargar-const-en-tabla [(symbol ";") (list 'fn 'main (symbol "(") (symbol ")") (symbol "{") 'println! (symbol "(") "{}" (symbol ",") 'TRES (symbol ")") (symbol "}")) ['use 'std (symbol "::") 'io (symbol ";") 'const 'TRES (symbol ":") 'i64 (symbol "=") 3] :sin-errores [[0] [['io ['lib '()] 0]]] 0 [['CAL 0] 'HLT] []])
        [(symbol ";") (list 'fn 'main (symbol "(") (symbol ")") (symbol "{") 'println! (symbol "(") "{}" (symbol ",") 'TRES (symbol ")") (symbol "}")) ['use 'std (symbol "::") 'io (symbol ";") 'const 'TRES (symbol ":") 'i64 (symbol "=") 3] :sin-errores [[0] [['io ['lib '()] 0] ['TRES ['const 'i64] 3]]] 0 [['CAL 0] 'HLT] []])))

(deftest inicializar-contexto-local-test
  (testing "Inicializar contexto local")
  (is
   (= (inicializar-contexto-local [(symbol "{") (list 'let 'x (symbol ":") 'i64 (symbol "=") 10 (symbol ";") 'println! (symbol "(") "{}" (symbol ",") 'x (symbol ")") (symbol "}")) ['fn 'main (symbol "(") (symbol ")")] 8 [[0] [['main ['fn [() ()]] 2]]] 0 [['CAL 2] 'HLT] []])
       [(symbol "{") (list 'let 'x (symbol ":") 'i64 (symbol "=") 10 (symbol ";") 'println! (symbol "(") "{}" (symbol ",") 'x (symbol ")") (symbol "}")) ['fn 'main (symbol "(") (symbol ")")] 8 [[0] [['main ['fn [() ()]] 2]]] 0 [['CAL 2] 'HLT] []]))
  (is
    (= (inicializar-contexto-local [(symbol "{") (list 'let 'x (symbol ":") 'i64 (symbol "=") 10 (symbol ";") 'println! (symbol "(") "{}" (symbol ",") 'x (symbol ")") (symbol "}")) ['fn 'main (symbol "(") (symbol ")")] :sin-errores [[0] [['main ['fn [() ()]] 2]]] 0 [['CAL 2] 'HLT] []])
        [(symbol "{") (list 'let 'x (symbol ":") 'i64 (symbol "=") 10 (symbol ";") 'println! (symbol "(") "{}" (symbol ",") 'x (symbol ")") (symbol "}")) ['fn 'main (symbol "(") (symbol ")")] :sin-errores [[0 1] [['main ['fn [() ()]] 2]]] 0 [['CAL 2] 'HLT] []])))

(deftest restaurar-contexto-anterior-test
  (testing "Restaurar contexto anterior")
  (is
   (= (restaurar-contexto-anterior ['EOF () ['fn 'main (symbol "(") (symbol ")") (symbol "{") 'let 'x (symbol ":") 'i64 (symbol "=") 10 (symbol ";") 'let 'y (symbol ":") 'i64 (symbol "=") 20 (symbol ";") 'println! (symbol "(") "{}" (symbol ",") 'x '+ 'y (symbol ")") (symbol "}")] 8 [[0 1] [['main ['fn [() ()]] 2] ['x ['var-inmut 'i64] 0] ['y ['var-inmut 'i64] 1]]] 2 [['CAL 2] 'HLT ['PUSHFI 10] ['POP 0] ['PUSHFI 20] ['POP 1] ['PUSHFI "{}"] ['PUSHFM 0] ['PUSHFM 1] 'ADD ['PUSHFI 2] 'OUT 'NL] [[2 ['i64 nil] ['i64 nil]]]])
      ['EOF () ['fn 'main (symbol "(") (symbol ")") (symbol "{") 'let 'x (symbol ":") 'i64 (symbol "=") 10 (symbol ";") 'let 'y (symbol ":") 'i64 (symbol "=") 20 (symbol ";") 'println! (symbol "(") "{}" (symbol ",") 'x '+ 'y (symbol ")") (symbol "}")] 8 [[0 1] [['main ['fn [() ()]] 2] ['x ['var-inmut 'i64] 0] ['y ['var-inmut 'i64] 1]]] 2 [['CAL 2] 'HLT ['PUSHFI 10] ['POP 0] ['PUSHFI 20] ['POP 1] ['PUSHFI "{}"] ['PUSHFM 0] ['PUSHFM 1] 'ADD ['PUSHFI 2] 'OUT 'NL] [[2 ['i64 nil] ['i64 nil]]]]))
  (is
   (= (restaurar-contexto-anterior ['EOF () ['fn 'main (symbol "(") (symbol ")") (symbol "{") 'let 'x (symbol ":") 'i64 (symbol "=") 10 (symbol ";") 'let 'y (symbol ":") 'i64 (symbol "=") 20 (symbol ";") 'println! (symbol "(") "{}" (symbol ",") 'x '+ 'y (symbol ")") (symbol "}")] :sin-errores [[0 1] [['main ['fn [() ()]] 2] ['x ['var-inmut 'i64] 0] ['y ['var-inmut 'i64] 1]]] 2 [['CAL 2] 'HLT ['PUSHFI 10] ['POP 0] ['PUSHFI 20] ['POP 1] ['PUSHFI "{}"] ['PUSHFM 0] ['PUSHFM 1] 'ADD ['PUSHFI 2] 'OUT 'NL] [[2 ['i64 nil] ['i64 nil]]]])
      ['EOF () ['fn 'main (symbol "(") (symbol ")") (symbol "{") 'let 'x (symbol ":") 'i64 (symbol "=") 10 (symbol ";") 'let 'y (symbol ":") 'i64 (symbol "=") 20 (symbol ";") 'println! (symbol "(") "{}" (symbol ",") 'x '+ 'y (symbol ")") (symbol "}")] :sin-errores [[0] [['main ['fn [() ()]] 2]]] 2 [['CAL 2] 'HLT ['PUSHFI 10] ['POP 0] ['PUSHFI 20] ['POP 1] ['PUSHFI "{}"] ['PUSHFM 0] ['PUSHFM 1] 'ADD ['PUSHFI 2] 'OUT 'NL] [[2 ['i64 nil] ['i64 nil]]]])))