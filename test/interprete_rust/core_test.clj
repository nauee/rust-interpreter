(ns interprete-rust.core-test
  #_{:clj-kondo/ignore [:refer-all]}
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
   (= (with-out-str (dump '[[POPREF 2] [PUSHFI 2] MUL [PUSHFI 1] ADD NEG]))
      "0 [POPREF 2]\n1 [PUSHFI 2]\n2 MUL\n3 [PUSHFI 1]\n4 ADD\n5 NEG\n"))
  (is
   (= (with-out-str (dump '[HLT]))
      "0 HLT\n"))
  (is
   (= (with-out-str (dump nil))
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

(deftest buscar-tipo-de-retorno-test
  (testing "Buscar tipo de retorno")
  (is
   (= (buscar-tipo-de-retorno [(symbol ";") (list 'println! (symbol "(") "La suma de 5 mas 7 es {}" (symbol ",") 'suma (symbol "(") 5 (symbol ",") 7 (symbol ")") (symbol ")") (symbol ";") (symbol "}")) ['fn 'suma (symbol "(") 'x (symbol ":") 'i64 (symbol ",") 'y (symbol ":") 'i64 (symbol ")") (symbol "->") 'i64 (symbol "{") 'x '+ 'y (symbol "}") 'fn 'main (symbol "(") (symbol ")") (symbol "{") 'suma (symbol "(") 5 (symbol ",") 7 (symbol ")")] :sin-errores [[0 2] [['suma ['fn [(list ['x (symbol ":") 'i64] ['y (symbol ":") 'i64]) 'i64]] 2] ['main ['fn [() ()]] 8]]] 0 [['CAL 8] 'HLT ['POPARG 1] ['POPARG 0] ['PUSHFM 0] ['PUSHFM 1] 'ADD 'RET ['PUSHFI 5] ['PUSHFI 7] ['CAL 2]] [[2 ['i64 nil] ['i64 nil]] [8]]] 2)
      'i64))
  (is
   (= (buscar-tipo-de-retorno [(symbol ";") (list 'println! (symbol "(") "La suma de 5 mas 7 es {}" (symbol ",") 'suma (symbol "(") 5 (symbol ",") 7 (symbol ")") (symbol ")") (symbol ";") (symbol "}")) ['fn 'suma (symbol "(") 'x (symbol ":") 'i64 (symbol ",") 'y (symbol ":") 'i64 (symbol ")") (symbol "->") 'i64 (symbol "{") 'x '+ 'y (symbol "}") 'fn 'main (symbol "(") (symbol ")") (symbol "{") 'suma (symbol "(") 5 (symbol ",") 7 (symbol ")")] :sin-errores [[0 2] [['suma ['fn [(list ['x (symbol ":") 'i64] ['y (symbol ":") 'i64]) 'i64]] 2] ['main ['fn [() ()]] 8]]] 0 [['CAL 8] 'HLT ['POPARG 1] ['POPARG 0] ['PUSHFM 0] ['PUSHFM 1] 'ADD 'RET ['PUSHFI 5] ['PUSHFI 7] ['CAL 2]] [[2 ['i64 nil] ['i64 nil]] [8]]] 8)
      '()))
  (is
   (= (buscar-tipo-de-retorno [(symbol ";") (list 'println! (symbol "(") "La suma de 5 mas 7 es {}" (symbol ",") 'suma (symbol "(") 5 (symbol ",") 7 (symbol ")") (symbol ")") (symbol ";") (symbol "}")) ['fn 'suma (symbol "(") 'x (symbol ":") 'i64 (symbol ",") 'y (symbol ":") 'i64 (symbol ")") (symbol "->") 'i64 (symbol "{") 'x '+ 'y (symbol "}") 'fn 'main (symbol "(") (symbol ")") (symbol "{") 'suma (symbol "(") 5 (symbol ",") 7 (symbol ")")] :sin-errores [[0 2] [['suma ['fn [(list ['x (symbol ":") 'i64] ['y (symbol ":") 'i64]) 'i64]] 2] ['main ['fn [() ()]] 8]]] 0 [['CAL 8] 'HLT ['POPARG 1] ['POPARG 0] ['PUSHFM 0] ['PUSHFM 1] 'ADD 'RET ['PUSHFI 5] ['PUSHFI 7] ['CAL 2]] [[2 ['i64 nil] ['i64 nil]] [8]]] 1)
      nil)))

(deftest generar-ref-test
  (testing "Generar ref")
  (is
   (= (generar-ref [(symbol ")") (list (symbol ";") 'println! (symbol "(") "{}" (symbol ",") 'v (symbol ")") (symbol ";") (symbol "}")) ['fn 'inc (symbol "(") 'v (symbol ":") (symbol "&") 'mut 'i64 (symbol ")") (symbol "{") '* 'v (symbol "+=") 1 (symbol ";") (symbol "}") 'fn 'main (symbol "(") (symbol ")") (symbol "{") 'let 'mut 'v (symbol ":") 'i64 (symbol "=") 5 (symbol ";") 'inc (symbol "(") (symbol "&") 'mut 'v] 8 [[0 2] [['inc ['fn [(list ['v (symbol ":") (symbol "&") 'mut 'i64]) ()]] 2] ['main ['fn [() ()]] 6] ['v ['var-mut 'i64] 0]]] 1 [['CAL 6] 'HLT ['POPARG 0] ['PUSHFI 1] ['POPADDREF 0] 'RETN ['PUSHFI 5] ['POP 0]] [[2 ['i64 nil]] [6 ['i64 nil]]]])
      [(symbol ")") (list (symbol ";") 'println! (symbol "(") "{}" (symbol ",") 'v (symbol ")") (symbol ";") (symbol "}")) ['fn 'inc (symbol "(") 'v (symbol ":") (symbol "&") 'mut 'i64 (symbol ")") (symbol "{") '* 'v (symbol "+=") 1 (symbol ";") (symbol "}") 'fn 'main (symbol "(") (symbol ")") (symbol "{") 'let 'mut 'v (symbol ":") 'i64 (symbol "=") 5 (symbol ";") 'inc (symbol "(") (symbol "&") 'mut 'v] 8 [[0 2] [['inc ['fn [(list ['v (symbol ":") (symbol "&") 'mut 'i64]) ()]] 2] ['main ['fn [() ()]] 6] ['v ['var-mut 'i64] 0]]] 1 [['CAL 6] 'HLT ['POPARG 0] ['PUSHFI 1] ['POPADDREF 0] 'RETN ['PUSHFI 5] ['POP 0]] [[2 ['i64 nil]] [6 ['i64 nil]]]]))
  (is
   (= (generar-ref [(symbol ")") (list (symbol ";") 'println! (symbol "(") "{}" (symbol ",") 'v (symbol ")") (symbol ";") (symbol "}")) ['fn 'inc (symbol "(") 'v (symbol ":") (symbol "&") 'mut 'i64 (symbol ")") (symbol "{") '* 'v (symbol "+=") 1 (symbol ";") (symbol "}") 'fn 'main (symbol "(") (symbol ")") (symbol "{") 'let 'mut 'v (symbol ":") 'i64 (symbol "=") 5 (symbol ";") 'inc (symbol "(") (symbol "&") 'mut 'v] :sin-errores [[0 2] [['inc ['fn [(list ['v (symbol ":") (symbol "&") 'mut 'i64]) ()]] 2] ['main ['fn [() ()]] 6] ['v ['var-mut 'i64] 0]]] 1 [['CAL 6] 'HLT ['POPARG 0] ['PUSHFI 1] ['POPADDREF 0] 'RETN ['PUSHFI 5] ['POP 0]] [[2 ['i64 nil]] [6 ['i64 nil]]]])
      [(symbol ")") (list (symbol ";") 'println! (symbol "(") "{}" (symbol ",") 'v (symbol ")") (symbol ";") (symbol "}")) ['fn 'inc (symbol "(") 'v (symbol ":") (symbol "&") 'mut 'i64 (symbol ")") (symbol "{") '* 'v (symbol "+=") 1 (symbol ";") (symbol "}") 'fn 'main (symbol "(") (symbol ")") (symbol "{") 'let 'mut 'v (symbol ":") 'i64 (symbol "=") 5 (symbol ";") 'inc (symbol "(") (symbol "&") 'mut 'v] :sin-errores [[0 2] [['inc ['fn [(list ['v (symbol ":") (symbol "&") 'mut 'i64]) ()]] 2] ['main ['fn [() ()]] 6] ['v ['var-mut 'i64] 0]]] 1 [['CAL 6] 'HLT ['POPARG 0] ['PUSHFI 1] ['POPADDREF 0] 'RETN ['PUSHFI 5] ['POP 0] ['PUSHADDR 0]] [[2 ['i64 nil]] [6 ['i64 nil]]]])))

(deftest fixup-test
  (testing "Fixup")
  (is
   (= (fixup [(symbol "{") (list 'x '= 20 (symbol ";") (symbol "}") (symbol ";") 'println! (symbol "(") "{}" (symbol ",") 'x (symbol ")") (symbol "}")) ['fn 'main (symbol "(") (symbol ")") (symbol "{") 'let 'x (symbol ":") 'i64 (symbol ";") 'if false (symbol "{") 'x '= 10 (symbol ";") (symbol "}") 'else] 8 [[0 1 2] [['main ['fn [() ()]] 2] ['x ['var-inmut 'i64] 0]]] 1 [['CAL 2] 'HLT ['PUSHFI false] ['JC 5] ['JMP '?] ['PUSHFI 10] ['POP 0] ['JMP '?]] [[2 ['i64 nil]]]] 4)
      [(symbol "{") (list 'x '= 20 (symbol ";") (symbol "}") (symbol ";") 'println! (symbol "(") "{}" (symbol ",") 'x (symbol ")") (symbol "}")) ['fn 'main (symbol "(") (symbol ")") (symbol "{") 'let 'x (symbol ":") 'i64 (symbol ";") 'if false (symbol "{") 'x '= 10 (symbol ";") (symbol "}") 'else] 8 [[0 1 2] [['main ['fn [() ()]] 2] ['x ['var-inmut 'i64] 0]]] 1 [['CAL 2] 'HLT ['PUSHFI false] ['JC 5] ['JMP '?] ['PUSHFI 10] ['POP 0] ['JMP '?]] [[2 ['i64 nil]]]]))
  (is
   (= (fixup [(symbol "{") (list 'x '= 20 (symbol ";") (symbol "}") (symbol ";") 'println! (symbol "(") "{}" (symbol ",") 'x (symbol ")") (symbol "}")) ['fn 'main (symbol "(") (symbol ")") (symbol "{") 'let 'x (symbol ":") 'i64 (symbol ";") 'if false (symbol "{") 'x '= 10 (symbol ";") (symbol "}") 'else] :sin-errores [[0 1 2] [['main ['fn [() ()]] 2] ['x ['var-inmut 'i64] 0]]] 1 [['CAL 2] 'HLT ['PUSHFI false] ['JC 5] ['JMP '?] ['PUSHFI 10] ['POP 0] ['JMP '?]] [[2 ['i64 nil]]]] 4)
      [(symbol "{") (list 'x '= 20 (symbol ";") (symbol "}") (symbol ";") 'println! (symbol "(") "{}" (symbol ",") 'x (symbol ")") (symbol "}")) ['fn 'main (symbol "(") (symbol ")") (symbol "{") 'let 'x (symbol ":") 'i64 (symbol ";") 'if false (symbol "{") 'x '= 10 (symbol ";") (symbol "}") 'else] :sin-errores [[0 1 2] [['main ['fn [() ()]] 2] ['x ['var-inmut 'i64] 0]]] 1 [['CAL 2] 'HLT ['PUSHFI false] ['JC 5] ['JMP 8] ['PUSHFI 10] ['POP 0] ['JMP '?]] [[2 ['i64 nil]]]]))
  (is
   (= (fixup [(symbol "{") (list 'x '= 20 (symbol ";") (symbol "}") (symbol ";") 'println! (symbol "(") "{}" (symbol ",") 'x (symbol ")") (symbol "}")) ['fn 'main (symbol "(") (symbol ")") (symbol "{") 'let 'x (symbol ":") 'i64 (symbol ";") 'if false (symbol "{") 'x '= 10 (symbol ";") (symbol "}") 'else] :sin-errores [[0 1 2] [['main ['fn [() ()]] 2] ['x ['var-inmut 'i64] 0]]] 1 [['CAL 2] 'HLT ['PUSHFI false] ['JC 5] ['JMP '?] ['PUSHFI 10] ['POP 0] ['JMP '?]] [[2 ['i64 nil]]]] 3)
      [(symbol "{") (list 'x '= 20 (symbol ";") (symbol "}") (symbol ";") 'println! (symbol "(") "{}" (symbol ",") 'x (symbol ")") (symbol "}")) ['fn 'main (symbol "(") (symbol ")") (symbol "{") 'let 'x (symbol ":") 'i64 (symbol ";") 'if false (symbol "{") 'x '= 10 (symbol ";") (symbol "}") 'else] :sin-errores [[0 1 2] [['main ['fn [() ()]] 2] ['x ['var-inmut 'i64] 0]]] 1 [['CAL 2] 'HLT ['PUSHFI false] ['JC 5] ['JMP '?] ['PUSHFI 10] ['POP 0] ['JMP '?]] [[2 ['i64 nil]]]]))
  (is
   (= (fixup [(symbol "{") (list 'x '= 20 (symbol ";") (symbol "}") (symbol ";") 'println! (symbol "(") "{}" (symbol ",") 'x (symbol ")") (symbol "}")) ['fn 'main (symbol "(") (symbol ")") (symbol "{") 'let 'x (symbol ":") 'i64 (symbol ";") 'if false (symbol "{") 'x '= 10 (symbol ";") (symbol "}") 'else] :sin-errores [[0 1 2] [['main ['fn [() ()]] 2] ['x ['var-inmut 'i64] 0]]] 1 [['CAL 2] 'HLT ['PUSHFI false] ['JC 5] ['JMP '?] ['PUSHFI 10] ['POP 0] ['JMP '?]] [[2 ['i64 nil]]]] 7)
      [(symbol "{") (list 'x '= 20 (symbol ";") (symbol "}") (symbol ";") 'println! (symbol "(") "{}" (symbol ",") 'x (symbol ")") (symbol "}")) ['fn 'main (symbol "(") (symbol ")") (symbol "{") 'let 'x (symbol ":") 'i64 (symbol ";") 'if false (symbol "{") 'x '= 10 (symbol ";") (symbol "}") 'else] :sin-errores [[0 1 2] [['main ['fn [() ()]] 2] ['x ['var-inmut 'i64] 0]]] 1 [['CAL 2] 'HLT ['PUSHFI false] ['JC 5] ['JMP '?] ['PUSHFI 10] ['POP 0] ['JMP 8]] [[2 ['i64 nil]]]])))

(deftest dividir-test
  (testing "Dividir")
  (is (= (dividir 12 3) 4))
  (is (= (dividir 12.0 3) 4.0))
  (is (= (dividir 12 3.0) 4.0))
  (is (= (dividir 12.0 3.0) 4.0))
  (is (= (dividir 1 2) 0))
  (is (= (dividir 1.0 2) 0.5))
  (is (= (dividir 1 2.0) 0.5))
  (is (= (dividir 1.0 2.0) 0.5)))

(deftest compatibles?-test
  (testing "Compatible")
  (is (= (compatibles? 'i64 5) true))
  (is (= (compatibles? 'i64 5.0) false))
  (is (= (compatibles? 'i64 [5 6 7]) true))
  (is (= (compatibles? 'i64 [5.0 6.0 7.0]) false))
  (is (= (compatibles? 'f64 5) false))
  (is (= (compatibles? 'f64 5.0) true))
  (is (= (compatibles? 'f64 [5 6 7]) false))
  (is (= (compatibles? 'f64 [5.0 6.0 7.0]) true))
  (is (= (compatibles? 'String "Hola") true))
  (is (= (compatibles? 'String ["Hola" "Mundo"]) true))
  (is (= (compatibles? 'String 2) false))
  (is (= (compatibles? 'bool 'true) true))
  (is (= (compatibles? 'bool ['true 'false 'true]) true))
  (is (= (compatibles? 'bool 1) false))
  (is (= (compatibles? 'usize 1) true))
  (is (= (compatibles? 'usize 1.0) false))
  (is (= (compatibles? 'usize [1 2 3]) true)))

(deftest pasar-a-int-test
  (testing "Pasar a int")
  (is (= (pasar-a-int "10") 10))
  (is (= (pasar-a-int " 0 ") 0))
  (is (= (pasar-a-int "12.5") 12))
  (is (= (pasar-a-int "10 por 10 es 20") "10 por 10 es 20"))
  (is (= (pasar-a-int 10.0) 10))
  (is (= (pasar-a-int 10) 10))
  (is (= (pasar-a-int 'a) 'a))
  (is (= (pasar-a-int [10.0]) [10.0])))

(deftest pasar-a-float-test
  (testing "Pasar a float")
  (is (= (pasar-a-float "10") 10.0))
  (is (= (pasar-a-float " 0 ") 0.0))
  (is (= (pasar-a-float "12.5") 12.5))
  (is (= (pasar-a-int "2.5 por 2 es 5") "2.5 por 2 es 5"))
  (is (= (pasar-a-float 10) 10.0))
  (is (= (pasar-a-float 12.5) 12.5))
  (is (= (pasar-a-float 'a) 'a))
  (is (= (pasar-a-float [10]) [10])))