(ns clojure-forca.core
  (:gen-class))

(def total-de-vidas 6)
(def palavra-secreta "MELANCIA")

(defn perdeu [] print "Você perdeu!!!")
(defn ganhou [] print "Você ganhou!!!")

(defn letras-faltantes [palavra acertos]
				(remove (fn [letra] (contains? acertos (str letra))) palavra))

(defn acertou-a-palavra-toda? [palavra acertos]
				(empty? (letras-faltantes palavra acertos)))

(defn le-letra! [] (read-line))

(defn acertou? [chute palavra] (.contains palavra chute))

(defn imprime-forca [vidas palavra acertos]
				(println "Vidas " vidas " - Dica: É uma fruta... Você encontra na feira!")
				(doseq [letra (seq palavra)]
								(if (contains? acertos (str letra))
												(print letra " ")
												(print "_" " ")))
				(println))

(defn jogo [vidas palavra acertos]
				(imprime-forca vidas palavra acertos)
				(cond
								(= vidas 0) (perdeu)
								(acertou-a-palavra-toda? palavra acertos) (ganhou)
								:else
								(let [chute (le-letra!)]
											(if (acertou? (.toUpperCase chute) palavra)
															(do
																		(println "Acertou a letra!")
																		(recur vidas palavra (conj acertos (.toUpperCase chute))))
															(do
																		(println "Errou a letra! Perdeu vida!")
																		(recur (dec vidas) palavra acertos))))))

(defn fib[x]
    (if (= x 0) 0 
    (if (= x 1) 1 
    (+ (fib (- x 1)) (fib (- x 2))))))

(defn fibr[x]
    (loop [a 1 b 1 numero 2]
        (if (= numero x) b
           	(recur b (+ a b) (inc numero)))))

(defn soma[n] 
    (loop [contador 1 soma 0]
        (if (> contador n) soma
        (recur (inc contador) (+ soma contador)))))

(def carros [50000.0, 60000.0])

(->> carros 
    (map (fn [x] (* x 2))) 
    (map (fn [x] (* x 3))) 
    (reduce (fn [acc n] (+ acc n))))

(defn comeca-o-jogo [] (jogo total-de-vidas palavra-secreta #{}))

(defn -main [& args]
				(comeca-o-jogo))
