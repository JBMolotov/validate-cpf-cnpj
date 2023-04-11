(ns validate-documents.core
  (:require [clojure.string :as string])
  (:gen-class))


(defn calc-dv1-cpf [digitos]
  (let [mults (reverse (range 2 11))
        dv1 (- 11 (mod (reduce + (map * digitos mults)) 11))]
    (if (>= dv1 10)
      0
      dv1)))

(defn calc-dv2-cpf [digitos dv1]
  (let [mults (reverse (range 2 12))
        dv2 (- 11 (mod (+ (reduce + (map * digitos mults)) (* 2 dv1)) 11))]
    (if (>= dv2 10)
      0
      dv2)))

(defn calc-dv1-cnpj [digitos]
  (let [mults (take 12 (cycle (range 2 10)))
        dv1 (- 11 (mod (reduce + (map * (reverse digitos) mults)) 11))]
    (if (>= dv1 10)
      0
      dv1)))

(defn calc-dv2-cnpj [digitos dv1]
  (let [mults (take 13 (cycle (range 2 10)))
        dv2 (- 11 (mod (reduce + (map * (conj (reverse digitos) dv1) mults)) 11))]
    (if (>= dv2 10)
      0
      dv2)))

(defn validar-cpf-cnpj [numero]
  (let [numero (string/replace numero #"\D" "")
        tamanho (count numero)]
    (cond
      ;; CPF
      (and (= tamanho 11) (not (re-find #"^(\d)\1+$" numero)))
      (let [digitos (mapv #(Integer/parseInt %) (string/split numero #""))
            dv1 (calc-dv1-cpf (take 9 digitos))
            dv2 (calc-dv2-cpf (take 9 digitos) dv1)]
        (if (= (subvec digitos 9 11) [dv1 dv2])
          (str "CPF")
          false))

      ;; CNPJ
      (and (= tamanho 14) (not (re-find #"^(\d)\1+$" numero)))
      (let [digitos (mapv #(Integer/parseInt %) (string/split numero #""))
            dv1 (calc-dv1-cnpj (take 12 digitos))
            dv2 (calc-dv2-cnpj (take 12 digitos) dv1)]
        (if (= (subvec digitos 12 14) [dv1 dv2])
          (str "CNPJ")
          false))

      ;; Número inválido
      :else false)))



(defn -main
  "I just check it CPFs and CNPJs are valid"
  [& args]
  (println (validar-cpf-cnpj "12345678909")))
