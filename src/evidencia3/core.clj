(ns evidencia3.core
  (:require [clojure.java.io :as io]))

(defn foo []
  (println "Hello, World!"))

(defn leerCrucero [ruta]
  (with-open [rdr (io/reader ruta)]
    (let [contenido (slurp rdr)]
      (read-string contenido))))

(def crucero (leerCrucero "src/evidencia3/crucero.txt"))
(println crucero)

(defn leerVehiculos [ruta]
  (with-open [rdr (io/reader ruta)]
    (let [contenido (slurp rdr)]
      (read-string contenido))))

(def vehiculos (leerVehiculos "src/evidencia3/vehiculos.txt"))
(println vehiculos)

(defn calcular-autos [cruceros autos]
  (let [total-autos (atom (zipmap (map first cruceros) (repeat 0)))] ; Envuelve el PersistentArrayMap en un átomo
    (doseq [auto autos]
      (let [[origen tiempo] (take 2 auto)]
        (swap! total-autos update origen + tiempo))) ; Usa swap! en el átomo
    (println "Cantidad de autos por semáforo:")
    (doseq [[semaforo cantidad] @total-autos] ; Accede al valor del átomo con @total-autos
      (println semaforo ":" cantidad))
    (println "Total de autos:" (apply + (vals @total-autos))))) ; Accede al valor del átomo con @total-autos

(println (calcular-autos crucero vehiculos))
