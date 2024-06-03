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

(defn calcular-autos [cruceros autos tiempo-total]
  (let [total-autos (atom (zipmap (map first cruceros) (repeat 0))) ; Envuelve el PersistentArrayMap en un átomo
        crucero-info (map #(hash-map :direccion (nth % 0) 
                                     :tiempo-verde (nth % 1)
                                     :tiempo-rojo (nth % 2)
                                     :tiempo-blanco (nth % 3)) cruceros)]
    (doseq [t (range tiempo-total)]
      (doseq [auto autos]
        (let [[origen tiempo-cruce tiempo-llegada] (take 3 auto)
              crucero (first (filter #(= (:direccion %) origen) crucero-info))]
          (when (and crucero 
                     (<= (+ tiempo-llegada (* t (reduce + (map :tiempo-verde crucero-info))))
                         tiempo-total)
                     (<= tiempo-cruce (:tiempo-verde crucero)))
            (swap! total-autos update origen inc))))) ; Incrementa el conteo de autos que cruzan por cada semáforo
    (println "Cantidad de autos por semáforo:")
    (doseq [[semaforo cantidad] @total-autos] ; Accede al valor del átomo con @total-autos
      (println semaforo ":" cantidad))
    (let [total-autos (apply + (vals @total-autos))]
      (println "Total de autos en" tiempo-total "segundos:" total-autos)
      total-autos)))

(def tiempo-total 300) ; Tiempo total de análisis en segundos
(println (calcular-autos crucero vehiculos tiempo-total))
