(ns evidencia3.core
  (:require [clojure.java.io :as io]))

(defn foo [] (println "Hola Mundo"))
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

(defn procesar-segundo [t crucero-info autos]
  (reduce (fn [acumulador crucero]
            (let [direccion (:direccion crucero)
                  tiempo-verde (:tiempo-verde crucero)
                  ciclo-tiempo (+ (:tiempo-verde crucero) (:tiempo-rojo crucero) (:tiempo-blanco crucero))
                  tiempo-en-ciclo (mod t ciclo-tiempo)
                  autos-en-verde (filter #(and (= (first %) direccion)
                                               (= (nth % 2) t))
                                         autos)
                  semaforo-verde? (pos? tiempo-en-ciclo) ; ¿El semáforo está en verde?
                  autos-pendientes (filter #(and (= (first %) direccion)
                                                 (< (nth % 2) t))
                                           autos)]
              (if (and semaforo-verde? (empty? autos-pendientes))
                (update-in acumulador [:semaforos-verde-sin-flujo direccion] inc)
                (reduce (fn [acum auto]
                          (let [[_ tiempo-cruce _] auto]
                            (-> acum
                                (update-in [:total-autos direccion] inc)
                                (update-in [:tiempo-cruce-por-semaforo direccion] #(+ % tiempo-cruce)))))
                        acumulador autos-en-verde))))
          {:total-autos (zipmap (map :direccion crucero-info) (repeat 0))
           :tiempo-cruce-por-semaforo (zipmap (map :direccion crucero-info) (repeat 0))
           :semaforos-verde-sin-flujo (zipmap (map :direccion crucero-info) (repeat 0))}
          crucero-info))

(defn calcular-autos [cruceros autos tiempo-total]
  (let [crucero-info (map #(hash-map :direccion (nth % 0) 
                                     :tiempo-verde (nth % 1)
                                     :tiempo-rojo (nth % 2)
                                     :tiempo-blanco (nth % 3)) cruceros)
        resultados (->> (pmap #(procesar-segundo % crucero-info autos) (range tiempo-total))
                        (reduce (partial merge-with (partial merge-with +))))]
    
    (println "Cantidad de autos por semáforo:")
    (doseq [[semaforo cantidad] (:total-autos resultados)]
      (println semaforo ":" cantidad))

    (let [total-autos-val (apply + (vals (:total-autos resultados)))
          total-tiempo-cruce (apply + (vals (:tiempo-cruce-por-semaforo resultados)))
          tiempo-promedio-cruce (if (> total-autos-val 0)
                                  (/ total-tiempo-cruce total-autos-val)
                                  0)]
      (println "Total de autos en" tiempo-total "segundos:" total-autos-val)
      (println "Semáforos en verde sin flujo de autos:")
      (doseq [[semaforo sin-flujo] (:semaforos-verde-sin-flujo resultados)]
        (println semaforo ":" sin-flujo))

      (println "Tiempo promedio de cruce por semáforo:")
      (doseq [[semaforo tiempo-total] (:tiempo-cruce-por-semaforo resultados)]
        (let [cantidad ((:total-autos resultados) semaforo)
              tiempo-promedio (if (> cantidad 0)
                                (/ tiempo-total cantidad)
                                0)]
          (println semaforo ":" tiempo-promedio "segundos")))

      (println "Tiempo promedio de cruce total:" tiempo-promedio-cruce "segundos")
      total-autos-val)))

(def tiempo-total 300)
(println (time (calcular-autos crucero vehiculos tiempo-total)))
