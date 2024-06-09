(ns evidencia3.core
  (:require [clojure.java.io :as io]
            [clojure.string :as str]))

(defn leerCrucero [ruta]
  (with-open [rdr (io/reader ruta)]
    (let [contenido (slurp rdr)]
      (read-string contenido))))

(defn leerVehiculos [ruta]
  (with-open [rdr (io/reader ruta)]
    (let [contenido (slurp rdr)]
      (read-string contenido))))

(defn procesar-segundo [t crucero-info autos]
  (reduce (fn [acumulador crucero]
            (let [direccion (:direccion crucero)
                  tiempo-verde (:tiempo-verde crucero)
                  ciclo-tiempo (+ (:tiempo-verde crucero) (:tiempo-rojo crucero) (:tiempo-blanco crucero))
                  tiempo-en-ciclo (mod t ciclo-tiempo)
                  inicio-verde? (= tiempo-en-ciclo 0)
                  en-verde? (< tiempo-en-ciclo tiempo-verde)
                  autos-en-verde (filter #(and (= (first %) direccion)
                                               (<= (mod (nth % 2) ciclo-tiempo) tiempo-verde)
                                               (> (nth % 2) t))
                                         autos)
                  autos-pendientes (filter #(and (= (first %) direccion)
                                                 (< (nth % 2) t))
                                           autos)
                  flujo-carros? (not (empty? autos-pendientes))]
              (if (and inicio-verde? (empty? autos-en-verde) (or (not= direccion "OE") (not= direccion "EO")) (not flujo-carros?))
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

(defn calcular-autos [crucero autos tiempo-total]
  (let [crucero-info (map #(hash-map :direccion (nth % 0) 
                                     :tiempo-verde (nth % 1)
                                     :tiempo-rojo (nth % 2)
                                     :tiempo-blanco (nth % 3)) crucero)
        resultados (->> (pmap #(procesar-segundo % crucero-info autos) (range tiempo-total))
                        (reduce (partial merge-with (partial merge-with +))))]
    (spit "resultados.txt"
          (str "Cantidad de autos por semáforo:\n"
               (->> (:total-autos resultados)
                    (map #(str (first %) ": " (second %) "\n"))
                    (apply str))
               "Total de autos en " tiempo-total " segundos: " (apply + (vals (:total-autos resultados))) "\n"
               "Semáforos en verde sin flujo de autos:\n"
               (->> (:semaforos-verde-sin-flujo resultados)
                    (map #(str (first %) ": " (second %) "\n"))
                    (apply str))
               "Tiempo promedio de cruce por semáforo:\n"
               (->> (:tiempo-cruce-por-semaforo resultados)
                    (map (fn [[semaforo tiempo-total]]
                           (let [cantidad ((:total-autos resultados) semaforo)
                                 tiempo-promedio (if (> cantidad 0)
                                                   (float (/ tiempo-total cantidad))
                                                   0)]
                             (str semaforo ": " tiempo-promedio " segundos\n"))))
                    (apply str))
               "Tiempo promedio de cruce total: " (if (> (apply + (vals (:total-autos resultados))) 0)
                                                     (float (/ (apply + (vals (:tiempo-cruce-por-semaforo resultados)))
                                                               (apply + (vals (:total-autos resultados)))))
                                                     0) " segundos\n")))
    (println "Resultados guardados en resultados.txt"))

(defn leer-todos-vehiculos [directorio]
  (mapcat (fn [archivo]
            (leerVehiculos (.getAbsolutePath archivo)))
          (filter #(re-matches #"vehiculo\d+\.txt" (.getName %)) (file-seq (io/file directorio)))))

(defn leer-todos-cruceros [directorio]
  (map (fn [archivo]
         (leerCrucero (.getAbsolutePath archivo)))
       (filter #(re-matches #"crucero\d+\.txt" (.getName %)) (file-seq (io/file directorio)))))

(defn imprimir-lista [lista-titulo lista]
  (println lista-titulo)
  (doseq [elemento lista]
    (println elemento))
  (println))

(defn analizar-cruceros []
  (let [crucero-dir "Documents/Metodos/evidencia3/src/evidencia3/cruceros/"
        vehiculo-dir "Documents/Metodos/evidencia3/src/evidencia3/vehiculos/"
        tiempo-total 300]
    (try
      (let [cruceros (leer-todos-cruceros crucero-dir)
            vehiculos (leer-todos-vehiculos vehiculo-dir)]
        (imprimir-lista "Lista de cruceros:" cruceros)
        (imprimir-lista "Lista de vehículos:" vehiculos)
        (doseq [crucero cruceros]
          (println "Datos del crucero:" crucero)
          (println "Datos de los vehículos:" vehiculos)
          (calcular-autos [crucero] vehiculos tiempo-total)))
      (catch Exception e
        (println "Error leyendo archivos:" (.getMessage e))))))

;; Ejecutar la función para iniciar la interacción
(analizar-cruceros)
(defn foo [] (println "Completado!"))

