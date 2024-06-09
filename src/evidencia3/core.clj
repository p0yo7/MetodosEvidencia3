(ns evidencia3.core
  (:require [clojure.java.io :as io]
            [clojure.string :as str]))

(defn leer-archivo [ruta]
  "Lee el contenido de un archivo y lo convierte en una estructura de datos Clojure."
  (with-open [rdr (io/reader ruta)]
    (let [contenido (slurp rdr)]
      (read-string contenido))))

(defn leer-archivos-en-directorio [directorio regex]
  "Lee todos los archivos en el directorio que coinciden con el regex y retorna una lista de sus contenidos."
  (let [archivos (filter #(re-matches regex (.getName %)) (file-seq (io/file directorio)))]
    (map leer-archivo (map #(.getAbsolutePath %) archivos))))

(defn leer-todos-cruceros [directorio]
  "Lee todos los archivos de cruceros en el directorio y retorna una lista de sus contenidos."
  (leer-archivos-en-directorio directorio #"crucero\d+\.txt"))

(defn leer-todos-vehiculos [directorio]
  "Lee todos los archivos de vehículos en el directorio y retorna una lista de sus contenidos."
  (leer-archivos-en-directorio directorio #"vehiculo\d+\.txt"))

(defn procesar-vehiculos [vehiculos]
  (reduce
    (fn [acc [crucero semaforo id tiempo-cruce tiempo-llegada]]
      (update-in acc [crucero semaforo] #(conj (or % []) {:tiempo-cruce tiempo-cruce :tiempo-llegada tiempo-llegada})))
    {}
    vehiculos))

(defn calcular-cantidad-vehiculos [datos]
  (reduce
    (fn [acc [crucero vehiculos]]
      (let [total-autos (apply + (map (comp count val) vehiculos))]
        (assoc acc crucero
               (merge (into {} (for [[semaforo tiempos] vehiculos]
                                 [semaforo (count tiempos)]))
                      {:total total-autos}))))
    {}
    datos))

(defn calcular-tiempo-promedio [datos]
  (reduce
    (fn [acc [crucero vehiculos]]
      (assoc acc crucero
             (into {} (for [[semaforo tiempos] vehiculos]
                        [semaforo (if (seq tiempos)
                                    (/ (apply + (map #(max 6 (min 13 (- (:tiempo-llegada %) (:tiempo-cruce %)))) tiempos)) (count tiempos))
                                    0)]))))
    {}
    datos))

(defn calcular-semaforos-verdes-sin-vehiculos [datos]
  (reduce
    (fn [acc [crucero vehiculos]]
      (let [sin-flujo (count
                        (filter
                          (fn [[semaforo tiempos]]
                            (empty? tiempos))
                          vehiculos))]
        (assoc acc crucero sin-flujo)))
    {}
    datos))

(defn iniciar-analisis []
  "Inicia el análisis de cruceros y vehículos."
  (let [crucero-dir "src/evidencia3/cruceros/"
        vehiculo-dir "src/evidencia3/vehiculos/"]
    (try
      (let [cruceros (leer-todos-cruceros crucero-dir)
            vehiculos (leer-todos-vehiculos vehiculo-dir)
            datos-vehiculos (procesar-vehiculos vehiculos)
            cantidad-vehiculos (calcular-cantidad-vehiculos datos-vehiculos)
            tiempo-promedio (calcular-tiempo-promedio datos-vehiculos)
            semaforos-verdes-sin-vehiculos (calcular-semaforos-verdes-sin-vehiculos datos-vehiculos)]
        (println "Lista de cruceros:" cruceros)
        (println "Lista de vehículos:" vehiculos)
        (println "Cantidad de vehículos que pasaron por cada crucero y semáforo:" cantidad-vehiculos)
        (println "Tiempo promedio de cruce por cada crucero y semáforo:" tiempo-promedio)
        (println "Cantidad de veces que el semáforo estuvo en verde pero no pasaron vehículos:" semaforos-verdes-sin-vehiculos))
      (catch Exception e
        (println "Error durante el análisis:" (.getMessage e))))))

;; Ejecutar la función para iniciar el análisis
(iniciar-analisis)

(defn foo [] (println "Completado!"))
