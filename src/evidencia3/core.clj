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
                                    (float (/ (apply + (map #(max 6 (min 13 (- (:tiempo-llegada %) (:tiempo-cruce %)))) tiempos)) (count tiempos)))
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

(defn formatear-salida [crucero cantidad-vehiculos tiempo-promedio semaforos-verdes]
  (let [total-autos (:total cantidad-vehiculos)
        total-semaforos (count (keys (dissoc cantidad-vehiculos :total)))
        promedio-total (float (/ (reduce + (vals tiempo-promedio)) (if (pos? total-semaforos) total-semaforos 1)))]
    (str "Crucero: " crucero "\n"
         "Cantidad de autos por semáforo:\n"
         (str/join "\n" (map #(str (key %) ": " (val %)) (dissoc cantidad-vehiculos :total))) "\n"
         "Total de autos en 300 segundos: " total-autos "\n"
         "Semáforos en verde sin flujo de autos:\n"
         (str/join "\n" (map #(str (key %) ": " (get semaforos-verdes (key %))) (dissoc cantidad-vehiculos :total))) "\n"
         "Tiempo promedio de cruce por semáforo:\n"
         (str/join "\n" (map #(str (key %) ": " (format "%.6f" (float (val %))) " segundos") tiempo-promedio)) "\n"
         "Tiempo promedio de cruce total: " (format "%.6f" promedio-total) " segundos\n")))

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
        (doseq [crucero (keys cantidad-vehiculos)]
          (println (formatear-salida crucero (get cantidad-vehiculos crucero) (get tiempo-promedio crucero) (get semaforos-verdes-sin-vehiculos crucero)))))
      (catch Exception e
        (println "Error durante el análisis:" (.getMessage e))))))

;; Ejecutar la función para iniciar el análisis
(iniciar-analisis)

(defn foo [] (println "Completado!"))
