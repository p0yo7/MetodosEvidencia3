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
    (fn [acc [crucero semaforo tiempo]]
      (update-in acc [crucero semaforo] #(conj % tiempo)))
    {}
    vehiculos))

(defn calcular-cantidad-vehiculos [datos]
  (into {}
    (pmap
      (fn [[crucero vehiculos]]
        [crucero
          (into {}
            (for [[semaforo tiempos] vehiculos]
              [semaforo (count tiempos)]))])
      datos)))

(defn calcular-tiempo-promedio [datos]
  (into {}
    (pmap
      (fn [[crucero vehiculos]]
        [crucero
          (into {}
            (for [[semaforo tiempos] vehiculos]
              [semaforo (/ (apply + tiempos) (count tiempos))]))])
      datos)))

(defn calcular-semáforos-verdes-sin-vehículos [datos]
  (into {}
    (pmap
      (fn [[crucero vehiculos]]
        [crucero
          (count
            (filter
              (fn [[semaforo tiempos]]
                (and (zero? (count tiempos)) (not= 0 (count (keys tiempos)))))
              vehiculos))])
      datos)))

(defn guardar-en-txt [resultado-crucero resultado-general]
  (let [n (count resultado-crucero)
        cruceros (range 1 (inc n))]
    (doseq [crucero cruceros]
      (spit (str "resultadoCrucero" crucero ".txt") (get resultado-crucero crucero)))
    (spit "resultadosGeneral.txt" resultado-general)))

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
            semaforos-verdes-sin-vehiculos (calcular-semáforos-verdes-sin-vehículos datos-vehiculos)]
        (println "Cantidad de vehículos que pasaron por cada crucero y semáforo:" cantidad-vehiculos)
        (println "Tiempo promedio de cruce por cada crucero y semáforo:" tiempo-promedio)
        (println "Cantidad de veces que el semáforo estuvo en verde pero no pasaron vehículos:" semaforos-verdes-sin-vehiculos)
        
        ;; Solicitar al usuario el crucero que desea visualizar
        (println "Ingrese el número del crucero que desea visualizar:")
        (let [crucero (read-line)
              resultado-crucero (get tiempo-promedio (read-string crucero))
              resultado-general (str "Cantidad de vehículos que pasaron por cada crucero y semáforo: " cantidad-vehiculos
                                     "\nTiempo promedio de cruce por cada crucero y semáforo: " tiempo-promedio
                                     "\nCantidad de veces que el semáforo estuvo en verde pero no pasaron vehículos: " semaforos-verdes-sin-vehiculos)]
          (guardar-en-txt {crucero resultado-crucero} resultado-general)))
      (catch Exception e
        (println "Error durante el análisis:" (.getMessage e))))))

;; Ejecutar la función para iniciar el análisis
(iniciar-analisis)

(defn foo [] (println "Completado!"))
