(ns evidencia3.core
  (:require [clojure.java.io :as io]
            [clojure.string :as str]
            [clojure.edn :as edn]))

(defn leer-archivo [ruta]
  "Lee el contenido de un archivo y lo convierte en una estructura de datos Clojure."
  (with-open [rdr (io/reader ruta)]
    (let [contenido (slurp rdr)]
      (edn/read-string contenido))))

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
        promedio-total (float (/ (reduce + (vals tiempo-promedio)) (if (pos? total-semaforos) total-semaforos 1)))
        detalle-semaforo (str "Cantidad de autos por semáforo:\n"
                              (str/join "\n" (map #(str (key %) ": " (val %)) (dissoc cantidad-vehiculos :total))) "\n"
                              "Total de autos: " total-autos "\n"
                              "Semáforos en verde sin flujo de autos:\n"
                              (str/join "\n" (map #(str (key %) ": " (get semaforos-verdes (key %))) (dissoc cantidad-vehiculos :total))) "\n"
                              "Tiempo promedio de cruce por semáforo:\n"
                              (str/join "\n" (map #(str (key %) ": " (format "%.6f" (float (val %))) " segundos") tiempo-promedio)) "\n"
                              "Tiempo promedio de cruce total: " (format "%.6f" promedio-total) " segundos\n")]
    (str "Crucero: " crucero "\n"
         detalle-semaforo)))

(defn calcular-10-mejor-peor [tiempos-promedio]
  (let [sorted-tiempos (sort-by second tiempos-promedio)
        total-cruceros (count sorted-tiempos)
        diez-porciento (max 1 (int (Math/ceil (* 0.1 total-cruceros))))
        mejores-cruceros (take diez-porciento sorted-tiempos)
        peores-cruceros (take-last diez-porciento sorted-tiempos)]
    {:mejores mejores-cruceros :peores peores-cruceros}))

(defn iniciar-analisis []
  "Inicia el análisis de cruceros y vehículos."
  (println "Ingrese el número de crucero del cual desea ver detalles:")
  (let [crucero-numero (read-line)
        crucero-id (str "Crucero" (format "%02d" (Integer. crucero-numero)))
        crucero-dir "src/evidencia3/cruceros/"
        vehiculo-dir "src/evidencia3/vehiculos/"]
    (try
      (let [cruceros (leer-todos-cruceros crucero-dir)
            vehiculos (leer-todos-vehiculos vehiculo-dir)
            datos-vehiculos (procesar-vehiculos vehiculos)
            cantidad-vehiculos (calcular-cantidad-vehiculos datos-vehiculos)
            tiempo-promedio (calcular-tiempo-promedio datos-vehiculos)
            semaforos-verdes-sin-vehiculos (calcular-semaforos-verdes-sin-vehiculos datos-vehiculos)
            tiempos-promedio-cruceros (map (fn [crucero]
                                             [crucero (let [valores (vals (get tiempo-promedio crucero))]
                                                        (if (seq valores)
                                                          (float (/ (reduce + valores) (count valores)))
                                                          0))])
                                           (keys tiempo-promedio))
            top-cruceros (calcular-10-mejor-peor tiempos-promedio-cruceros)
            resultados (str/join "\n"
                                 (for [crucero (keys cantidad-vehiculos)]
                                   (formatear-salida crucero (get cantidad-vehiculos crucero) (get tiempo-promedio crucero) (get semaforos-verdes-sin-vehiculos crucero))))
            top-mejores (str "10% de cruceros con menor tiempo de espera:\n"
                             (str/join "\n" (map #(str (first %) ": " (second %) " segundos") (:mejores top-cruceros))))
            top-peores (str "10% de cruceros con mayor tiempo de espera:\n"
                            (str/join "\n" (map #(str (first %) ": " (second %) " segundos") (:peores top-cruceros))))]
        ;; Imprimir solo el crucero solicitado y los top 10% en la consola
        (let [crucero-solicitado (get cantidad-vehiculos crucero-id)]
          (if crucero-solicitado
            (println (formatear-salida crucero-id crucero-solicitado (get tiempo-promedio crucero-id) (get semaforos-verdes-sin-vehiculos crucero-id)))
            (println "No se encontraron datos para el crucero solicitado.")))
        (println top-mejores)
        (println top-peores)
        ;; Guardar todos los resultados en un archivo de texto
        (spit "resultados.txt" (str resultados "\n" top-mejores "\n" top-peores)))
      (catch Exception e
        (println "Error durante el análisis:" (.getMessage e))))))

;; Ejecutar la función para iniciar el análisis
(iniciar-analisis)

(defn foo [] (println "Completado!"))
