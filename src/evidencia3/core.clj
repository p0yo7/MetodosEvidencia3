(ns evidencia3.core
  (:require [clojure.java.io :as io]
            [clojure.string :as str]
            [clojure.edn :as edn]
            [clojure.core.async :refer [chan go go-loop put! <! close!]]))

(defn leer-archivo [ruta]
  "Lee el contenido de un archivo y lo convierte en una estructura de datos Clojure."
  (try
    (with-open [rdr (io/reader ruta)]
      (let [contenido (slurp rdr)]
        (edn/read-string contenido)))
    (catch Exception e
      (println "Error en leer-archivo:" (.getMessage e))
      (throw e))))

(defn leer-archivos-en-directorio [directorio regex]
  "Lee todos los archivos en el directorio que coinciden con el regex y retorna una lista de sus contenidos."
  (try
    (let [archivos (filter #(re-matches regex (.getName %)) (file-seq (io/file directorio)))]
      (map leer-archivo (map #(.getAbsolutePath %) archivos)))
    (catch Exception e
      (println "Error en leer-archivos-en-directorio:" (.getMessage e))
      (throw e))))

(defn leer-todos-cruceros [directorio]
  "Lee todos los archivos de cruceros en el directorio y retorna una lista de sus contenidos."
  (try
    (leer-archivos-en-directorio directorio #"crucero\d+\.txt")
    (catch Exception e
      (println "Error en leer-todos-cruceros:" (.getMessage e))
      (throw e))))

(defn leer-todos-vehiculos [directorio]
  "Lee todos los archivos de vehículos en el directorio y retorna una lista de sus contenidos."
  (try
    (leer-archivos-en-directorio directorio #"vehiculo\d+\.txt")
    (catch Exception e
      (println "Error en leer-todos-vehiculos:" (.getMessage e))
      (throw e))))

(defn procesar-vehiculos [vehiculos]
  (try
    (reduce
      (fn [acc [crucero semaforo id tiempo-cruce tiempo-llegada carril]]
        (update-in acc [crucero semaforo carril] #(conj (or % []) {:tiempo-cruce tiempo-cruce :tiempo-llegada tiempo-llegada})))
      {}
      vehiculos)
    (catch Exception e
      (println "Error en procesar-vehiculos:" (.getMessage e))
      (throw e))))

(defn contar-vehiculos [vehiculos]
  "Cuenta el número total de vehículos en la lista."
  (count vehiculos))

(defn calcular-cantidad-vehiculos [datos]
  (try
    (reduce
      (fn [acc [crucero semaforos]]
        (let [total-autos (apply + (map (fn [[_ carriles]] (apply + (map (fn [[_ tiempos]] (count tiempos)) carriles))) semaforos))]
          (assoc acc crucero
                 (merge (into {} (for [[semaforo carriles] semaforos
                                       [carril tiempos] carriles]
                                   [(str semaforo "-" carril) (count tiempos)]))
                        {:total total-autos}))))
      {}
      datos)
    (catch Exception e
      (println "Error en calcular-cantidad-vehiculos:" (.getMessage e))
      (throw e))))

(defn calcular-tiempo-promedio [datos]
  (try
    (reduce
      (fn [acc [crucero semaforos]]
        (assoc acc crucero
               (into {} (for [[semaforo carriles] semaforos
                              [carril tiempos] carriles]
                          [(str semaforo "-" carril) (if (seq tiempos)
                                                        (float (/ (apply + (map #(max 6 (min 13 (- (:tiempo-llegada %) (:tiempo-cruce %)))) tiempos)) (count tiempos)))
                                                        0)]))))
      {}
      datos)
    (catch Exception e
      (println "Error en calcular-tiempo-promedio:" (.getMessage e))
      (throw e))))

(defn calcular-semaforos-verdes-sin-vehiculos [datos]
  (try
    (reduce
      (fn [acc [crucero semaforos]]
        (let [sin-flujo (count
                          (filter
                            (fn [[_ carriles]]
                              (every? empty? (vals carriles)))
                            semaforos))]
          (assoc acc crucero sin-flujo)))
      {}
      datos)
    (catch Exception e
      (println "Error en calcular-semaforos-verdes-sin-vehiculos:" (.getMessage e))
      (throw e))))

(defn formatear-salida [crucero cantidad-vehiculos tiempo-promedio semaforos-verdes]
  (try
    (let [total-autos (:total cantidad-vehiculos)
          total-semaforos (count (keys (dissoc cantidad-vehiculos :total)))
          promedio-total (float (/ (reduce + (vals tiempo-promedio)) (if (pos? total-semaforos) total-semaforos 1)))
          detalle-semaforo (str "Cantidad de autos por semáforo:\n"
                                (str/join "\n" (map #(str (key %) ": " (val %)) (dissoc cantidad-vehiculos :total))) "\n"
                                "Total de autos: " total-autos "\n"
                                "Semáforos en verde sin flujo de autos:\n"
                                (str/join "\n" (map #(str (key %) ": " (if (get semaforos-verdes (key %)) "" "")) (dissoc cantidad-vehiculos :total))) "\n"
                                "Tiempo promedio de cruce por semáforo:\n"
                                (str/join "\n" (map #(str (key %) ": " (format "%.6f" (float (val %))) " segundos") tiempo-promedio)) "\n"
                                "Tiempo promedio de cruce total: " (format "%.6f" promedio-total) " segundos\n")]
      (str "Crucero: " crucero "\n"
           detalle-semaforo))
    (catch Exception e
      (println "Error en formatear-salida:" (.getMessage e))
      (throw e))))

(defn calcular-10-mejor-peor [tiempos-promedio]
  (try
    (let [sorted-tiempos (sort-by second tiempos-promedio)
          total-cruceros (count sorted-tiempos)
          diez-porciento (max 1 (int (Math/ceil (* 0.1 total-cruceros))))
          mejores-cruceros (take diez-porciento sorted-tiempos)
          peores-cruceros (take-last diez-porciento sorted-tiempos)]
      {:mejores mejores-cruceros :peores peores-cruceros})
    (catch Exception e
      (println "Error en calcular-10-mejor-peor:" (.getMessage e))
      (throw e))))

(defn iniciar-simulacion [datos-vehiculos]
  "Inicia la simulación mostrando los eventos en el tiempo."
  (try
    (let [eventos (chan)]
      (doseq [[crucero semaforos] datos-vehiculos]
        (doseq [[semaforo carriles] semaforos]
          (doseq [[carril tiempos] carriles]
            (doseq [evento tiempos]
              (put! eventos {:crucero crucero :semaforo semaforo :carril carril :tiempo-llegada (:tiempo-llegada evento)})))))
      (go-loop []
        (when-let [evento (<! eventos)]
          (println (str "Tiempo relativo " (:tiempo-llegada evento) ": Crucero " (:crucero evento) ", Semáforo " (:semaforo evento) ", Carril " (:carril evento) " tiene el semáforo en verde."))
          (recur))))
    (catch Exception e
      (println "Error en iniciar-simulacion:" (.getMessage e))
      (throw e))))

(defn imprimir-crucero-solicitado [cantidad-vehiculos tiempo-promedio semaforos-verdes crucero-id]
  (try
    (let [crucero-solicitado (get cantidad-vehiculos crucero-id)]
      (if crucero-solicitado
        (let [salida (formatear-salida crucero-id crucero-solicitado (get tiempo-promedio crucero-id) (get semaforos-verdes crucero-id))]
          (println salida) ; Imprimir en consola
          (spit "resultados.txt" (str salida "\n") :append true) ; Guardar en archivo
          (println "Información del crucero solicitada guardada en resultados.txt."))
        (println "No se encontraron datos para el crucero solicitado.")))
    (catch Exception e
      (println "Error en imprimir-crucero-solicitado:" (.getMessage e))
      (throw e))))

(defn calcular-tiempo-muerto [cruce vehiculos]
  (let [[direccion t-verde t-amarillo t-rojo t-blanco t-inicio] cruce
        t-fin-verde (+ t-inicio t-verde)
        vehiculos-en-cruce (filter #(= direccion (second %)) vehiculos)
        tiempos-cruce (map #(vector (nth % 4) (+ (nth % 4) (nth % 3))) vehiculos-en-cruce)
        tiempos-validos (filter (fn [[llegada salida]]
                                  (and (<= t-inicio llegada t-fin-verde)
                                       (<= t-inicio salida t-fin-verde)))
                                tiempos-cruce)
        intervalos-validos (mapcat (fn [[llegada salida]]
                                     (range llegada (inc salida)))
                                   tiempos-validos)
        tiempo-verde (range t-inicio (inc t-fin-verde))
        tiempos-muertos (filter (fn [t] (not (some #{t} intervalos-validos))) tiempo-verde)]
    (count tiempos-muertos)))

(defn tiempo-muerto [cruceros vehiculos]
  (map (fn [crucero]
         (map (fn [cruce]
                (calcular-tiempo-muerto cruce vehiculos))
              crucero))
       cruceros))

(defn iniciar-analisis []
  "Inicia el análisis de cruceros y vehículos."
  (println "Ingrese el número de crucero del cual desea ver detalles:")
  (let [crucero-numero (read-line)
        crucero-id (format "Crucero%02d" (Integer/parseInt crucero-numero))
        crucero-dir "src/evidencia3/cruceros/"
        vehiculo-dir "src/evidencia3/vehiculos/"]
    (try
      (let [cruceros-futuro (future (leer-todos-cruceros crucero-dir))
            vehiculos-futuro (future (leer-todos-vehiculos vehiculo-dir))
            cruceros @cruceros-futuro
            vehiculos @vehiculos-futuro
            total-vehiculos (contar-vehiculos vehiculos)
            datos-vehiculos (procesar-vehiculos vehiculos)]
        (println "Total de vehículos procesados:" total-vehiculos)
        (println "Vehículos procesados:" datos-vehiculos)
        (let [cantidad-vehiculos (calcular-cantidad-vehiculos datos-vehiculos)
              tiempo-promedio (calcular-tiempo-promedio datos-vehiculos)
              semaforos-verdes-sin-vehiculos (calcular-semaforos-verdes-sin-vehiculos datos-vehiculos)
              tiempos-promedio-cruceros (map (fn [crucero]
                                               [crucero (let [valores (vals (get tiempo-promedio crucero))]
                                                          (if (seq valores)
                                                            (float (/ (reduce + valores) (count valores)))
                                                            0))])
                                             (keys tiempo-promedio))
              top-cruceros (calcular-10-mejor-peor tiempos-promedio-cruceros)
              tiempos-muertos (tiempo-muerto cruceros vehiculos)
              resultados (str/join "\n"
                                   (for [crucero (keys cantidad-vehiculos)]
                                     (formatear-salida crucero (get cantidad-vehiculos crucero) (get tiempo-promedio crucero) (get semaforos-verdes-sin-vehiculos crucero))))
              top-mejores (str "10% de cruceros con menor tiempo de espera:\n"
                               (str/join "\n" (map #(str (first %) ": " (second %) " segundos") (:mejores top-cruceros))))
              top-peores (str "10% de cruceros con mayor tiempo de espera:\n"
                              (str/join "\n" (map #(str (first %) ": " (second %) " segundos") (:peores top-cruceros))))]

          ;; Iniciar la simulación para todos los cruceros
          (iniciar-simulacion datos-vehiculos)

          ;; Esperar un poco para asegurar que la simulación haya impreso todos los eventos
          (Thread/sleep 1000)

          ;; Imprimir la información del crucero solicitado
          (imprimir-crucero-solicitado cantidad-vehiculos tiempo-promedio semaforos-verdes-sin-vehiculos crucero-id)

          ;; Imprimir y guardar los top 10% de cruceros
          (println top-mejores)
          (println top-peores)

          ;; Guardar todos los resultados en resultados.txt
          (spit "resultados.txt" (str resultados "\n" top-mejores "\n" top-peores "\nTiempos muertos:\n" 
                                      (str/join "\n" (map-indexed (fn [idx tm]
                                                                    (str "Crucero " (inc idx) ": " (str/join ", " tm)))
                                                                  tiempos-muertos))) :append true)))
      (catch Exception e
        (println "Error en iniciar-analisis:" (.getMessage e))))))
;; Ejecutar la función para iniciar el análisis
(iniciar-analisis)

(defn foo [] (println "Completado!"))