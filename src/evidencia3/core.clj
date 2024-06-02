(ns evidencia3.core
  (:require [clojure.java.io :as io]))

(defn foo []
  (println  "Hello, World!"))

(defn leerCrucero [ruta]
	(with-open [rdr (io/reader ruta)]
		(let [contenido(slurp rdr)]
			(read-string contenido))))
(def crucero (leerCrucero "src/evidencia3/crucero.txt"))
(println crucero)


(defn leerVehiculos [ruta]
	(with-open [rdr (io/reader ruta)]
		(let [contenido(slurp rdr)]
			(read-string contenido))))
(def vehiculos (leerVehiculos "src/evidencia3/vehiculos.txt"))
(println vehiculos)
