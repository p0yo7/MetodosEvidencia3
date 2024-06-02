(ns evidencia3.core
  (:require [clojure.java.io :as io]))

(defn foo []
  (println  "Hello, World!"))

(defn leerArchivo [ruta]
	(with-open [rdr (io/reader ruta)]
		(let [contenido(slurp rdr)]
			(read-string contenido))))
(def inventario (leerArchivo "src/evidencia3/crucero.txt"))
(println inventario)
