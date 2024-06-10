import random as r

crucero4 = ["NS", "SN", "OE", "EO"]
crucero6A = ["SONE", "NESO", "NS", "SN", "OE", "EO"]
crucero6B = ["NOSE", "SENO", "NS", "SN", "OE", "EO"]

def creaArchivo(id, texto):
    nombreArchivo = f"vehiculo{id}.txt"
    with open(nombreArchivo, "w") as archivo:
        archivo.write(texto)

def genera(choice, id, crucero, tLlegadaSmall, tLlegadaBig):
    direccion = r.choice(crucero)
    TiempoTarda = r.randint(6, 13)
    TiempoLlegada = r.randint(tLlegadaSmall, tLlegadaBig)
    carril = r.randint(1, 2)
    texto = f"(Crucero0{choice+1} {direccion} {id} {TiempoTarda} {TiempoLlegada} {carril})"
    creaArchivo(id, texto)

def generaVehiculos():
    for i in range(200):
        crucero = r.randint(0, 6)  # Cambiado a 6 para incluir los casos adicionales
        if crucero == 0:
            genera(crucero, i, crucero6A, 0, 67)
        elif crucero == 1:
            genera(crucero, i, crucero6B, 0, 119)
        elif crucero == 2:
            genera(crucero, i, crucero4, 0, 71)
        elif crucero == 3:
            genera(crucero, i, crucero4, 0, 44)
        elif crucero == 4:
            genera(crucero, i, crucero4, 0, 56)
        elif crucero == 5:
            genera(crucero, i, crucero4, 0, 66)
        elif crucero == 6:
            genera(crucero, i, crucero4, 0, 46)
    print("Creación de Vehículos completada")

generaVehiculos()
