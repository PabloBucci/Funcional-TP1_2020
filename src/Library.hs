module Library
    (Auto (..) ,
    anio, 
    ) where

import PdePreludat

type Desgaste = Float
type Patente = String
type Fecha = (Int, Int, Int)

-- Definiciones base
anio :: Fecha -> Int
anio (_, _, year) = year

data Auto = Auto {
    patente :: Patente,
    desgasteLlantas :: [Desgaste],
    rpm :: Int,
    temperaturaAgua :: Int,
    ultimoArreglo :: Fecha
} deriving Show

-- ==========================================================================================================================================
-- PUNTO 1: Costo de la reparación de un auto
-- ==========================================================================================================================================
-- (común)

-- Saber el costo de reparación de un auto
-- ●	si la patente tiene 7 dígitos, es $ 12.500
-- ●	si no, si la patente está entre las letras "DJ" y "NB", se aplica el calculoPatental
--      ○	que es $ 3.000 * la longitud para las patentes que terminen en 4
--      ○	o $ 20.000 para el resto de las patentes
-- ●	de lo contrario, se le cobra $ 15000

-- Importante: tenés que usar composición en las funciones auxiliares



-- ==========================================================================================================================================
-- PUNTO 2: Auto Peligroso / Necesita revision
-- ==========================================================================================================================================
-- ATENCIÓN: Resolver únicamente con Composición y aplicación parcial
-- Parte 1) Auto peligroso (integrante a)
-- Dado un auto, saber si es peligroso. Esta condición se cumple cuando el desgaste de la primera llanta es mayor a 0.5

autoPeligroso :: Auto -> Bool
autoPeligroso = ((> 0.5).head).desgasteLlantas

-- Parte 2) Necesita revisión (integrante b)
-- Dado un auto, saber si necesita revisión. Esta condición se cumple cuando el último arreglo fue realizado en el año 2015 ó antes.


-- ==========================================================================================================================================
-- PUNTO 3: Personal técnico encargado de las reparaciones
-- ==========================================================================================================================================

type Mecanico = Auto -> Auto

-- Parte 1) Integrante a
-- Necesitamos definir a las siguientes personas que realizan actividades en el taller mecánico:
-- ●	Alfa: hace que el auto regule a 2.000 vueltas, salvo que esté a menos de 2.000 vueltas, en cuyo caso lo deja como está
-- ●	Bravo: cambia todas las cubiertas, dejándolas sin desgaste
-- ●	Charly:  realiza las mismas actividades que Alfa y Bravo

alfa :: Mecanico
alfa auto | ((>2000).rpm) auto = auto {rpm = 2000}
          | otherwise = auto

bravo :: Mecanico
bravo auto = auto {desgasteLlantas = map (*0) (desgasteLlantas auto)}

charly :: Mecanico
charly auto = foldr ($) auto [alfa, bravo]

-- Parte 2) Integrante b
-- Necesitamos definir a las siguientes personas que realizan actividades en el taller mecánico
-- ●	Tango: le gusta decir que hizo muchas cosas pero en realidad no hace ningún arreglo
-- ●	Zulu: revisa la temperatura del agua, la deja a 90 y hace lo mismo que Lima (ver a continuación)
-- ●	Lima:  cambia las cubiertas delanteras (las dos primeras), dejándolas sin desgaste. Las posteriores quedan igual


-- ==========================================================================================================================================
-- PUNTO 4: Ordenamiento TOC de autos
-- ==========================================================================================================================================

-- (Común para ambos integrantes) 
-- Solamente se puede utilizar recursividad en este punto

-- Dada una serie de autos, saber si están ordenados en base al siguiente criterio:
-- ●	los autos ubicados en la posición impar de la lista deben tener una cantidad de desgaste impar
-- ●	los autos ubicados en la posición par deben tener una cantidad de desgaste par
-- ●	asumimos que el primer elemento está en la posición 1, el segundo elemento en la posición 2, etc.

-- La cantidad de desgaste es la sumatoria de desgastes de las cubiertas de los autos multiplicada por 10. Ejemplo: 0.2 + 0.5 + 0.6 + 0.1 = 1.4 * 10 = 14. Para determinar si es par o no (y evitar errores de redondeo) es conveniente utilizar la función round.

-- ==========================================================================================================================================
-- PUNTO 5: Orden de Reparación
-- ==========================================================================================================================================

-- (Común para ambos integrantes) 
-- Aplicar una orden de reparación, que tiene
-- ●	una fecha
-- ●	una lista de técnicos
-- y consiste en que cada uno de los técnicos realice las reparaciones que sabe sobre el auto, al que además hay que actualizarle la última
-- fecha de reparación.


-- ==========================================================================================================================================
-- PUNTO 6: 
-- ==========================================================================================================================================
-- Solamente se puede utilizar funciones de orden superior en este punto.
-- Parte 1) Integrante a: Técnicos que dejan el auto en condiciones
-- Dada una lista de técnicos determinar aquellos técnicos que dejarían el auto en condiciones (que no sea peligroso andar, recordar el punto 2.1 del integrante a).

-- Parte 2) Integrante b: Costo de reparación de autos que necesitan revisión
-- Dada una lista de autos, saber cuál es el costo de reparación de los autos que necesitan revisión.


-- ==========================================================================================================================================
-- PUNTO 7: 
-- ==========================================================================================================================================
-- Parte 1) Integrante a: Técnicos que dejan el auto en condiciones
-- En base al punto “dada una lista de técnicos determinar qué técnicos dejarían el auto en condiciones” y considerando una lista de técnicos
-- infinita, ¿podríamos obtener el primer técnico que deja el auto en condiciones? Muestre un ejemplo y justifique. 

-- Parte 2) Integrante b: Costo de reparación de autos que necesitan revisión
-- En base al punto “Dada una lista de autos, saber cuál es el costo de reparación de los autos que necesitan revisión.”,  ¿podríamos tener 
-- una lista infinita de autos? Muestre un ejemplo y justifique. Y si tomáramos en cuenta los tres primeros autos que necesitan revisión, 
-- ¿cómo debería cambiar la función? Por otra parte, ¿esta versión aceptaría una lista infinita de autos? Modifique la función 6.b con otro nombre
-- y justifique sus respuestas.

-- Respuesta:

-- No podríamos utilizar la función del punto 6b si tenemos una lista infinita, pues map y sum deben recorrer toda la lista.
-- Ahora bien, si tomamos los tres primeros autos que necesitan revisión, se podría realizar el cálculo de Reparación de la Lista de los tres primeros 
-- autos que cumplen con la función "necesitaRevisión", debido a la "lazy evaluation". La nueva función sería:


-- para aplicar esta función, es necesario que hay al menos tres autos que necesiten reparacion.

-- Ayuda: puede crear técnicos y autos infinitos mediante alguna de estas técnicas







-- CASOS DE PRUEBA

fiatRojo :: Auto
fiatRojo = Auto{
    patente = "AT001LN", 
    desgasteLlantas=[2,3,1,1], 
    rpm=110 ,
    temperaturaAgua=30 ,
    ultimoArreglo = (3,12,2015)
}

fiatAzul :: Auto
fiatAzul = Auto{
    patente = "DJV214", 
    desgasteLlantas=[0,1,3,3], 
    rpm=125 ,
    temperaturaAgua=42 ,
    ultimoArreglo = (12,3,2019)
}

fiatVerde :: Auto
fiatVerde = Auto{
    patente = "DJV215", 
    desgasteLlantas=[0,1,1,0], 
    rpm=125 ,
    temperaturaAgua=42 ,
    ultimoArreglo = (12,3,2019)
}

upRojo :: Auto
upRojo = Auto{
    patente = "DFH029", 
    desgasteLlantas=[1,0,0,1], 
    rpm=125 ,
    temperaturaAgua=42 ,
    ultimoArreglo = (12,3,2016)
}

