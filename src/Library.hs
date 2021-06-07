module Library where
import PdePreludat

data Gimnasta = UnGimnasta{
    nombre :: Nombre
,   edad :: Number
,   peso :: Kilos
,   tonificacion :: Tonificacion
} deriving(Show)

--                     Nombre   Edad Peso Tonif
pancho = UnGimnasta "Francisco" 40.0 120.0 1.0
andres = UnGimnasta "Andy" 22.0 80.0 6.0
juancito = UnGimnasta "Andy" 22.0 105.0 5.0

{-------------------------------Punto 1-------------------------------}
saludable :: Gimnasta -> Bool
saludable gimnasta = (not.estaObeso) gimnasta && ((>5).tonificacion) gimnasta

estaObeso :: Gimnasta -> Bool
estaObeso = (>100).peso

{-
> saludable pancho 
False 
> saludable andres 
True
-}

{-------------------------------Punto 2-------------------------------}
type Calorias = Number

quemarCalorias :: Gimnasta -> Calorias -> Gimnasta
quemarCalorias gimnasta calorias | estaObeso gimnasta = bajarPeso gimnasta (div calorias 150)
                                 | edad gimnasta > 30 && calorias > 200 = bajarPeso gimnasta 1
                                 | otherwise = bajarPeso gimnasta (div calorias (edad gimnasta * peso gimnasta))

bajarPeso :: Gimnasta -> Kilos -> Gimnasta
bajarPeso gimnasta kilos = gimnasta {peso=peso gimnasta - kilos}

{-
> quemarCalorias pancho 300
Gimnasta "Francisco" 40.0 118.0 1.0
> quemarCalorias andres 300 
Gimnasta "Andy" 22.0 79.8 6.0
-}

{-------------------------------Punto 3-------------------------------}
type Minutos = Number
type Ejercicio = Minutos -> Gimnasta -> Gimnasta
hacerUnEjercicio :: Minutos -> Gimnasta -> Ejercicio -> Gimnasta
--hacerUnEjercicio :: Ejercicio -> Minutos -> Gimnasta -> Gimnasta
hacerUnEjercicio minutos gimnasta ejercicio = ejercicio minutos gimnasta

caminataEnCinta :: Ejercicio
caminataEnCinta minutos gimnasta = quemarCalorias gimnasta (1*5*minutos)

{-
> caminataEnCinta 40 pancho 
Gimnasta "Francisco" 40.0 118.6 1.0
-}

entrenamientoEnCinta :: Ejercicio
entrenamientoEnCinta minutos gimnasta = quemarCalorias gimnasta (calcularCaloriasEntrenamientoCinta minutos)

calcularCaloriasEntrenamientoCinta :: Minutos -> Calorias
calcularCaloriasEntrenamientoCinta minutos = (1*((6+(velMax minutos))/2)*minutos)

velMax :: Minutos -> Number
velMax minutos = 6 + (div minutos 5)

{-
> entrenamientoEnCinta 40 pancho 
Gimnasta "Francisco" 40.0 117.3 1.0
-}

type Kilos = Number
pesas :: Kilos -> Ejercicio
pesas kilos minutos gimnasta | minutos > 10 = tonificar gimnasta (div kilos 10) 
                             | otherwise = gimnasta

tonificar :: Gimnasta -> Tonificacion -> Gimnasta
tonificar gimnasta cantidad = gimnasta {tonificacion=tonificacion gimnasta + cantidad}

{-
> pesas 50 15 pancho 
Gimnasta "Francisco" 40.0 120.0 6.0 ­­­ --tonifica 5 (50 / 10) 
-}

type Inclinacion = Number

colina :: Inclinacion -> Ejercicio
colina inclinacion minutos gimnasta = quemarCalorias gimnasta (2*minutos*inclinacion)

{-
> colina 5 40 pancho 
Gimnasta "Francisco" 40.0 117.3 1.0  --quema 400 calorías (2*40*5) 
-}

montania :: Inclinacion -> Ejercicio
montania inclinacion minutos = flip tonificar 1.colina (inclinacion+3) (div minutos 2).colina inclinacion (div minutos 2)

{-
> montania 5 40 pancho 
Gimnasta "Francisco" 40.0 116.5 2.0
-}

{-------------------------------Punto 4-------------------------------}
data Rutina = UnaRutina {
    nombreRutina :: String
,   duracion :: Minutos
,   ejercicios :: [Ejercicio]
}

lunes = UnaRutina "La de los lunes" 60 [caminataEnCinta,entrenamientoEnCinta,pesas 50,colina 30,montania 25]
martes = UnaRutina "La de los martes" 30 [caminataEnCinta,pesas 50]
miercoles = UnaRutina "La de los miercoles" 10 [colina 30,montania 25]
jueves = UnaRutina "La de los jueves" 150 [caminataEnCinta,entrenamientoEnCinta,montania 25]
semana = [lunes,martes,miercoles,jueves]

realizarRutinaConRecursividad :: Rutina -> Gimnasta -> Gimnasta
realizarRutinaConRecursividad rutina gimnasta = hacerEjercicios (calcularMinutosPorEjecicio rutina) (ejercicios rutina) gimnasta

hacerEjercicios :: Minutos -> [Ejercicio] -> Gimnasta -> Gimnasta
hacerEjercicios minutosPorEjercicio (ejercicio: []) gimnasta = ejercicio minutosPorEjercicio gimnasta
hacerEjercicios minutosPorEjercicio (ejercicio:ejercicios) gimnasta = ((hacerEjercicios minutosPorEjercicio ejercicios).(ejercicio minutosPorEjercicio)) gimnasta

realizarRutinaConFoldl :: Rutina -> Gimnasta -> Gimnasta
realizarRutinaConFoldl rutina gimnasta = foldl (hacerUnEjercicio (calcularMinutosPorEjecicio rutina)) gimnasta (ejercicios rutina)

calcularMinutosPorEjecicio :: Rutina -> Minutos
calcularMinutosPorEjecicio rutina = div (duracion rutina) ((length.ejercicios) rutina)

{-------------------------------Punto 5-------------------------------}
type Nombre = String
type Tonificacion = Number
type Resumen = (Nombre,Kilos,Tonificacion)
resumenDeRutina :: Gimnasta -> Rutina -> Resumen
resumenDeRutina gimnasta rutina = armarTupla (nombreRutina rutina) gimnasta (realizarRutinaConFoldl rutina gimnasta)

armarTupla :: String -> Gimnasta -> Gimnasta -> Resumen
armarTupla nombreDeRutina gimnastaOriginal gimnastaPostRutina = (nombreDeRutina,deltaSegun peso gimnastaOriginal gimnastaPostRutina, deltaSegun tonificacion gimnastaPostRutina gimnastaOriginal)

deltaSegun :: (Gimnasta -> Number) -> Gimnasta -> Gimnasta -> Number
deltaSegun criterio gimnasta1 gimnasta2 = criterio gimnasta1 - criterio gimnasta2

{-------------------------------Punto 6-------------------------------}
resumenDeLasSaludables :: [Rutina] -> Gimnasta -> [Resumen]
resumenDeLasSaludables rutinas gimnasta = ((map (resumenDeRutina gimnasta)).(filter (rutinaEsSaludable gimnasta))) rutinas

rutinaEsSaludable :: Gimnasta -> Rutina -> Bool
rutinaEsSaludable gimnasta rutina = (saludable.(realizarRutinaConFoldl rutina)) gimnasta
{-
Dada una lista de rutinas, obtener un resumen de todas las que (individualmente) pueden
llevar a un gimnasta dado a estar saludable. 
-}