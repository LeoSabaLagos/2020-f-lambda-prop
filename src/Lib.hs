module Lib where
import Text.Show.Functions

laVerdad = True

{-
Lambda Prop
Buscar departamentos para alquilar por los medios tradicionales es una tarea compleja, 
ya que requiere mucho tiempo de investigación buscando en los 
clasificados de los diarios y recorriendo inmobiliarias. 

Es por eso que hoy en día cada vez son más las personas que dejaron eso atrás dejando que internet 
se encargue de buscar las supuestas mejores alternativas para sus necesidades.

Por eso surge una nueva página para buscar departamentos 
que permita al usuario personalizar sus propias búsquedas y
de paso eventualmente mandarle mails con las nuevas ofertas 
inmobiliarias que podrían ser de su interés a ver si agarra viaje.
-}

-- Tenemos los departamentos modelados de la siguiente forma:

type Barrio = String
type Mail = String
type Requisito = Depto -> Bool
type Busqueda = [Requisito]

data Depto = Depto { 
  ambientes :: Int,
  superficie :: Int,
  precio :: Int,
  barrio :: Barrio
} deriving (Show, Eq)

data Persona = Persona {
    mail :: Mail,
    busquedas :: [Busqueda]
}

ordenarSegun _ [] = []
ordenarSegun criterio (x:xs) =
  (ordenarSegun criterio . filter (not . criterio x)) xs ++
  [x] ++
  (ordenarSegun criterio . filter (criterio x)) xs

between cotaInferior cotaSuperior valor =
  valor <= cotaSuperior && valor >= cotaInferior

{- deptosDeEjemplo = [
  Depto 3 80 7500 "Palermo", 
  Depto 1 45 3500 "Villa Urquiza", 
  Depto 2 50 5000 "Palermo", 
  Depto 1 45 5500 "Recoleta"] -}

depto1 = Depto 3 80 7500 "Palermo"
depto2 = Depto 1 45 3500 "Villa Urquiza"
depto3 = Depto 2 50 5000 "Palermo"
depto4 = Depto 1 45 5500 "Recoleta"

-- Lista de deptos de ejemplo
deptosLeo = [depto1,depto2,depto3,depto4]

-- Se pide desarrollar las siguientes funciones y consultas de modo que se 
-- aprovechen tanto como sea posible
-- los conceptos de orden superior, aplicación parcial y composición.
---------------------------------------------------------------------------------------------------------
-- Punto 1
-- Parte a

type Criterio =  Int -> Int -> Bool

mayor :: Criterio
--mayor n1 n2 = n1 > n2
mayor n1 = (<n1)

menor :: Criterio
--mayor n1 n2 = n1 > n2
menor n1 = (>n1)

-- Parte b
listaStrings :: [String]
listaStrings = ["hola","mmmnnn","lolol","z"]

ordenarSegunLargo :: Criterio -> [String] -> [Int]
-- ordenarSegunLargo criterio lista = ordenarSegun criterio (map length lista)
ordenarSegunLargo criterio = (ordenarSegun criterio).(map length)
---------------------------------------------------------------------------------------------------------
-- Punto 2
-- Parte a

ubicadoEn :: Depto -> [Barrio] -> Bool
--ubicadoEn depto listaBarrios = any (barrio depto ==) listaBarrios 
ubicadoEn depto = any (barrio depto ==)

--Parte b
cumpleRango :: Int -> Int -> (Depto -> Int) -> Depto -> Bool
--cumpleRango cotaInferior cotaSuperior caracteristica depto = between cotaInferior cotaSuperior (caracteristica depto)
cumpleRango cotaInferior cotaSuperior caracteristica = (between cotaInferior cotaSuperior).caracteristica
---------------------------------------------------------------------------------------------------------
-- Punto 3
-- Parte a

-- Requisitos de ejemplo
-- type Requisito = Depto -> Bool
ambientesEntre2y5 :: Requisito
ambientesEntre2y5 = cumpleRango 2 5 ambientes

superficieEntre40y60 :: Requisito
superficieEntre40y60 = cumpleRango 40 60 superficie

precioEntre4000Y20000 :: Requisito
precioEntre4000Y20000 = cumpleRango 4000 20000 precio

busquedaLeo = [ambientesEntre2y5,superficieEntre40y60,precioEntre4000Y20000]

--type Busqueda = [Requisito]

cumpleBusqueda :: Depto -> Busqueda -> Bool
cumpleBusqueda depto = (all cumpleRequisito).(map ($depto))

cumpleRequisito :: Bool -> Bool
cumpleRequisito = (== True)

--Parte b
buscar :: Busqueda -> CriterioDepto -> [Depto] -> [Depto]
buscar busqueda criterio = (ordenarSegun criterio).(deptosQueCumplen busqueda)
 
deptosQueCumplen :: Busqueda -> [Depto] -> [Depto]
--deptosQueCumplen busqueda listaDeptos = filter (`cumpleBusqueda` busqueda) listaDeptos
deptosQueCumplen busqueda = filter (`cumpleBusqueda` busqueda)

-- Criterio de ejemplo
type CriterioDepto = Depto -> Depto -> Bool

mayorSuperficie :: CriterioDepto
-- mayorSuperficie d1 d2 = superficie d1 > superficie d2
mayorSuperficie d1 = (<superficie d1).superficie

-- Parte c
-- Requisitos necesarios

-- Encontrarse en Recoleta o Palermo 
barrioPalermoORecoleta :: Requisito 
barrioPalermoORecoleta depto =  esBarrioPalermo depto || esBarrioRecoleta depto

esBarrioPalermo :: Requisito
esBarrioPalermo = (== "Palermo").barrio

esBarrioRecoleta :: Requisito
esBarrioRecoleta = (== "Recoleta").barrio

-- Ser de 1 o 2 ambientes 
unoODosAmbientes :: Requisito
unoODosAmbientes depto = unAmbiente depto || dosAmbientes depto

unAmbiente :: Requisito
unAmbiente = (==1).ambientes

dosAmbientes :: Requisito
dosAmbientes = (==2).ambientes

-- Alquilarse a menos de $6000 por mes
alquierMenorA6000 :: Requisito
alquierMenorA6000 = cumpleRango 0 6000 precio

busqueda3C :: Busqueda
busqueda3C = [barrioPalermoORecoleta,unoODosAmbientes,alquierMenorA6000]

-- Linea con la que mostraria el ejemplo
-- buscar busqueda3C mayorSuperficie deptosLeo

---------------------------------------------------------------------------------------------------------
-- Punto 4
-- Otros requisitos
alquierMenorA1000 :: Requisito 
alquierMenorA1000 = (<1000).precio

barrioLanus :: Requisito
barrioLanus = (=="Lanus").barrio

-- Otra busqueda de ejemplo
busquedaRatona = [alquierMenorA1000,barrioLanus]

-- Personas de ejemplo
leo :: Persona
leo = Persona "lsabalagos@gmail.com" [busquedaLeo]

ale :: Persona
ale = Persona "alesiosaba@gmail.com" [busquedaRatona]

personasEjemplo :: [Persona]
personasEjemplo = [leo,ale]

mailsDePersonasInteresadas :: Depto -> [Persona] -> [Mail]
mailsDePersonasInteresadas depto = obtenerMails.(personasConBusquedaCumplida depto)

personasConBusquedaCumplida :: Depto -> [Persona] -> [Persona]
personasConBusquedaCumplida depto = filter (cumpleBusquedasPersona depto) 

cumpleBusquedasPersona :: Depto -> Persona -> Bool
cumpleBusquedasPersona depto = (any (cumpleBusqueda depto)).busquedas 

obtenerMails :: [Persona] -> [Mail]
obtenerMails = map mail
---------------------------------------------------------------------------------------------------------