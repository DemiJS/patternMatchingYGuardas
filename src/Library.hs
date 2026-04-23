module Library where
import PdePreludat
import GHC.Base (Int, Float)

doble :: Number -> Number
doble numero = numero + numero

-- se recibe un salario y se devuelve otro modificado

-- si empleado < 3y , 50.000 por año
-- si empleado > 3y , 150.000 x 3 + 10.000 por año
-- si empleado cat "a", suma 20.000
-- si empleado cat "b", suma 10.000
-- aumento fijo : suma 1.000

bonoPorAntiguedad :: Number -> Number -> Number
bonoPorAntiguedad años salario 
    | años > 3 =  salario + 150000 + (10000 * años)
    | otherwise = salario + 50000 * años 

bonoPorCategoria :: Char -> Number -> Number 
bonoPorCategoria 'a' salario = salario + 20000
bonoPorCategoria 'b' salario = salario + 10000
bonoPorCategoria _ salario = salario

aumentoFijo :: Number -> Number
aumentoFijo = (+) 1000

salarioFinal :: Number -> Char -> Number -> Number
salarioFinal años cat = aumentoFijo . bonoPorCategoria cat . bonoPorAntiguedad años

sePuedeAscender :: Number -> Number -> Number -> Bool
sePuedeAscender años salario salarioTope = años > 3 && salario > salarioTope