module Library where
import PdePreludat

-- 1.1. Modelar los tipos de los bloques, las transacciones y las cuentas.
data Cuenta = Cuenta {
    identificador :: String,
    saldo :: Number
} deriving (Show, Eq)

type Bloque = [(String, Transaccion)]

type BlockChain = [Bloque]

type Transaccion = Cuenta -> Cuenta

-- 1.2. Las transacciones... Para simplificar el modelo, el intercambio estará separado en transacciones de cobro y pago:
alterarSaldo importe cuenta = cuenta {saldo = saldo cuenta + importe}

-- 1.2.1. Hacer una función de pago, que reciba un número y una cuenta, y devuelva la cuenta con esa cantidad menos de Currycoins.
pago importe = alterarSaldo (-importe)

-- 1.2.2. Hacer una función de cobranza, que reciba un número y una cuenta, y devuelva la cuenta con esa cantidad más de Currycoins.
cobranza = alterarSaldo

-- 1.2.3. Hacer una función de minería, que reciba una cuenta y la devuelva con 25 Currycoins más. Nota: No repetir lógica.
mineria = alterarSaldo 25
mineria' = cobranza 25

-- 2. Necesitamos funciones que busquen una cuenta en una lista de cuentas:

-- 2.1. Hacer una función que dado un identificador y una cuenta, nos indique si el identificador corresponde a esa cuenta.
correspondeId ident cuenta = ident == identificador cuenta
correspondeId' ident = (ident==) . identificador

-- 2.2. Hacer una función que dada una condición y una lista de cuentas, devuelva la primera cuenta que cumple la condición.
primeraCuenta condicion = head . filter condicion

-- 2.3. Hacer una función que dada una condición y una lista de cuentas, devuelva la lista de cuentas SIN la primera cuenta que cumpla la condición.
cuentasQueCumplenSinPrimera condicion cuentas = filter distintoPrimeraQueCumple cuentas
    where distintoPrimeraQueCumple = (primeraCuenta condicion cuentas /=)

-- 3. Hacer una función que reciba un identificador, una lista de cuentas y una función que modifique una cuenta. De manera que devuelva la lista de cuentas, pero con la cuenta correspondiente al identificador modificada por la función. Las demás cuentas deben permanecer sin cambios. Nota: No hace falta conservar el orden original de la lista.
aplicarCambioA ident cuentas funcion = 
    (funcion $ primeraCuenta (correspondeId ident) cuentas) : cuentasQueCumplenSinPrimera (correspondeId ident) cuentas

aplicarCambioA' ident cuentas funcion = (funcion $ primeraCuenta condicion cuentas) : cuentasQueCumplenSinPrimera condicion cuentas
    where condicion = (correspondeId ident)

-- 4. Queremos saber como un bloque afecta a una lista de cuentas. Hacer una función que reciba esas dos cosas y devuelva la lista de cuentas con todas las transacciones ejecutadas.
afectarCon bloque cuentas = foldl aplicarTransaccion cuentas bloque
    where aplicarTransaccion ctas (ident, transaccion) = aplicarCambioA ident ctas transaccion

-- 5. Pero... ¡Esperen!... Necesitamos verificar que las cuentas sean estables. Con este fin, hacer una función que reciba una lista de cuentas y nos indique si todas tienen un saldo de Currycoins mayor o igual a cero.
cuentasEstables = all ((>0).saldo)

-- 6. Hacer un chequeo de una blockchain: necesitamos una función que reciba una blockchain y una lista de cuentas, y ejecute las transacciones de los bloques sobre la lista de cuentas y nos indique si todos los estados intermedios de las cuentas (es decir, tras aplicar cada bloque) son estables.
revisarBlockchain [] cuentas = cuentasEstables cuentas
revisarBlockchain (bloque: bloques) cuentas = cuentasEstables cuentas && revisarBlockchain bloques bloqueAplicado
    where bloqueAplicado = afectarCon bloque cuentas

-- 7. Dada la siguiente función, explicar cómo se infiere su tipo:

funcionSinPudor :: [[a]] -> ((a -> Number), (b -> b)) -> (b -> b)
funcionSinPudor x y  
    | (length . filter even . map (fst y) $ head x) > 10 = id  
    | otherwise                                          = snd y