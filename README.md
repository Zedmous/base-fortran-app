# GUÍA DE FORTRAN

# Índice de Contenidos

1. [Variables y sus Tipos](#variables-y-sus-tipos)
2. [Estructuras de Datos](#estructuras-de-datos)
    - [Estructuras Selectivas](#estructuras-selectivas)
    - [Estructuras Repetitivas](#estructuras-repetitivas)
3. [Operadores](#operadores)
    - [Operadores Lógicos](#operadores-lógicos)
    - [Operadores Comparativos](#operadores-comparativos)
4. [Conversiones](#conversiones)
5. [Operaciones Aritméticas](#operaciones-aritméticas)
6. [Sistemas Numéricos](#sistemas-numéricos)
7. [Puntos Flotantes](#puntos-flotantes)
8. [Arreglos](#arreglos)

## VARIABLES Y SUS TIPOS

### Variables Enteras

```fortran
integer :: a
a = 10
```

### Variables Reales

```fortran
real :: b
b = 3.14
```

### Variables Complejos

```fortran
complex :: c
c = (1.0, 2.0)
```

### Variables lógicas

```fortran
logical :: flag
flag = .true.
```

### Variables de tipo de Cadena de Carácteres

```fortran
character(len=10) :: name
name = 'Fortran'
```

## LECTURA Y ESCRITURA DE DATOS

### Lectura e Impresiones

```fortran
program ReadAndPrintData
    !Declaración de Variables
    implicit none
    integer :: intValue
    real :: realValue
    character(len=20) :: str
    integer, dimension(5) :: arr
    integer :: i, io_status

    ! Lectura desde la consola
    print *, 'Introduce un valor entero:'
    read(*, *) intValue
    print *, 'Introduce un valor real:'
    read(*, *) realValue
    print *, 'Introduce una cadena de texto:'
    read(*, '(A)') str

    ! Impresión de datos leídos desde la consola
    print *, 'Valor entero introducido:', intValue
    print *, 'Valor real introducido:', realValue
    print *, 'Cadena de texto introducida:', str

    ! Lectura desde un archivo
    open(unit=10, file='data.txt', status='old', action='read', iostat=io_status)
    if (io_status /= 0) then
        print *, 'Error al abrir el archivo.'
        stop
    end if

    read(10, *) arr
    close(10)

    ! Impresión de datos leídos desde el archivo
    print *, 'Array leído del archivo:'
    do i = 1, 5
        print *, 'Elemento', i, ':', arr(i)
    end do

end program ReadAndPrintData
```
## ESTRUCTURAS DE DATOS

### Estructuras de Decisión o Selectivas

#### If Else

```fortran
program DecisionExample
    integer :: x
    x = 10
    if (x > 5) then
        print *, 'x es mayor que 5'
    else
        print *, 'x es menor o igual a 5'
    end if
end program DecisionExample
```

#### Select Case

```fortran
program CaseExample
    integer :: x
    x = 2

    select case (x)
        case (1)
            print *, 'x es 1'
        case (2)
            print *, 'x es 2'
        case default
            print *, 'x no es 1 ni 2'
    end select
end program CaseExample
```

## Estructuras de Repetitivas

### DO Loop

```fortran
program DoLoopExample
    integer :: i
    do i = 1, 10
        print *, 'i =', i
    end do
end program DoLoopExample
```

### Do While Loop

```fortran
program DoWhileExample
    integer :: i
    i = 1

    do while (i <= 10)
        print *, 'i =', i
        i = i + 1
    end do
end program DoWhileExample
```

### Exit y Cycle

```fortran
program ExitCycleExample
    integer :: i

    do i = 1, 10
        if (i == 5) exit
        if (mod(i, 2) == 0) cycle
        print *, 'i =', i
    end do
end program ExitCycleExample
```

## OPERADORES

### Operadores logicos y comparativos
```fortran
program LogicalComparisonOperators
    implicit none
    integer :: a, b, i
    logical :: condition

    ! Inicialización de valores
    a = 5
    b = 10
    condition = .true.

    ! Ejemplos de operadores de comparación en IF
    if (a == b) then
        print *, 'a es igual a b'
    else if (a /= b) then
        print *, 'a es diferente de b'
    end if

    if (a > b) then
        print *, 'a es mayor que b'
    else if (a < b) then
        print *, 'a es menor que b'
    else if (a >= b) then
        print *, 'a es mayor o igual que b'
    else if (a <= b) then
        print *, 'a es menor o igual que b'
    end if

    ! Ejemplos de operadores lógicos en IF
    if (a > 0 .AND. b < 15) then
        print *, 'a es mayor que 0 y b es menor que 15'
    end if

    if (a < 0 .OR. b > 5) then
        print *, 'a es menor que 0 o b es mayor que 5'
    end if

    if (.NOT. (a == b)) then
        print *, 'a no es igual a b'
    end if

    ! Ejemplo de operadores lógicos y de comparación en DO WHILE
    i = 1
    do while (i <= 10 .AND. condition)
        print *, 'i =', i
        if (i == 5) condition = .false.
        i = i + 1
    end do

end program LogicalComparisonOperators
```

## CONVERSIONES

### Convertir de entero a real

```fortran
program ConvertIntToReal
    integer :: a
    real :: b

    a = 5
    b = real(a)

    print *, 'b =', b
end program ConvertIntToReal
```

### Convertir de real a entero

```fortran
program ConvertRealToInt
    real :: b
    integer :: a

    b = 3.14
    a = int(b)

    print *, 'a =', a
end program ConvertRealToInt
```

### Convertir de caracteres a enteros

```fortran
program CharToIntExample
    character(len=5) :: str
    integer :: num

    str = '12345'
    read(str, *) num

    print *, 'num =', num
end program CharToIntExample
```

### Convertir de real a strings

```fortran
program ConvertRealToString
    real :: num
    character(len=20) :: str

    num = 3.14159
    write(str, '(F0.5)') num  ! F0.5 es el formato para convertir real a string con 5 decimales

    print *, 'El número real como string es:', str
end program ConvertRealToString
```

## Operaciones arimeticas

```fortran
program MathOperations
    implicit none
    real :: a, b, c
    integer :: intResult
    real :: floatResult

    ! Operaciones Aritméticas Básicas
    a = 5.0
    b = 2.0
    c = a + b
    print *, 'Adición: ', c
    c = a - b
    print *, 'Sustracción: ', c
    c = a * b
    print *, 'Multiplicación: ', c
    c = a / b
    print *, 'División: ', c

    ! Potencia
    c = a ** b
    print *, 'Potencia: ', c

    ! Funciones Matemáticas
    c = sqrt(a)
    print *, 'Raíz Cuadrada: ', c
    c = abs(a)
    print *, 'Valor Absoluto: ', c
    c = exp(a)
    print *, 'Exponencial: ', c
    c = log(a)
    print *, 'Logaritmo Natural: ', c
    c = log10(a)
    print *, 'Logaritmo Base 10: ', c

    ! Funciones Trigonométricas
    c = sin(a)
    print *, 'Seno: ', c
    c = cos(a)
    print *, 'Coseno: ', c
    c = tan(a)
    print *, 'Tangente: ', c
    c = asin(0.5)
    print *, 'Arc Seno: ', c
    c = acos(0.5)
    print *, 'Arc Coseno: ', c
    c = atan(1.0)
    print *, 'Arc Tangente: ', c

    ! Funciones Hiperbólicas
    c = sinh(a)
    print *, 'Seno Hiperbólico: ', c
    c = cosh(a)
    print *, 'Coseno Hiperbólico: ', c
    c = tanh(a)
    print *, 'Tangente Hiperbólica: ', c

    ! Funciones de Redondeo
    c = 5.6789
    intResult = nint(c)
    print *, 'Redondeo al Entero más Cercano: ', intResult
    intResult = int(c)
    print *, 'Parte Entera: ', intResult
    floatResult = 5.6789
    intResult = nint(floatResult * 100.0) / 100.0  ! Redondeo a 2 decimales
    print *, 'Redondeo a 2 Decimales: ', intResult / 100.0
    floatResult = 5.6789
    intResult = int(floatResult * 100.0) / 100.0  ! Parte entera a 2 decimales
    print *, 'Parte Entera a 2 Decimales: ', intResult / 100.0

end program MathOperations
```

## SISTEMAS NÚMERICOS

```fortran
program NumberSystems
    implicit none
    integer :: bin_a, bin_b, bin_result
    integer :: oct_a, oct_b, oct_result
    integer :: hex_a, hex_b, hex_result

    ! Operaciones en Binario
    bin_a = 2#0101  ! Binario 0101 = Decimal 5
    bin_b = 2#0110  ! Binario 0110 = Decimal 6
    bin_result = bin_a + bin_b
    print *, 'Resultado de la Adición en Binario: ', bin_result

    bin_result = bin_a - bin_b
    print *, 'Resultado de la Sustracción en Binario: ', bin_result

    ! Operaciones en Octal
    oct_a = 8#123  ! Octal 123 = Decimal 83
    oct_b = 8#456  ! Octal 456 = Decimal 302
    oct_result = oct_a + oct_b
    print *, 'Resultado de la Adición en Octal: ', oct_result

    oct_result = oct_a - oct_b
    print *, 'Resultado de la Sustracción en Octal: ', oct_result

    ! Operaciones en Hexadecimal
    hex_a = 16#1A  ! Hexadecimal 1A = Decimal 26
    hex_b = 16#2B  ! Hexadecimal 2B = Decimal 43
    hex_result = hex_a + hex_b
    print *, 'Resultado de la Adición en Hexadecimal: ', hex_result

    hex_result = hex_a - hex_b
    print *, 'Resultado de la Sustracción en Hexadecimal: ', hex_result

end program NumberSystems
```

## PUNTO FLOTANTE

```fortran
program FloatingPointAndErrors
    implicit none

    ! Declaración de variables
    real :: a, b, result
    real :: trueValue, approxValue, absError, relError
    real :: sum, diff, prod, quot

    ! Ejemplo del Sistema Numérico de Punto Flotante
    ! Definición y aplicación
    a = 5.123456
    b = 2.71828

    print *, 'Valores de punto flotante:'
    print *, 'a = ', a
    print *, 'b = ', b

    ! Teoría de Errores
    ! Valores verdaderos y aproximados
    trueValue = 3.14159
    approxValue = 3.14

    ! Cálculo del Error Absoluto
    absError = abs(trueValue - approxValue)
    print *, 'Error Absoluto =', absError

    ! Cálculo del Error Relativo
    relError = absError / abs(trueValue)
    print *, 'Error Relativo =', relError

    ! Error Propagado (se vería en cálculos sucesivos, solo un ejemplo básico aquí)
    ! Realizamos una suma para ilustrar
    result = a + b + approxValue
    print *, 'Resultado con error propagado (suma):', result

    ! Error Significativo y Redondeo
    ! Simulación de error de redondeo
    trueValue = 123456.789
    approxValue = 123456.7  ! Redondeo a 1 decimal
    absError = abs(trueValue - approxValue)
    print *, 'Error de Redondeo Absoluto =', absError

    ! Aritmética de Punto Flotante
    a = 5.5
    b = 2.2

    ! Adición
    sum = a + b
    print *, 'Adición: ', sum

    ! Sustracción
    diff = a - b
    print *, 'Sustracción: ', diff

    ! Multiplicación
    prod = a * b
    print *, 'Multiplicación: ', prod

    ! División
    quot = a / b
    print *, 'División: ', quot

end program FloatingPointAndErrors
```

## ARREGLOS

### Declaración y Uso de Arreglos Unidimensionales

```fortran
program ArrayExample
    integer, dimension(5) :: arr
    integer :: i

    do i = 1, 5
        arr(i) = i * 2
    end do

    print *, arr
end program ArrayExample
```

### Arreglos Multidimensionales

```fortran
program MultiArrayExample
    integer, dimension(3, 3) :: mat
    integer :: i, j

    do i = 1, 3
        do j = 1, 3
            mat(i, j) = i + j
        end do
    end do

    print *, mat
end program MultiArrayExample
```
## FUNCIONES Y SUBRUTINAS

## Funciones
```fortran
program FunctionExample
    implicit none
    real :: result

    result = square(5.0)
    print *, 'El cuadrado de 5.0 es', result

contains

    real function square(x)
        real, intent(in) :: x
        square = x * x
    end function square

end program FunctionExample
```

## Subrutinas
```fortran
program SubroutineExample
    implicit none

    call greet('Fortran')

contains

    subroutine greet(name)
        character(len=*), intent(in) :: name
        print *, 'Hola, ', name
    end subroutine greet

end program SubroutineExample
```

## Definición de Módulos y Tipos Definidos por el Usuario

```fortran
module PersonModule
    type :: Person
        character(len=20) :: name
        integer :: age
    contains
        procedure :: printInfo
    end type Person

contains

    subroutine printInfo(this)
        class(Person), intent(in) :: this
        print *, 'Nombre: ', this%name
        print *, 'Edad: ', this%age
    end subroutine printInfo

end module PersonModule

program ClassExample
    use PersonModule
    implicit none
    type(Person) :: person1

    person1%name = 'Juan'
    person1%age = 30
    call person1%printInfo()

end program ClassExample

```
## Conceptos combinados

```fortran
program CompleteExample
    use PersonModule
    implicit none

    ! Declaración de variables
    integer :: intValue
    real :: realValue
    character(len=20) :: str

    ! Definición y uso de un arreglo unidimensional
    integer, dimension(5) :: arr
    integer :: i

    ! Definición y uso de un arreglo multidimensional
    integer, dimension(3, 3) :: mat
    integer :: j, k

    ! Leer valores desde la consola
    print *, 'Introduce un valor entero:'
    read(*, *) intValue
    print *, 'Introduce un valor real:'
    read(*, *) realValue

    ! Convertir entero a string
    write(str, '(I0)') intValue  ! I0 es el formato para convertir entero a string sin espacios

    ! Imprimir valores
    print *, 'El valor entero introducido es:', intValue
    print *, 'El valor real introducido es:', realValue
    print *, 'El número entero como string es:', str

    ! Estructura de decisión IF-THEN-ELSE
    if (intValue > 10) then
        print *, 'El entero es mayor que 10'
    else
        print *, 'El entero es 10 o menor'
    end if

    ! Estructura de decisión SELECT CASE
    select case (intValue)
        case (1)
            print *, 'El entero es 1'
        case (2)
            print *, 'El entero es 2'
        case default
            print *, 'El entero no es 1 ni 2'
    end select

    ! Estructura repetitiva DO Loop
    do i = 1, 5
        arr(i) = i * 2
    end do
    print *, 'Arreglo unidimensional:', arr

    ! Estructura repetitiva DO WHILE Loop
    i = 1
    do while (i <= 5)
        print *, 'Valor de i en DO WHILE:', i
        i = i + 1
    end do

    ! Estructura repetitiva con EXIT y CYCLE
    do j = 1, 10
        if (j == 5) exit
        if (mod(j, 2) == 0) cycle
        print *, 'j =', j
    end do

    ! Llenar y mostrar arreglo multidimensional
    do j = 1, 3
        do k = 1, 3
            mat(j, k) = j + k
        end do
    end do
    print *, 'Arreglo multidimensional:'
    do j = 1, 3
        print *, (mat(j, k), k=1, 3)
    end do

    ! Uso de subrutina para mostrar información de una "clase"
    type(Person) :: person1
    person1%name = 'Juan'
    person1%age = 30
    call person1%printInfo()

    ! Llamada a la función que calcula el cuadrado
    realValue = square(realValue)
    print *, 'El cuadrado del valor real es:', realValue

contains

    ! Definición de la función interna square
    real function square(x)
        real, intent(in) :: x
        square = x * x
    end function square

end program CompleteExample

module PersonModule
    implicit none
    type :: Person
        character(len=20) :: name
        integer :: age
    contains
        procedure :: printInfo
    end type Person

contains

    subroutine printInfo(this)
        class(Person), intent(in) :: this
        print *, 'Nombre: ', this%name
        print *, 'Edad: ', this%age
    end subroutine printInfo

end module PersonModule
```
