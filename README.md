# Guía de Fortran

## Tipos de Variables

### Enteros

```fortran
integer :: a
a = 10
```

### Reales

```fortran
real :: b
b = 3.14
```

### Complejos

```fortran
complex :: c
c = (1.0, 2.0)
```

### logicos

```fortran
logical :: flag
flag = .true.
```

### caracteres

```fortran
character(len=10) :: name
name = 'Fortran'
```

##Estructuras de Decisión

## If Else

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

## Select Case

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

## DO Loop

```fortran
program DoLoopExample
    integer :: i
    do i = 1, 10
        print *, 'i =', i
    end do
end program DoLoopExample
```

## DO WHILE Loop

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

## EXIT y CYCLE

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

### Arreglos

## Declaración y Uso de Arreglos Unidimensionales

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

## Arreglos Multidimensionales

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

### Conversiones

## Convertir de entero a real

```fortran
program ConvertIntToReal
    integer :: a
    real :: b

    a = 5
    b = real(a)

    print *, 'b =', b
end program ConvertIntToReal
```

## Convertir de real a entero

```fortran
program ConvertRealToInt
    real :: b
    integer :: a

    b = 3.14
    a = int(b)

    print *, 'a =', a
end program ConvertRealToInt
```

## Convertir de caracteres a enteros

```fortran
program CharToIntExample
    character(len=5) :: str
    integer :: num

    str = '12345'
    read(str, *) num

    print *, 'num =', num
end program CharToIntExample
```

## Convertir de real a strings

```fortran
program ConvertRealToString
    real :: num
    character(len=20) :: str

    num = 3.14159
    write(str, '(F0.5)') num  ! F0.5 es el formato para convertir real a string con 5 decimales

    print *, 'El número real como string es:', str
end program ConvertRealToString
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
## Puntos Flotantes

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