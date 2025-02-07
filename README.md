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