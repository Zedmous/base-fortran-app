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
logical :: flag
flag = .true.

### caracteres
character(len=10) :: name
name = 'Fortran'

##Estructuras de Decisión

## If Else
program DecisionExample
    integer :: x
    x = 10

    if (x > 5) then
        print *, 'x es mayor que 5'
    else
        print *, 'x es menor o igual a 5'
    end if
end program DecisionExample

## Select Case
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

## DO Loop
program DoLoopExample
    integer :: i
    do i = 1, 10
        print *, 'i =', i
    end do
end program DoLoopExample

## DO WHILE Loop
program DoWhileExample
    integer :: i
    i = 1

    do while (i <= 10)
        print *, 'i =', i
        i = i + 1
    end do
end program DoWhileExample

## EXIT y CYCLE
program ExitCycleExample
    integer :: i

    do i = 1, 10
        if (i == 5) exit
        if (mod(i, 2) == 0) cycle
        print *, 'i =', i
    end do
end program ExitCycleExample

### Arreglos

## Declaración y Uso de Arreglos Unidimensionales
program ArrayExample
    integer, dimension(5) :: arr
    integer :: i

    do i = 1, 5
        arr(i) = i * 2
    end do

    print *, arr
end program ArrayExample

## Arreglos Multidimensionales
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