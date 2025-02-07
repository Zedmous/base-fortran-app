program ReadInteger
    implicit none
    integer :: intValue

    print *, 'Introduce un valor entero:'
    read(*, *) intValue
    print *, 'El valor entero introducido es:', intValue
end program ReadInteger


