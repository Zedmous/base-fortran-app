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

