program test

        use funtran_standlib

        implicit none

        integer, parameter :: dp2 = selected_real_kind(15, 300)
        real (kind=dp2) :: testArr(5), dict(2)
        logical :: res

        testArr(1) = 1.0
        testArr(2) = 1.0
        testArr(3) = 2.6
        testArr(4) = 2.5
        testArr(5) = 1.0

        dict(1) = 1.0
        dict(2) = 2.5

        res = arrayComp(testArr, dict)

        print *, res
end program test
