!---------------------------------------------------------------------------------------------------------------------------!
!                                                        FUNTRAN 0.1                                                        !
! Funtran is a general purpose fortran library put together, updated, and mantained by William Lavery. It is comprised of a !
!   variety of useful modules that are designed to reduce the necessity of boiler plate code and expedite the programming   !
! experience. New modules are added as needed, meaning the library will naturally grow over time. You can find explinations !
!           preceeding each module, including syntax, arguments, and some background into how the function works.           !
!---------------------------------------------------------------------------------------------------------------------------!

!---------------------------------------------------------------------------------------------------------------------------!
! Funtran standLib (Standard Library) contains all of the most general functions in the Funtran module. Whilst all of these !
!   functions are usable outside of the program, many of them are originally designed to be used internally by other more   !
!                                                     complex functions.                                                    !
!---------------------------------------------------------------------------------------------------------------------------!

module funtran_standlib
        implicit none

        integer, parameter :: dp = SELECTED_REAL_KIND(15, 300)

        public arrayComp

        private arrayCompInt, arrayCompReal, arrayCompChar

        interface arrayComp
                module procedure arrayCompInt, arrayCompReal, arrayCompChar
        end interface arrayComp

        contains

!---------------------------------------------------------------------------------------------------------------------------!
!                The arrayComp (Array Comparator) function is designed to take 2 array arguments (testArr, dict).           !
!                                               The result (res) is a boolean.                                              !
!                                                                                                                           !
!                                                                                                                           !
! - testArr (Array Type): The array who's content you want to check.                                                        !
!                                                                                                                           !
! - dict (Array Type): The array that contains the characters you want to scan testArr for.                                 !
!                                                                                                                           !
! - res (Boolean Type): The result of the function. If all characters in testArr are in dict, return TRUE, else FALSE.      !
!---------------------------------------------------------------------------------------------------------------------------!
        function arrayCompInt(testArr, dict) result(res)
                integer, intent(IN) :: testArr(:), dict(:)
                logical :: res, found
                integer :: i, j

                res = .TRUE.
                do i = 1, size(testArr)
                    found = .FALSE.
                    do j = 1, size(dict)
                        if (testArr(i) == dict(j)) then
                            found = .TRUE.
                            exit
                        endif
                    end do
                    if (.NOT. found) then
                        res = .FALSE.
                        exit
                    endif
                end do
        end function arrayCompInt

        function arrayCompReal(testArr, dict) result(res)
                real(kind=dp), intent(IN) :: testArr(:), dict(:)
                logical :: res, found
                integer :: i, j

                res = .TRUE.
                do i = 1, size(testArr)
                    found = .FALSE.
                    do j = 1, size(dict)
                        if (testArr(i) == dict(j)) then
                            found = .TRUE.
                            exit
                        endif
                    end do
                    if (.NOT. found) then
                        res = .FALSE.
                        exit
                    endif
                end do
        end function arrayCompReal

        function arrayCompChar(testArr, dict) result(res)
                character(*), intent(IN) :: testArr(:), dict(:)
                logical :: res, found
                integer :: i, j

                res = .TRUE.
                do i = 1, size(testArr)
                    found = .FALSE.
                    do j = 1, size(dict)
                        if (testArr(i) == dict(j)) then
                            found = .TRUE.
                            exit
                        endif
                    end do
                    if (.NOT. found) then
                        res = .FALSE.
                        exit
                    endif
                end do
        end function arrayCompChar

end module funtran_standlib
