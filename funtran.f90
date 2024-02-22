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

module funtran.standlib
        implicit none

        integer, parameter :: dp = SELECTED_REAL_KIND(15, 300)

        contains

!---------------------------------------------------------------------------------------------------------------------------!
! The arrayComp (Array Comparator) function is designed to take 2 array arguments (testArr, dict), and 1 character argument !
!                                        (dataType). The result (res) is a boolean.                                         !
!                                                                                                                           !
!                                                                                                                           !
! - testArr (Array Type): The array who's content you want to check.                                                        !
!                                                                                                                           !
! - dict (Array Type): The array that contains the characters you want to scan testArr for.                                 !
!                                                                                                                           !
! - dataType (Character Type): The data type of bothe testArr and dict.                                                     !
!                                                                                                                           !
! - res (Boolean Type): The result of the function. If all characters in testArr are in dict, return TRUE, else FALSE.      !
!---------------------------------------------------------------------------------------------------------------------------!

                function arrayComp(testArr, dict, dataType) result(res)
                        
                end funtion arrayComp
end module funtran.standlib
