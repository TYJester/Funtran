!---------------------------------------------------------------------------------------------------------------------------!
!---------------------------------------------------------------------------------------------------------------------------!
!                                                       Funtran Consts                                                      !
!                                                                                                                           !
!   The Funtran Consts module contains basic and fundamental constants used throughout the Funtran library. Constants are   !
!                                        also available for use in external programs.                                       !
!---------------------------------------------------------------------------------------------------------------------------!
!---------------------------------------------------------------------------------------------------------------------------!

module funtran_consts
        implicit none

        public
        
        !dp - Universal double precision value
        integer, parameter :: dp = SELECTED_REAL_KIND(15, 300)

        !pi_ - Approximation for the value of pi
        real (KIND = dp), parameter :: pi_ = 4 * ATAN(1.0_dp)

        !e_ - Value of Euler's number
        real (KIND = dp), parameter :: e_ = exp(1.0_dp)

        !h_ - Approximation for Planck's constant
        real (KIND = dp), parameter :: h_ = 6.6260715e-34_dp

        !hbar_ - Approximation for the reduced Planck's constant
        real (KIND = dp), parameter :: hbar_ = h_/(2 * pi_)
end module funtran_consts
