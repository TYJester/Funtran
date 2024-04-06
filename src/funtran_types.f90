!----------------------------------------------------------------------------------------------------------------------------------!
!----------------------------------------------------------------------------------------------------------------------------------!
!                                                          Funtran Types                                                           !
!                                                                                                                                  !
!    Funtran Types is a component module of the wider Funtran library. It is a core dependency for many other Funtran modules,     !
!  providing custom data types necessary for many procedures to function. The data types themselves are also available for custom  !
!            use. Make sure to import this module by name, and read the information below on how to use the data types.            !
!----------------------------------------------------------------------------------------------------------------------------------!
!----------------------------------------------------------------------------------------------------------------------------------!

module funtran_types
        use funtran_consts

        implicit none

        private

        public :: string, stack

!----------------------------------------------------------------------------------------------------------------------------------!
!----------------------------------------------------------------------------------------------------------------------------------!
!                                                         Data Type: String                                                        !
!                                                                                                                                  !
!   The custom "string" data type is a simple, but useful one. It contains a single component called "val", which is a character   !
! of dynamic length. It is designed for use in allocatable arrays that need to contain dynamic length characters. Initialising the !
!                                                    type should look like this:                                                   !
!                                                                                                                                  !
!                                                      type(string) :: varName                                                     !
!                                                                                                                                  !
!     To use the string data type to store information, the "val" component must be accessed. This can be achieved as follows:     !
!                                                                                                                                  !
!                                                            varName%val                                                           !
!                                                                                                                                  !
!        Using this notation, a character string of any length can be assigned to the variable as many times as necessary.         !
!----------------------------------------------------------------------------------------------------------------------------------!
!----------------------------------------------------------------------------------------------------------------------------------!

        type, public :: string
                character (LEN = :), allocatable :: val
        end type string

!----------------------------------------------------------------------------------------------------------------------------------!
!----------------------------------------------------------------------------------------------------------------------------------!
!                                                         Data Type: Stack                                                         !
!                                                                                                                                  !
! The custom "stack" data type is designed to behave as a traditional stack would. It has the behaviour of a dynamic length array, !
!  and contains the primary functions expected from a stack (as described below). The stack is designed to take character inputs,  !
!   and should primarily be controlled using the built in procedures. The following procedures are available to the stack type:    !
!                                                                                                                                  !
!                      call stackVariable%init() - THIS IS REQUIRED AND MUST BE USED TO INITIALISE THE STACK                       !
!                                                                                                                                  !
!                        call stackVariable%push(value) - Pushes a character value to the top of the stack                         !
!                                                                                                                                  !
!                              call stackVariable%pop() - Removes the value from the top of the stack                              !
!                                                                                                                                  !
!                      stackVariable%peek() - Returns the value from the top of the stack without removing it                      !
!                                                                                                                                  !
!                            stackVariable%isEmpty() - Returns TRUE if empty, returns FALSE if not empty                           !
!                                                                                                                                  !
!                      call stackVariable%invert() - Inverts the stack (i.e. first in stack is now last, etc...)                   !
!                                                                                                                                  !
!                        call stackVariable%clear() - Completely empties the stack. Stack remains initialised                      !
!----------------------------------------------------------------------------------------------------------------------------------!
!----------------------------------------------------------------------------------------------------------------------------------!

        type, public :: stack
                type(string), dimension(:), allocatable :: arr, bufferArr

                contains
                        procedure :: init => init_stack
                        procedure :: push => push_stack
                        procedure :: pop => pop_stack
                        procedure :: peek => peek_stack
                        procedure :: isEmpty => isEmpty_stack
                        procedure :: invert => invert_stack
                        procedure :: clear => clear_stack
                        procedure :: length => length_stack
                        procedure :: concat => concat_stack
        end type stack

!----------------------------------------------------------------------------------------------------------------------------------!
!                                                          Module Procedures                                                       !
!----------------------------------------------------------------------------------------------------------------------------------!

        contains

!----------------------------------------------------------------------------------------------------------------------------------!
!                                                                 Init                                                             !
!      Necessary to initialise a stack. Allocates the array component of the data type as a 1 dimensional array with length 1.     !
!----------------------------------------------------------------------------------------------------------------------------------!
                subroutine init_stack(self)
                        class(stack) :: self
                        
                        allocate(self%arr(1))
                        allocate(self%bufferArr(1))
                end subroutine init_stack
!----------------------------------------------------------------------------------------------------------------------------------!
!                                                                 Push                                                             !
! Accepts a string as a parameter "input" and places it at the top of the stack. Functionally, array is stored in a buffer whilst  !
!                            space is made to fit the new value at index 1 (i.e. the top of the stack).                            !
!----------------------------------------------------------------------------------------------------------------------------------!
                subroutine push_stack(self, input)
                        class(stack) :: self
                        character (LEN = *) :: input
                        
                        deallocate(self%arr)
                        allocate(self%arr(size(self%bufferArr) + 1))

                        self%arr(2:size(self%bufferArr) + 1) = self%bufferArr
                        self%arr(1)%val = input

                        deallocate(self%bufferArr)
                        allocate(self%bufferArr(size(self%arr)))

                        self%bufferArr = self%arr
                end subroutine push_stack
!----------------------------------------------------------------------------------------------------------------------------------!
!                                                                 Pop                                                              !
!      Removes the value from the top of the stack. Functionally, stores the array in a buffer whilst reducing it's size by 1.     !
!----------------------------------------------------------------------------------------------------------------------------------!
                subroutine pop_stack(self)
                        class(stack) :: self

                        deallocate(self%arr)
                        allocate(self%arr(size(self%bufferArr) - 1))

                        self%arr = self%bufferArr(2: size(self%bufferArr))

                        deallocate(self%bufferArr)
                        allocate(self%bufferArr(size(self%arr)))

                        self%bufferArr = self%arr
                end subroutine pop_stack
!----------------------------------------------------------------------------------------------------------------------------------!
!                                                                 Peek                                                             !
!                                           Returns the top value from the stack as a string.                                      !
!----------------------------------------------------------------------------------------------------------------------------------!
                function peek_stack(self) result(res)
                        class(stack) :: self
                        character (len = :), allocatable :: res

                        res = self%arr(1)%val
                end function peek_stack
!----------------------------------------------------------------------------------------------------------------------------------!
!                                                               isEmpty                                                            !
!        Returns TRUE if the stack is empty, FALSE if else. NOTE stack is empty when the array component has a length of 1.        !
!----------------------------------------------------------------------------------------------------------------------------------!
                function isEmpty_stack(self) result(res)
                        class(stack) :: self
                        logical :: res

                        if (size(self%arr) == 1) then
                                res = .TRUE.
                        else
                                res = .FALSE.
                        end if
                end function isEmpty_stack
!----------------------------------------------------------------------------------------------------------------------------------!
!                                                                Invert                                                            !
!      Inverts the order of the stack, i.e. top of stack becomes bottom, etc. Functions by storing values in a buffer array.       !
!----------------------------------------------------------------------------------------------------------------------------------!
                subroutine invert_stack(self)
                        class(stack) :: self
                        type(string), allocatable :: bufferArr(:)
                        integer :: i

                        allocate(bufferArr(size(self%arr) - 1))

                        do i = 1, size(bufferArr)
                                bufferArr(i)%val = self%arr(i)%val
                        end do

                        call self%clear()

                        do i = 1, size(bufferArr)
                                call self%push(bufferArr(i)%val)
                        end do
                end subroutine invert_stack
!----------------------------------------------------------------------------------------------------------------------------------!
!                                                                Clear                                                             !
!      Empties the stack. Does this by deallocating internal arrays and them reallocating them in the same way as initilised.      !
!----------------------------------------------------------------------------------------------------------------------------------!
                subroutine clear_stack(self)
                        class(stack) :: self

                        deallocate(self%arr)
                        deallocate(self%bufferArr)

                        allocate(self%arr(1))
                        allocate(self%bufferArr(1))
                end subroutine clear_stack

!----------------------------------------------------------------------------------------------------------------------------------!
!                                                                Length                                                            !
!                                             Returns the number of elements in the stack.                                         !
!----------------------------------------------------------------------------------------------------------------------------------!

                function length_stack(self) result(res)
                        class(stack) :: self
                        integer :: res

                        res = size(self%arr) - 1
                end function length_stack

!----------------------------------------------------------------------------------------------------------------------------------!
!                                                                Concat                                                            !
!       Returns a string of all of the elements in the stack, starting with the bottom of the stack and ending with the top.       !
!----------------------------------------------------------------------------------------------------------------------------------!

                function concat_stack(self) result(res)
                        class(stack) :: self
                        character (LEN = :), allocatable :: res
                        integer :: i

                        call self%invert()

                        do i = 1, self%length()
                                res = res // self%peek()
                                call self%pop()
                        end do
                end function concat_stack
end module funtran_types
