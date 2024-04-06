!---------------------------------------------------------------------------------------------------------------------------!
!---------------------------------------------------------------------------------------------------------------------------!
!                                                      Funtran StandLib                                                     !
! Funtran StandLib (Standard Library) contains all of the most general functions in the Funtran module. Whilst all of these !
!   functions are usable outside of the program, many of them are originally designed to be used internally by other more   !
!                                                     complex functions.                                                    !
!---------------------------------------------------------------------------------------------------------------------------!
!---------------------------------------------------------------------------------------------------------------------------!

module funtran_standlib
        use funtran_types
        use funtran_consts

        implicit none

        public isNumeric, arrayComp

        private arrayCompInt, arrayCompReal, arrayCompChar

        interface arrayComp
                module procedure arrayCompInt, arrayCompReal, arrayCompChar
        end interface arrayComp

        contains

!---------------------------------------------------------------------------------------------------------------------------!
!---------------------------------------------------------------------------------------------------------------------------!
!                                                          bInt                                                             !
!     bInt returns the binary value of any given integer as a string, with the first digit representing positive (0) or     !
!                                                      negative (1)                                                         !
!---------------------------------------------------------------------------------------------------------------------------!
!---------------------------------------------------------------------------------------------------------------------------!
                function bInt(val) result(res)
                        integer :: val, n, i, rem
                        character (LEN = :), allocatable :: res
                        type(stack) :: buffer

                        call buffer%init()

                        n = 0

                        if (val == 0) then
                                res = '0'
                                stop
                        else if (val < 0) then
                                call buffer%push('1')
                        else
                                call buffer%push('0')
                        end if

                        rem = abs(val)

                        do
                                if (rem < 2**(n + 1)) then
                                        exit
                                else
                                        n = n + 1
                                end if
                        end do

                        do i = n, 0, -1
                                if (rem > 2**i) then
                                        call buffer%push('1')
                                        rem = rem - (2**i)
                                else
                                        call buffer%push('0')
                                end if
                        end do

                        res = buffer%concat()
                end function bInt

!---------------------------------------------------------------------------------------------------------------------------!
!---------------------------------------------------------------------------------------------------------------------------!
!                                                        isNumeric                                                          !
!                                                                                                                           !
!    The isNumeric function takes a string argument "str" and checks if it is a integer or real string. If the string is    !
!                              numeric, the function returns TRUE. Otherwise, it returns FALSE.                             !
!---------------------------------------------------------------------------------------------------------------------------!
!---------------------------------------------------------------------------------------------------------------------------!

                function isNumeric(str) result(res)
                        character (LEN = *), intent(IN) :: str
                        real (KIND = dp) :: num
                        integer :: io
                        logical :: res

                        read(str, *, iostat = io) num

                        if (io == 0) then
                                res = .TRUE.
                        else
                                res = .FALSE.
                        end if
                end function isNumeric

!---------------------------------------------------------------------------------------------------------------------------!
!---------------------------------------------------------------------------------------------------------------------------!
!                                                           toReal                                                          !
!                                                                                                                           !
!           The toReal function takes a numerical string argument "str" and returns the real value of the string.           !
!---------------------------------------------------------------------------------------------------------------------------!
!---------------------------------------------------------------------------------------------------------------------------!

                function toReal(str) result(res)
                        character (LEN = *) :: str
                        real (KIND = dp) :: res

                        read(str, *) res
                end function toReal

                function toInt(str) result(res)
                        character (LEN = *) :: str
                        integer :: res

                        read(str, *) res
                end function toInt

!---------------------------------------------------------------------------------------------------------------------------!
!---------------------------------------------------------------------------------------------------------------------------!
!                                                           toChar                                                          !
!                                                                                                                           !
!           The toChar function takes a real argument "num" and returns a character variable containing the real.           !
!---------------------------------------------------------------------------------------------------------------------------!
!---------------------------------------------------------------------------------------------------------------------------!

                function toChar(num) result(res)
                        real (KIND = dp), intent(IN) :: num
                        character (LEN = :), allocatable :: res
                        character (LEN = 999) :: buffer

                        write(buffer, '(F984.15)') num

                        res = trim(adjustl(buffer))
                end function toChar

!---------------------------------------------------------------------------------------------------------------------------!
!---------------------------------------------------------------------------------------------------------------------------!
!                                                         arrayComp                                                         !
!                                                                                                                           !
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

!---------------------------------------------------------------------------------------------------------------------------!
!---------------------------------------------------------------------------------------------------------------------------!
!                                                             eval                                                          !
!                                                                                                                           !
!    The eval (evaluation) function takes 2 required inputs (func, varArr) and 1 optional input (constArr). The function    !
!  evaluates the func given particular values for the variables (as defined in varArr) using Reverse Polish Notation (RPN)  !
!                                             and returns the numerical result                                              !
!                                                                                                                           !
!                                                                                                                           !
! - func (Character Type): The string containing the function to be solved in Infix notation.                               !
!                                                                                                                           !
! - varArr (Character Type): The array containing the variables and their associated values.                                !
!                                                                                                                           !
! - constArr (Character Type): The array containing the constants and their associated values.                              !
!                                                                                                                           !
! - res (Real Type): The solution to the function.                                                                          !
!---------------------------------------------------------------------------------------------------------------------------!
!---------------------------------------------------------------------------------------------------------------------------!

        function eval(func, varArr, constArr) result(res)
                character (LEN = *), intent(IN) :: func
                type(string), intent(IN) :: varArr(:, :)
                type(string), optional, intent(IN) :: constArr(:, :)
                type(string) :: buffer
                type(stack) :: infix, postfix, stackObj, evalBuffer
                real (KIND = dp) :: res
                integer :: i, j

                call infix%init()
                call postfix%init()
                call stackObj%init()
                call evalBuffer%init()

                buffer%val = ''

!----------------------------------------------------------------------------------------------------------------------------------!
!                                                              Step 1                                                              !
! The function takes the string and cycles through it, using the following rules to construct an INFIX stack:                      !
!         - If operator or parenthesis, substring is pushed to stack;                                                              !
!         - If decimal point, substring is added to buffer;                                                                        !
!         - Otherwise add substring to buffer;                                                                                     !
!         - After loop, push remaining buffer to stack                                                                             !
!----------------------------------------------------------------------------------------------------------------------------------!

                do i = 1, len(func)
                        if (any(func(i:i) == (/'+', '-', '*', '|', '^', '(', ')'/))) then
                                if (buffer%val /= '') then
                                        call infix%push(buffer%val)
                                        buffer%val = ''
                                end if
                                call infix%push(func(i:i))

                        else if (func(i:i) == '.') then
                                buffer%val = buffer%val // '.'

                        else
                                buffer%val = buffer%val // func(i:i)
                        end if
                end do

                if (buffer%val /= '') call infix%push(buffer%val)

                call infix%invert()

!----------------------------------------------------------------------------------------------------------------------------------!
!                                                              Step 2                                                              !
!  The function replaces all instances of variables/constants in the INFIX stack with the associated value by looping through the  !
!                                         INFIX stack and the array of variables/constants.                                        !
!----------------------------------------------------------------------------------------------------------------------------------!

                do i = 1, size(infix%arr)
                        do j = 1, size(varArr, DIM = 2)
                                if (infix%arr(i)%val == varArr(1, j)%val) then
                                        infix%arr(i)%val = varArr(2, j)%val
                                        infix%bufferArr(i)%val = varArr(2, j)%val
                                        exit
                                end if
                        end do
                end do

                if (PRESENT(constArr)) then
                        do i = 1, size(infix%arr)
                                do j = 1, size(constArr, DIM = 2)
                                        if (infix%arr(i)%val == constArr(1, j)%val) then
                                                infix%arr(i)%val = constArr(2, j)%val
                                                infix%bufferArr(i)%val = constArr(2, j)%val
                                                exit
                                        end if
                                end do
                        end do
                end if

!----------------------------------------------------------------------------------------------------------------------------------!
!                                                              Step 3                                                              !
! The function converts the INFIX stack into a POSTFIX stack, which is easier to evaluate. the algorithm is as follows:            !
!         - If is numeric (or a decimal point), push onto POSTFIX stack and pop from INFIX stack;                                  !
!         - If is a left bracket, push onto operator stack and pop from INFIX stack;                                               !
!         - If is a right bracket, consecutavely push from top of operator stack to POSTFIX stack until encounter left bracket;    !
!         - If is operator, push to operator stack. If last top of operator stack has greater/equal precident, push it to POSTFIX; !
!         - If INFIX stack is empty, but operator stack is not, push all from operator stack into POSTFIX stack.                   !
!----------------------------------------------------------------------------------------------------------------------------------!

                do i = 1, size(infix%arr) - 1
                        if (isNumeric(infix%peek()) .OR. infix%peek() == '.') then
                                call postfix%push(infix%peek())
                                call infix%pop()
                        else if (infix%peek() == '(') then
                                call stackObj%push(infix%peek())
                                call infix%pop()
                        else if (infix%peek() == ')') then
                                do
                                        if (stackObj%peek() == '(') then
                                                call stackObj%pop()
                                                call infix%pop()
                                                exit
                                        else
                                                call postfix%push(stackObj%peek())
                                                call stackObj%pop()
                                        end if
                                end do
                        else if (infix%peek() == '+' .OR. infix%peek() == '-') then
                                do
                                        if (stackObj%peek() == '+' .OR. stackObj%peek() == '-') then
                                                call postfix%push(stackObj%peek())
                                                call stackObj%pop()
                                        else if (stackObj%peek() == '*' .OR. stackObj%peek() == '|') then
                                                call postfix%push(stackObj%peek())
                                                call stackObj%pop()
                                        else if (stackObj%peek() == '^') then
                                                call postfix%push(stackObj%peek())
                                                call stackObj%pop()
                                        else
                                                call stackObj%push(infix%peek())
                                                call infix%pop()
                                               exit
                                        end if
                                end do
                        else if (infix%peek() == '*' .OR. infix%peek() == '|') then
                                do
                                        if (stackObj%peek() == '*' .OR. stackObj%peek() == '|') then
                                                call postfix%push(stackObj%peek())
                                                call stackObj%pop()
                                        else if (stackObj%peek() == '^') then
                                                call postfix%push(stackObj%peek())
                                                call stackObj%pop()
                                        else
                                                call stackObj%push(infix%peek())
                                                call infix%pop()
                                               exit
                                        end if
                                end do
                        else if (infix%peek() == '^') then
                                do
                                        if (stackObj%peek() == '^') then
                                                call postfix%push(stackObj%peek())
                                                call stackObj%pop()
                                        else
                                                call stackObj%push(infix%peek())
                                                call infix%pop()
                                                exit
                                        end if
                                end do
                        else
                                call stackObj%push(infix%peek())
                                call infix%pop()
                        end if
                end do

                if (.NOT. stackObj%isEmpty()) then
                        do i = 1, size(stackObj%arr) - 1
                                call postfix%push(stackObj%peek())
                                call stackObj%pop()
                        end do
                end if

                call stackObj%clear()

                call postfix%invert()

!----------------------------------------------------------------------------------------------------------------------------------!
!                                                              Step 4                                                              !
! The function then evaluates the POSTFIX stack. The algorithm is as follows:                                                      !
!         - If is numeric, add to numeric stack;                                                                                   !
!         - If is an operator, pop top two values from numeric stack, use the operator, and push the new value to the stack.       !
! The final value is returned to the program.                                                                                      !
!----------------------------------------------------------------------------------------------------------------------------------!

                do i = 1, size(postfix%arr) - 1
                        if (isNumeric(postfix%peek())) then
                                call stackObj%push(postfix%peek())
                        else if (postfix%peek() == '+') then
                                call evalBuffer%push(stackObj%peek())
                                call stackObj%pop()
                                call evalBuffer%push(stackObj%peek())
                                call stackObj%pop()

                                call stackObj%push(toChar(toReal(evalBuffer%arr(1)%val) + toReal(evalBuffer%arr(2)%val)))
                                call evalBuffer%clear()
                        
                        else if (postfix%peek() == '-') then
                                call evalBuffer%push(stackObj%peek())
                                call stackObj%pop()
                                call evalBuffer%push(stackObj%peek())
                                call stackObj%pop()

                                call stackObj%push(toChar(toReal(evalBuffer%arr(1)%val) - toReal(evalBuffer%arr(2)%val)))
                                call evalBuffer%clear()
                        
                        else if (postfix%peek() == '*') then
                                call evalBuffer%push(stackObj%peek())
                                call stackObj%pop()
                                call evalBuffer%push(stackObj%peek())
                                call stackObj%pop()

                                call stackObj%push(toChar(toReal(evalBuffer%arr(1)%val) * toReal(evalBuffer%arr(2)%val)))
                                call evalBuffer%clear()

                        else if (postfix%peek() == '|') then
                                call evalBuffer%push(stackObj%peek())
                                call stackObj%pop()
                                call evalBuffer%push(stackObj%peek())
                                call stackObj%pop()

                                call stackObj%push(toChar(toReal(evalBuffer%arr(1)%val) / toReal(evalBuffer%arr(2)%val)))
                                call evalBuffer%clear()

                        else if (postfix%peek() == '^') then
                                call evalBuffer%push(stackObj%peek())
                                call stackObj%pop()
                                call evalBuffer%push(stackObj%peek())
                                call stackObj%pop()

                                call stackObj%push(toChar(toReal(evalBuffer%arr(1)%val) ** toReal(evalBuffer%arr(2)%val)))
                                call evalBuffer%clear()
                        else if (postfix%peek() == 'sin') then
                                call evalBuffer%push(stackObj%peek())
                                call stackObj%pop()

                                call stackObj%push(toChar(sin(toReal(evalBuffer%arr(1)%val))))
                                call evalBuffer%clear
                        else if (postfix%peek() == 'cos') then
                                call evalBuffer%push(stackObj%peek())
                                call stackObj%pop()

                                call stackObj%push(toChar(cos(toReal(evalBuffer%arr(1)%val))))
                                call evalBuffer%clear
                        else if (postfix%peek() == 'tan') then
                                call evalBuffer%push(stackObj%peek())
                                call stackObj%pop()

                                call stackObj%push(toChar(tan(toReal(evalBuffer%arr(1)%val))))
                                call evalBuffer%clear
                        else if (postfix%peek() == 'arcsin') then
                                call evalBuffer%push(stackObj%peek())
                                call stackObj%pop()

                                call stackObj%push(toChar(asin(toReal(evalBuffer%arr(1)%val))))
                                call evalBuffer%clear
                        else if (postfix%peek() == 'arccos') then
                                call evalBuffer%push(stackObj%peek())
                                call stackObj%pop()

                                call stackObj%push(toChar(acos(toReal(evalBuffer%arr(1)%val))))
                                call evalBuffer%clear
                        else if (postfix%peek() == 'arctan') then
                                call evalBuffer%push(stackObj%peek())
                                call stackObj%pop()

                                call stackObj%push(toChar(atan(toReal(evalBuffer%arr(1)%val))))
                                call evalBuffer%clear
                        else if (postfix%peek() == 'sinh') then
                                call evalBuffer%push(stackObj%peek())
                                call stackObj%pop()

                                call stackObj%push(toChar(sinh(toReal(evalBuffer%arr(1)%val))))
                                call evalBuffer%clear
                        else if (postfix%peek() == 'cosh') then
                                call evalBuffer%push(stackObj%peek())
                                call stackObj%pop()

                                call stackObj%push(toChar(cosh(toReal(evalBuffer%arr(1)%val))))
                                call evalBuffer%clear
                        else if (postfix%peek() == 'tanh') then
                                call evalBuffer%push(stackObj%peek())
                                call stackObj%pop()

                                call stackObj%push(toChar(tanh(toReal(evalBuffer%arr(1)%val))))
                                call evalBuffer%clear
                        else if (postfix%peek() == 'arcsinh') then
                                call evalBuffer%push(stackObj%peek())
                                call stackObj%pop()

                                call stackObj%push(toChar(asinh(toReal(evalBuffer%arr(1)%val))))
                                call evalBuffer%clear
                        else if (postfix%peek() == 'arccosh') then
                                call evalBuffer%push(stackObj%peek())
                                call stackObj%pop()

                                call stackObj%push(toChar(acosh(toReal(evalBuffer%arr(1)%val))))
                                call evalBuffer%clear
                        else if (postfix%peek() == 'arctanh') then
                                call evalBuffer%push(stackObj%peek())
                                call stackObj%pop()

                                call stackObj%push(toChar(atanh(toReal(evalBuffer%arr(1)%val))))
                                call evalBuffer%clear
                        else if (postfix%peek() == 'ln') then
                                call evalBuffer%push(stackObj%peek())
                                call stackObj%pop()

                                call stackObj%push(toChar(log(toReal(evalBuffer%arr(1)%val))))
                                call evalBuffer%clear
                        end if
                        call postfix%pop()
                end do

                res = toReal(stackObj%arr(1)%val)
        end function eval
end module funtran_standlib

