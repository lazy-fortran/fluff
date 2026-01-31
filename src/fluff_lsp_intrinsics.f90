module fluff_lsp_intrinsics
    implicit none
    private

    public :: intrinsic_info_t
    public :: get_intrinsic_info

    ! Intrinsic function information
    type :: intrinsic_info_t
        character(len=:), allocatable :: signature
        character(len=:), allocatable :: documentation
        character(len=:), allocatable :: kind
    end type intrinsic_info_t

contains

    subroutine get_intrinsic_info(token, info, found)
        character(len=*), intent(in) :: token
        type(intrinsic_info_t), intent(out) :: info
        logical, intent(out) :: found

        info%signature = ""
        info%documentation = ""
        info%kind = ""
        found = .false.

        select case (token)
            ! Trigonometric functions
        case ("sin")
            info%signature = "elemental real function sin(x)"
            info%documentation = "Computes the sine of x (in radians)"
            info%kind = "intrinsic"
            found = .true.
        case ("cos")
            info%signature = "elemental real function cos(x)"
            info%documentation = "Computes the cosine of x (in radians)"
            info%kind = "intrinsic"
            found = .true.
        case ("tan")
            info%signature = "elemental real function tan(x)"
            info%documentation = "Computes the tangent of x (in radians)"
            info%kind = "intrinsic"
            found = .true.
        case ("asin")
            info%signature = "elemental real function asin(x)"
            info%documentation = "Computes the arc sine of x in radians"
            info%kind = "intrinsic"
            found = .true.
        case ("acos")
            info%signature = "elemental real function acos(x)"
            info%documentation = "Computes the arc cosine of x in radians"
            info%kind = "intrinsic"
            found = .true.
        case ("atan")
            info%signature = "elemental real function atan(x) or atan(y, x)"
            info%documentation = "Computes the arc tangent of x or atan2(y, x)"
            info%kind = "intrinsic"
            found = .true.
        case ("atan2")
            info%signature = "elemental real function atan2(y, x)"
            info%documentation = "Computes the arc tangent of y/x using signs of both"
            info%kind = "intrinsic"
            found = .true.
        case ("sinh")
            info%signature = "elemental real function sinh(x)"
            info%documentation = "Computes the hyperbolic sine of x"
            info%kind = "intrinsic"
            found = .true.
        case ("cosh")
            info%signature = "elemental real function cosh(x)"
            info%documentation = "Computes the hyperbolic cosine of x"
            info%kind = "intrinsic"
            found = .true.
        case ("tanh")
            info%signature = "elemental real function tanh(x)"
            info%documentation = "Computes the hyperbolic tangent of x"
            info%kind = "intrinsic"
            found = .true.

            ! Math functions
        case ("sqrt")
            info%signature = "elemental real function sqrt(x)"
            info%documentation = "Computes the square root of x"
            info%kind = "intrinsic"
            found = .true.
        case ("abs")
            info%signature = "elemental function abs(a)"
            info%documentation = "Computes the absolute value of a"
            info%kind = "intrinsic"
            found = .true.
        case ("exp")
            info%signature = "elemental real function exp(x)"
            info%documentation = "Computes the exponential of x (e**x)"
            info%kind = "intrinsic"
            found = .true.
        case ("log")
            info%signature = "elemental real function log(x)"
            info%documentation = "Computes the natural logarithm of x"
            info%kind = "intrinsic"
            found = .true.
        case ("log10")
            info%signature = "elemental real function log10(x)"
            info%documentation = "Computes the base-10 logarithm of x"
            info%kind = "intrinsic"
            found = .true.
        case ("mod")
            info%signature = "elemental function mod(a, p)"
            info%documentation = "Computes the remainder of a divided by p"
            info%kind = "intrinsic"
            found = .true.
        case ("modulo")
            info%signature = "elemental function modulo(a, p)"
            info%documentation = "Computes a modulo p (floored division remainder)"
            info%kind = "intrinsic"
            found = .true.
        case ("min")
            info%signature = "elemental function min(a1, a2, ...)"
            info%documentation = "Returns the minimum of the arguments"
            info%kind = "intrinsic"
            found = .true.
        case ("max")
            info%signature = "elemental function max(a1, a2, ...)"
            info%documentation = "Returns the maximum of the arguments"
            info%kind = "intrinsic"
            found = .true.
        case ("floor")
            info%signature = "elemental integer function floor(a, kind)"
            info%documentation = "Returns the greatest integer less than or equal to a"
            info%kind = "intrinsic"
            found = .true.
        case ("ceiling")
            info%signature = "elemental integer function ceiling(a, kind)"
            info%documentation = &
                "Returns the smallest integer greater than or equal to a"
            info%kind = "intrinsic"
            found = .true.
        case ("nint")
            info%signature = "elemental integer function nint(a, kind)"
            info%documentation = "Returns the nearest integer to a"
            info%kind = "intrinsic"
            found = .true.
        case ("sign")
            info%signature = "elemental function sign(a, b)"
            info%documentation = "Returns abs(a) with the sign of b"
            info%kind = "intrinsic"
            found = .true.

            ! Array functions
        case ("size")
            info%signature = "integer function size(array, dim, kind)"
            info%documentation = "Returns the total size or extent along dimension"
            info%kind = "intrinsic"
            found = .true.
        case ("shape")
            info%signature = "integer function shape(source, kind)"
            info%documentation = "Returns the shape of an array"
            info%kind = "intrinsic"
            found = .true.
        case ("lbound")
            info%signature = "integer function lbound(array, dim, kind)"
            info%documentation = "Returns the lower bounds of an array"
            info%kind = "intrinsic"
            found = .true.
        case ("ubound")
            info%signature = "integer function ubound(array, dim, kind)"
            info%documentation = "Returns the upper bounds of an array"
            info%kind = "intrinsic"
            found = .true.
        case ("sum")
            info%signature = "function sum(array, dim, mask)"
            info%documentation = "Sums all elements or along a dimension"
            info%kind = "intrinsic"
            found = .true.
        case ("product")
            info%signature = "function product(array, dim, mask)"
            info%documentation = "Computes the product of array elements"
            info%kind = "intrinsic"
            found = .true.
        case ("maxval")
            info%signature = "function maxval(array, dim, mask)"
            info%documentation = "Returns the maximum value in an array"
            info%kind = "intrinsic"
            found = .true.
        case ("minval")
            info%signature = "function minval(array, dim, mask)"
            info%documentation = "Returns the minimum value in an array"
            info%kind = "intrinsic"
            found = .true.
        case ("count")
            info%signature = "integer function count(mask, dim, kind)"
            info%documentation = "Counts the number of true elements in mask"
            info%kind = "intrinsic"
            found = .true.
        case ("any")
            info%signature = "logical function any(mask, dim)"
            info%documentation = "Returns true if any element of mask is true"
            info%kind = "intrinsic"
            found = .true.
        case ("all")
            info%signature = "logical function all(mask, dim)"
            info%documentation = "Returns true if all elements of mask are true"
            info%kind = "intrinsic"
            found = .true.
        case ("pack")
            info%signature = "function pack(array, mask, vector)"
            info%documentation = "Packs array elements where mask is true"
            info%kind = "intrinsic"
            found = .true.
        case ("unpack")
            info%signature = "function unpack(vector, mask, field)"
            info%documentation = "Unpacks vector elements into an array"
            info%kind = "intrinsic"
            found = .true.
        case ("reshape")
            info%signature = "function reshape(source, shape, pad, order)"
            info%documentation = "Reshapes an array to a new shape"
            info%kind = "intrinsic"
            found = .true.
        case ("transpose")
            info%signature = "function transpose(matrix)"
            info%documentation = "Transposes a rank-2 array"
            info%kind = "intrinsic"
            found = .true.
        case ("matmul")
            info%signature = "function matmul(matrix_a, matrix_b)"
            info%documentation = "Performs matrix multiplication"
            info%kind = "intrinsic"
            found = .true.
        case ("dot_product")
            info%signature = "function dot_product(vector_a, vector_b)"
            info%documentation = "Computes the dot product of two vectors"
            info%kind = "intrinsic"
            found = .true.
        case ("merge")
            info%signature = "elemental function merge(tsource, fsource, mask)"
            info%documentation = "Chooses between tsource and fsource based on mask"
            info%kind = "intrinsic"
            found = .true.
        case ("spread")
            info%signature = "function spread(source, dim, ncopies)"
            info%documentation = "Replicates an array by adding a dimension"
            info%kind = "intrinsic"
            found = .true.

            ! String functions
        case ("trim")
            info%signature = "character function trim(string)"
            info%documentation = "Removes trailing blanks from a string"
            info%kind = "intrinsic"
            found = .true.
        case ("adjustl")
            info%signature = "elemental character function adjustl(string)"
            info%documentation = "Left-justifies a string"
            info%kind = "intrinsic"
            found = .true.
        case ("adjustr")
            info%signature = "elemental character function adjustr(string)"
            info%documentation = "Right-justifies a string"
            info%kind = "intrinsic"
            found = .true.
        case ("len")
            info%signature = "integer function len(string, kind)"
            info%documentation = "Returns the length of a string"
            info%kind = "intrinsic"
            found = .true.
        case ("len_trim")
            info%signature = "elemental integer function len_trim(string, kind)"
            info%documentation = "Returns the length without trailing blanks"
            info%kind = "intrinsic"
            found = .true.
        case ("index")
            info%signature = &
                "elemental integer function index(string, substring, back, kind)"
            info%documentation = "Returns the position of substring in string"
            info%kind = "intrinsic"
            found = .true.
        case ("scan")
            info%signature = "elemental integer function scan(string, set, back, kind)"
            info%documentation = "Scans string for any character in set"
            info%kind = "intrinsic"
            found = .true.
        case ("verify")
            info%signature = &
                "elemental integer function verify(string, set, back, kind)"
            info%documentation = "Verifies all characters are in set"
            info%kind = "intrinsic"
            found = .true.
        case ("repeat")
            info%signature = "character function repeat(string, ncopies)"
            info%documentation = "Concatenates copies of a string"
            info%kind = "intrinsic"
            found = .true.

            ! Type conversion functions
        case ("real")
            info%signature = "elemental real function real(a, kind)"
            info%documentation = "Converts to real type"
            info%kind = "intrinsic"
            found = .true.
        case ("int")
            info%signature = "elemental integer function int(a, kind)"
            info%documentation = "Converts to integer type (truncates toward zero)"
            info%kind = "intrinsic"
            found = .true.
        case ("dble")
            info%signature = "elemental double precision function dble(a)"
            info%documentation = "Converts to double precision real"
            info%kind = "intrinsic"
            found = .true.
        case ("cmplx")
            info%signature = "elemental complex function cmplx(x, y, kind)"
            info%documentation = "Creates a complex number"
            info%kind = "intrinsic"
            found = .true.
        case ("char")
            info%signature = "elemental character function char(i, kind)"
            info%documentation = "Returns the character for ASCII code i"
            info%kind = "intrinsic"
            found = .true.
        case ("ichar")
            info%signature = "elemental integer function ichar(c, kind)"
            info%documentation = "Returns the ASCII code of character c"
            info%kind = "intrinsic"
            found = .true.
        case ("transfer")
            info%signature = "function transfer(source, mold, size)"
            info%documentation = "Transfers bit pattern to a different type"
            info%kind = "intrinsic"
            found = .true.
        case ("logical")
            info%signature = "elemental logical function logical(l, kind)"
            info%documentation = "Converts to logical type"
            info%kind = "intrinsic"
            found = .true.

            ! Inquiry functions
        case ("kind")
            info%signature = "integer function kind(x)"
            info%documentation = "Returns the kind parameter of x"
            info%kind = "intrinsic"
            found = .true.
        case ("allocated")
            info%signature = "logical function allocated(array)"
            info%documentation = "Returns true if array is allocated"
            info%kind = "intrinsic"
            found = .true.
        case ("associated")
            info%signature = "logical function associated(pointer, target)"
            info%documentation = "Returns true if pointer is associated"
            info%kind = "intrinsic"
            found = .true.
        case ("present")
            info%signature = "logical function present(a)"
            info%documentation = "Returns true if optional argument is present"
            info%kind = "intrinsic"
            found = .true.
        case ("huge")
            info%signature = "function huge(x)"
            info%documentation = "Returns the largest number of same kind as x"
            info%kind = "intrinsic"
            found = .true.
        case ("tiny")
            info%signature = "function tiny(x)"
            info%documentation = "Returns the smallest positive number of same kind"
            info%kind = "intrinsic"
            found = .true.
        case ("epsilon")
            info%signature = "function epsilon(x)"
            info%documentation = "Returns the smallest number e such that 1+e > 1"
            info%kind = "intrinsic"
            found = .true.
        case ("precision")
            info%signature = "integer function precision(x)"
            info%documentation = "Returns the decimal precision of x"
            info%kind = "intrinsic"
            found = .true.
        case ("range")
            info%signature = "integer function range(x)"
            info%documentation = "Returns the decimal exponent range of x"
            info%kind = "intrinsic"
            found = .true.
        case ("digits")
            info%signature = "integer function digits(x)"
            info%documentation = "Returns the number of significant digits"
            info%kind = "intrinsic"
            found = .true.
        case ("bit_size")
            info%signature = "integer function bit_size(i)"
            info%documentation = "Returns the number of bits in the representation"
            info%kind = "intrinsic"
            found = .true.

            ! Bit manipulation
        case ("iand")
            info%signature = "elemental integer function iand(i, j)"
            info%documentation = "Performs bitwise AND"
            info%kind = "intrinsic"
            found = .true.
        case ("ior")
            info%signature = "elemental integer function ior(i, j)"
            info%documentation = "Performs bitwise OR"
            info%kind = "intrinsic"
            found = .true.
        case ("ieor")
            info%signature = "elemental integer function ieor(i, j)"
            info%documentation = "Performs bitwise exclusive OR"
            info%kind = "intrinsic"
            found = .true.
        case ("not")
            info%signature = "elemental integer function not(i)"
            info%documentation = "Performs bitwise NOT"
            info%kind = "intrinsic"
            found = .true.
        case ("btest")
            info%signature = "elemental logical function btest(i, pos)"
            info%documentation = "Tests bit at position pos"
            info%kind = "intrinsic"
            found = .true.
        case ("ibset")
            info%signature = "elemental integer function ibset(i, pos)"
            info%documentation = "Sets bit at position pos to 1"
            info%kind = "intrinsic"
            found = .true.
        case ("ibclr")
            info%signature = "elemental integer function ibclr(i, pos)"
            info%documentation = "Clears bit at position pos to 0"
            info%kind = "intrinsic"
            found = .true.
        case ("ishft")
            info%signature = "elemental integer function ishft(i, shift)"
            info%documentation = "Shifts bits by shift positions"
            info%kind = "intrinsic"
            found = .true.
        end select

    end subroutine get_intrinsic_info

end module fluff_lsp_intrinsics
