program Base_8_to_9
    implicit none
    character(len=50) :: N ! x進数で表される数値 (文字列)
    integer :: x, y ! x: 元の進数, y: 変換後の進数
    character(len=50) :: result ! y進数に変換した結果
    integer :: status

    ! 入力 (元の数値N、x進数、y進数)
    print *, 'Enter the number (as string), base x, and base y:'
    read (*, *) N, x, y

    ! x進数からy進数への変換
    call convert_base(N, x, y, result, status)

    ! 結果の出力
    if (status == 0) then
        print *, 'The number in base ', y, ' is: ', result
    else
        print *, 'Error in conversion!'
    end if

contains
    ! x進数からy進数へ変換するサブルーチン
    subroutine convert_base(num_str, from_base, to_base, result, status)
        character(len=*), intent(in) :: num_str
        integer, intent(in) :: from_base, to_base
        character(len=*), intent(out) :: result
        integer, intent(out) :: status

        integer :: num_10, i, digit, len_str
        character(len=50) :: tmp_result
        character(len=1) :: ch

        result = ''
        status = 0
        num_10 = 0
        len_str = len_trim(num_str)

        ! (1) x進数から10進数へ変換
        do i = 1, len_str
            ch = num_str(i:i)
            select case (ch)
            case ('0':'9')
                digit = ichar(ch) - ichar('0')
            case ('A':'Z')
                digit = ichar(ch) - ichar('A') + 10
            case default
                status = 1 ! 不正な文字が含まれている場合
                return
            end select
            if (digit >= from_base) then
                status = 1 ! 不正な桁
                return
            end if
            num_10 = num_10*from_base + digit
        end do

        ! (2) 10進数からy進数へ変換
        tmp_result = ''
        if (num_10 == 0) then
            result = '0'
            return
        end if

        do while (num_10 > 0)
            digit = mod(num_10, to_base)
            if (digit < 10) then
                tmp_result = char(digit + ichar('0'))//tmp_result
            else
                tmp_result = char(digit - 10 + ichar('A'))//tmp_result
            end if
            num_10 = num_10/to_base
        end do

        result = adjustl(tmp_result)
    end subroutine convert_base
end program Base_8_to_9
