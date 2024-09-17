program Not_Too_Bright
    ! h : グリッドの縦の長さ
    ! w : グリッドの横の長さ
    use, intrinsic :: iso_fortran_env
    implicit none
    integer :: h, w ! グリッドの縦と横の長さを格納する変数

    ! 入力を受け取る
    read (input_unit, *) h, w

    ! 1行または1列の場合、全てのLEDを点灯可能
    if (h == 1 .or. w == 1) then
        print'(i0)', h*w
    else
        ! それ以外の場合は、縦横それぞれの半分(奇数なら+1)のLEDを点灯できる
        print'(i0)', ((h + 1)/2)*((w + 1)/2)
    end if
end program Not_Too_Bright
