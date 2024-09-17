program Deck
    ! Q      : 操作の数（クエリの数）
    ! top    : 山札の上にカードを追加する位置
    ! under  : 山札の下にカードを追加する位置
    ! t      : 操作の種類を格納する配列
    ! x      : 各操作で使う整数を格納する配列
    ! arr    : 山札の上と下にカードを格納する2次元配列
    implicit none
    integer :: i
    integer :: Q
    integer :: top, under
    integer :: tmp
    integer, allocatable :: t(:), x(:), arr(:, :)

    ! 入力
    read (*, *) Q
    allocate (t(Q), x(Q), arr(2, Q))
    do i = 1, Q
        read (*, *) t(i), x(i)
    end do

    ! カードを整理
    top = 1; under = 1; arr = 0
    do i = 1, Q
        select case (t(i))
        case (1) ! 山札の上にカードを追加
            arr(1, top) = x(i)
            top = top + 1
        case (2) ! 山札の下にカードを追加
            arr(2, under) = x(i)
            under = under + 1
        case (3) ! 山札の上から x(i) 番目のカードを出力
            if (x(i) < top) then
                tmp = top - x(i)
                write (*, *) arr(1, tmp)
            else
                tmp = x(i) - top + 1
                write (*, *) arr(2, tmp)
            end if
        end select
    end do
end program Deck
