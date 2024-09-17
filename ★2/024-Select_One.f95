program Select_One
    ! N   : 配列の長さ, K: 許される操作回数
    ! diff: 配列Aと配列Bの差の合計
    ! A   : 変換前の配列, B: 変換後の目標配列
    implicit none
    integer :: i
    integer :: N, K
    integer :: diff
    integer, allocatable :: A(:), B(:)

    !入力
    read (*, *) N, K
    allocate (A(N), B(N))
    read (*, *) A
    read (*, *) B

    !操作回数の計上
    diff = 0
    do i = 1, N
        diff = diff + abs(A(i) - B(i))
    end do

    !結果の出力
    if (diff <= K .and. mod(K - diff, 2) == 0) then
        print *, 'Yes'
    else
        print *, 'No'
    end if

end program Select_One

