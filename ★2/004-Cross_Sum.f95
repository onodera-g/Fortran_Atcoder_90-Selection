program Cross_Sum
    !A    ：整数
    !H,W  ：行と列の数
    !Hn,Wn：各行と各列の合計
    implicit none
    integer i, j
    integer H, W
    integer, allocatable::A(:, :), Hn(:), Wn(:)

    !入力
    read (*, *) H, W
    allocate (A(H, W), Hn(H), Wn(W))
    do i = 1, H
        read (*, *) (A(i, j), j=1, W)
    end do

    !各行列の合計
    do i = 1, H
        Hn(i) = sum(A(i, :)) !各行の合計
    end do
    do j = 1, W
        Wn(j) = sum(A(:, j)) !各列の合計
    end do

    !結果の出力
    do i = 1, H
        do j = 1, W
            if (j /= W) then
                write (*, '(i0,1x)', advance='no') Hn(i) + Wn(j) - A(i, j)
            else
                write (*, '(i0)') Hn(i) + Wn(j) - A(i, j)
            end if
        end do
    end do
end program Cross_Sum
