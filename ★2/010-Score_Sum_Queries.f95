program Score_Sum_Queries
    !N    ：生徒の人数
    !P    ：生徒の得点
    !C    ：生徒の得点
    !Q    ：クエリの総数
    !L,R  ：クエリにより合計したい範囲
    !subAB：生徒の得点の合計の累積
    implicit none
    integer i
    integer N, Q
    integer, allocatable::C(:), P(:), L(:), R(:), sumAB(:, :)

    !入力
    read (*, *) N
    allocate (C(N), P(N), sumAB(2, 0:N))
    do i = 1, N
        read (*, *) C(i), P(i)
    end do
    read (*, *) Q
    allocate (L(Q), R(Q))
    do i = 1, Q
        read (*, *) L(i), R(i)
    end do

    !合計の計算
    sumAB = 0
    do i = 1, N
        sumAB(C(i), i) = sumAB(C(i), i - 1) + p(i)
        sumAB(3 - C(i), i) = sumAB(3 - C(i), i - 1)
    end do

    !結果の出力
    do i = 1, Q
        write (*, '(i0,1x)', advance='no') sumAB(1, R(i)) - sumAB(1, L(i) - 1)
        write (*, '(i0)') sumAB(2, R(i)) - sumAB(2, L(i) - 1)
    end do

end program Score_Sum_Queries

