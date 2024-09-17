program Select_5
    ! N   : 整数の数
    ! P   : 割る数
    ! Q   : 余り
    ! cnt : 条件を満たす組み合わせの数
    ! A   : 整数の配列
    implicit none
    integer(16) :: i, j, k, l, m
    integer(16) :: N, P, Q
    integer(16) :: cnt
    integer(16), allocatable :: A(:)

    ! 入力
    read (*, *) N, P, Q
    allocate (A(N))
    read (*, *) A

    ! 5つの要素の積を P で割った余りが Q と等しいかを確認
    cnt = 0
    do i = 1, N - 4
        do j = i + 1, N - 3
            do k = j + 1, N - 2
                do l = k + 1, N - 1
                    do m = l + 1, N
                        if (mod(mod(mod(mod(A(i)*A(j), P)*A(k), P)*A(l), P)*A(m), P) == Q) cnt = cnt + 1
                    end do
                end do
            end do
        end do
    end do

    ! 結果の出力
    print *, cnt
end program Select_5
