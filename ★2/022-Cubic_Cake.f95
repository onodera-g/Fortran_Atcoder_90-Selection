program Cubic_Cake
    ! ABC(1): 幅A, ABC(2): 奥行きB, ABC(3): 高さCを格納する配列
    ! gcd_result: 直方体の幅A、奥行きB、高さCの最大公約数を格納する変数
    ! operations: 各方向で必要な切断回数の合計を格納する変数
    implicit none
    integer(16) :: ABC(3)
    integer(16) :: gcd_result
    integer(16) :: operations

    ! 入力
    read (*, *) ABC(1), ABC(2), ABC(3)

    ! 最大公約数の計算
    call cal_gcd(ABC, gcd_result)

    ! 各方向で必要な切断回数の計算
    operations = (ABC(1)/gcd_result - 1) + (ABC(2)/gcd_result - 1) + (ABC(3)/gcd_result - 1)

    ! 結果を出力
    write (*, *) operations

contains
    subroutine cal_gcd(nums, result)
        ! subroutine cal_gcd: 幅A、奥行きB、高さCの最大公約数を計算するサブルーチン
        ! nums: 幅A、奥行きB、高さCを格納した配列
        ! result: 計算された最大公約数を格納する変数
        implicit none
        integer(16), dimension(3), intent(in) :: nums
        integer(16), intent(out) :: result
        integer(16) :: i !

        ! 初期値として最初の要素のGCDを設定
        result = nums(1)

        ! 全ての数のGCDを計算
        do i = 2, size(nums)
            result = gcd(result, nums(i))
        end do
    end subroutine cal_gcd

    integer(16) function gcd(a, b)
        ! function gcd: 2つの整数のGCDを計算する関数
        ! a, b: GCDを計算する2つの整数
        ! r: 剰余を格納する変数
        ! x, y: a, bの絶対値をコピーし、GCD計算に使用する一時変数
        implicit none
        integer(16) :: a, b, r
        integer(16) :: x, y

        ! x, y に a, b をコピー
        x = abs(a)
        y = abs(b)

        ! ユークリッドの互除法を使用してGCDを計算
        do while (y /= 0)
            r = mod(x, y)
            x = y
            y = r
        end do
        gcd = x
    end function gcd
end program Cubic_Cake
