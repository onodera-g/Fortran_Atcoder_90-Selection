program Sign_Up_Requests
    ! N              : 登録申請の数
    ! initial_chr    : ユーザ名の最初の文字のASCIIコード
    ! num_registered : 各初期文字ごとの登録されたユーザ名の数を追跡する配列
    ! registered     : それぞれの初期文字に基づいて登録されたユーザ名を保存する2次元配列
    ! name           : ユーザ名を格納する文字列
    implicit none
    integer :: N, i, j, initial_chr
    integer :: num_registered(48:122)
    character(len=15) :: registered(10**5, 48:122), name

    !入力
    read (*, *) N
    num_registered = 0

    ! 各日のユーザ申請を処理
    do i = 1, N
        ! ユーザ名の入力
        read (*, '(A)') name

        ! ユーザ名の1文字のASCIIコードを取得
        initial_chr = iachar(name(1:1))

        ! 登録済みユーザ名のチェック
        do j = 1, num_registered(initial_chr)
            if (name == registered(j, initial_chr)) exit ! 登録済みの場合は処理終了
        end do

        ! 新しいユーザ名なら登録
        if (j > num_registered(initial_chr)) then
            num_registered(initial_chr) = num_registered(initial_chr) + 1
            registered(num_registered(initial_chr), initial_chr) = name
            print *, i ! 登録された日の番号を出力
        end if
    end do
end program Sign_Up_Requests
