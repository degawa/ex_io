module ex_io
    use, intrinsic :: iso_fortran_env
    use :: stdlib_strings
    use :: stdlib_error
    use :: json_module
    implicit none
    private
    public :: load_configure_file
    public :: construct, finalize
    public :: compute_distance_function
    public :: output_distance_function

    integer(int32), private :: num_x
        !! x軸方向格子点数
    integer(int32), private :: num_y
        !! y軸方向格子点数
    integer(int32), private :: num_z
        !! z軸方向格子点数

    real(real64), private, allocatable :: x(:)
        !! x軸方向の座標値
    real(real64), private, allocatable :: y(:)
        !! y軸方向の座標値
    real(real64), private, allocatable :: z(:)
        !! z軸方向の座標値

contains
    !| 設定ファイルを読み込む．
    subroutine load_configure_file(config_json, filename)
        use :: stdlib_logger
        implicit none

        type(json_file), intent(inout) :: config_json
            !! 設定ファイル
        character(*), intent(in) :: filename
            !! 設定ファイル名．拡張子は不要

        block
            logical :: exists
            inquire (file=filename//".json", exist=exists)
            call check(exists, filename//".json does not exist")
        end block

        call config_json%load(filename=filename//".json")
        call config_json%print_error_message(output_unit)

    end subroutine load_configure_file

    !| 設定ファルの内容に基づいて配列を割り付ける．
    subroutine construct(val, config_json)
        use :: stdlib_math
        implicit none

        real(real64), allocatable, intent(inout) :: val(:, :, :)
            !! 割り付ける変数<br>
            !! 割り付けられるサイズは`("number of grid points.x","number of grid points.y","number of grid points.z")`
        type(json_file), intent(inout) :: config_json
            !! 設定ファイル

        real(real64) :: x_min, x_max
        real(real64) :: y_min, y_max
        real(real64) :: z_min, z_max

        integer(int32) :: alloc_stat
        character(:), allocatable :: path

        ! 計算領域の上下限値を読み込み
        path = "physical conditions.space."
        call config_json%get(path//"x.min", x_min)
        call config_json%get(path//"x.max", x_max)
        call config_json%get(path//"y.min", y_min)
        call config_json%get(path//"y.max", y_max)
        call config_json%get(path//"z.min", z_min)
        call config_json%get(path//"z.max", z_max)

        ! 格子点数を読み込み
        path = "discrete conditions.number of grid points."
        call config_json%get(path//"x", num_x)
        call config_json%get(path//"y", num_y)
        call config_json%get(path//"z", num_z)

        ! メモリを割り付け
        allocate (val(1:num_x, 1:num_y, 1:num_z), source=0d0, stat=alloc_stat)
        call check(alloc_stat == 0, msg="allocation faild")

        ! 座標値を設定
        x = linspace(x_min, x_max, num_x) ! x座標
        y = linspace(y_min, y_max, num_y) ! y座標
        z = linspace(z_min, z_max, num_z) ! z座標
    end subroutine construct

    !| 割り付けた配列を解放する．
    subroutine finalize(val)
        implicit none
        real(real64), allocatable, intent(inout) :: val(:, :, :)

        if (allocated(val)) deallocate (val)
        if (allocated(x)) deallocate (x)
        if (allocated(y)) deallocate (y)
        if (allocated(z)) deallocate (z)
    end subroutine finalize

    !| 距離関数を計算する．
    subroutine compute_distance_function(val, config_json)
        implicit none

        real(real64), intent(inout) :: val(:, :, :)
            !! 距離関数値`=val(1:num_x,1:num_y,1:num_z)`
        type(json_file), intent(inout) :: config_json
            !! 設定ファイル

        character(32), allocatable :: objects_name(:)
        character(:), allocatable :: path

        ! 物体の名前を読み込み
        path = "objects.objects name"
        call config_json%get(path, objects_name)

        block
            integer(int32) :: n

            ! 物体がない場合は，界面が非常に遠いところにあると見なし，実数の最大値で初期化
            val = huge(val)
            do n = 1, size(objects_name)
                select case (objects_name(n))
                case ("rect")
                    ! 直方体周りの距離関数の計算
                    call compute_distance_function_rect(val, config_json)
                case ("sphere")
                    ! 球周りの距離関数の計算
                    call compute_distance_function_sphere(val, config_json)
                end select

            end do
        end block
    end subroutine compute_distance_function

    !| 直方体周りの距離関数を計算する．
    subroutine compute_distance_function_rect(val, config_json)
        implicit none

        real(real64), intent(inout) :: val(:, :, :)
            !! 距離関数値`=val(1:num_x,1:num_y,1:num_z)`
        type(json_file), intent(inout) :: config_json
            !! 設定ファイル

        real(real64) :: center_x, center_y, center_z
        real(real64) :: length_x, length_y, length_z

        block
            character(:), allocatable :: path

            ! 直方体の中心座標を読み込み
            path = "objects.rect.center."
            call config_json%get(path//"x", center_x)
            call config_json%get(path//"y", center_y)
            call config_json%get(path//"z", center_z)
        end block

        block
            character(:), allocatable :: path

            ! 直方体の各辺の長さを読み込み
            path = "objects.rect.length."
            call config_json%get(path//"x", length_x)
            call config_json%get(path//"y", length_y)
            call config_json%get(path//"z", length_z)
        end block

        block
            integer(int32) :: i, j, k
            real(real64) :: d, x_, y_, z_

            ! 距離関数を計算
            ! min( max(|x|-Lx/2, |y|-Ly/2, |z|-Lz/2), 0) + √( max(|x|-Lx/2, 0)^2 + max(|y|-Ly/2, 0)^2 + max(|z|-Lz/2, 0)^2 )
            do concurrent(k=1:num_z, j=1:num_y, i=1:num_x)
                x_ = x(i) - center_x
                y_ = y(j) - center_y
                z_ = z(k) - center_z

                !&<
                d =  min(max(abs(x_) - length_x/2d0, &
                             abs(y_) - length_y/2d0, &
                             abs(z_) - length_z/2d0), 0d0) &
                    +sqrt( max(abs(x_) - length_x/2d0, 0d0)**2 &
                          +max(abs(y_) - length_y/2d0, 0d0)**2 &
                          +max(abs(z_) - length_z/2d0, 0d0)**2)
                !&>

                val(i, j, k) = min(val(i, j, k), d) ! より界面に近い方を採用する
            end do
        end block
    end subroutine compute_distance_function_rect

    !| 球周りの距離関数を計算する．
    subroutine compute_distance_function_sphere(val, config_json)
        implicit none

        real(real64), intent(inout) :: val(:, :, :)
            !! 距離関数値`=val(1:num_x,1:num_y,1:num_z)`
        type(json_file), intent(inout) :: config_json
            !! 設定ファイル

        real(real64) :: center_x, center_y, center_z
        real(real64) :: radius

        block
            character(:), allocatable :: path

            ! 球の中心座標を読み込み
            path = "objects.sphere.center."
            call config_json%get(path//"x", center_x)
            call config_json%get(path//"y", center_y)
            call config_json%get(path//"z", center_z)
        end block

        block
            character(:), allocatable :: path

            ! 球の半径を読み込み
            path = "objects.sphere."
            call config_json%get(path//"radius", radius)
        end block

        block
            integer(int32) :: i, j, k
            real(real64) :: d, x_, y_, z_

            ! 距離関数を計算
            ! √( (x-c_x)^2 + (y-c_y)^2 + (z-c_z)^2 ) - r
            do concurrent(k=1:num_z, j=1:num_y, i=1:num_x)
                x_ = x(i) - center_x
                y_ = y(j) - center_y
                z_ = z(k) - center_z

                d = sqrt(x_**2 + y_**2 + z_**2) - radius

                val(i, j, k) = min(val(i, j, k), d) ! より界面に近い方を採用する
            end do
        end block
    end subroutine compute_distance_function_sphere

    !| 計算した距離関数をvtr形式で出力する．
    subroutine output_distance_function(val, filename)
        use :: vtk_fortran, only:vtk_file
        implicit none

        real(real64), intent(in) :: val(:, :, :)
            !! 距離関数値`=val(1:num_x,1:num_y,1:num_z)`
        character(*), intent(in) :: filename
            !! 設定ファイル

        type(vtk_file) :: vtr
        integer(int32) :: stat

        stat = vtr%initialize(format="raw", &
                              filename=filename//".vtr", &
                              mesh_topology="RectilinearGrid", &
                              nx1=1, nx2=num_x, ny1=1, ny2=num_y, nz1=1, nz2=num_z)

        stat = vtr%xml_writer%write_piece(nx1=1, nx2=num_x, ny1=1, ny2=num_y, nz1=1, nz2=num_z)

        ! 各方向座標値の書き出し
        stat = vtr%xml_writer%write_geo(x=x, y=y, z=z)

        ! 配列の書き出し
        stat = vtr%xml_writer%write_dataarray(location="node", action="open")
        stat = vtr%xml_writer%write_dataarray(x=val, data_name="distance function", one_component=.true.)
        stat = vtr%xml_writer%write_dataarray(location="node", action="close")

        stat = vtr%xml_writer%write_piece()

        stat = vtr%finalize()
    end subroutine output_distance_function
end module ex_io
