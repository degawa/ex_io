program main
    use, intrinsic :: iso_fortran_env
    use :: json_module
    use :: stdlib_logger
    use :: ex_io
    implicit none

    type(json_file) :: config_json
    real(real64), allocatable :: d(:, :, :)

    call config_json%initialize()

    call load_configure_file(config_json, filename="config")
    call construct(d, config_json)
    call compute_distance_function(d, config_json)
    call output_distance_function(d, "distance_function")

    call finalize(d)
    call config_json%destroy()
end program main
