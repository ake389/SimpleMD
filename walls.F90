module walls
    use inputs, only: wp
    implicit none

    public :: check_wall_bounce
    
contains

    subroutine check_wall_bounce (x,y,u,v)
        use inputs, only: li,hi,Np
        implicit none

        real(wp), intent(inout) :: x(Np)
        real(wp), intent(inout) :: y(Np)
        real(wp), intent(inout) :: u(Np)
        real(wp), intent(inout) :: v(Np)

        integer :: ii

        do ii = 1,Np
            if (x(ii) .le. li(1)) then
                x(ii) = li(1)*1.01_wp
                u(ii) = -u(ii)!*0.999_wp
            endif
            if (x(ii) .ge. hi(1)) then
                x(ii) = hi(1)*0.99_wp
                u(ii) = -u(ii)!*0.999_wp
            endif
            if (y(ii) .le. li(2)) then
                y(ii) = li(2)*1.01_wp
                v(ii) = -v(ii)!*0.999_wp
            endif
            if (y(ii) .ge. hi(2)) then
                y(ii) = hi(2)*0.99_wp
                v(ii) = -v(ii)!*0.999_wp
            endif
        enddo

    end subroutine
    
end module