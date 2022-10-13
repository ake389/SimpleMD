module initial_conditions
    use inputs, only: wp
    implicit none
    
contains

    subroutine set_initial_conditions(x,y,u,v)
        use inputs, only: Np,hi
        implicit none

        real(wp), intent(inout) :: x(Np)
        real(wp), intent(inout) :: y(Np)
        real(wp), intent(inout) :: u(Np)
        real(wp), intent(inout) :: v(Np)
    
        real(wp) :: num1,num2
        integer :: ii

        do ii = 1,Np
            call random_number(num1)
            x(ii) = hi(1)*num1
            call random_number(num1)
            y(ii) = hi(2)*num1
            call random_number(num1)
            call random_number(num2)
            u(ii) = (num1-num2)*1000._wp
            call random_number(num1)
            call random_number(num2)
            v(ii) = (num1-num2)*1000._wp
        enddo
    end subroutine
    
end module