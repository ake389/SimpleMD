module interaction
    use inputs, only: wp
    implicit none

    private :: potentialEnergy
    public :: force
    
contains

    subroutine potentialEnergy (r,U)
        use inputs, only: sigma,eps,n,m
        implicit none

        real(wp), intent(in   ) :: r
        real(wp), intent(  out) :: U

        real(wp) :: C

        C = (n/(n-m))*((n/m)*(m/(n-m)))

        U = C*eps*(((sigma/r)**n)-((sigma/r)**m))

    end subroutine

    subroutine force (x_ab,y_ab,F_ab)
        use inputs, only: sigma
        implicit none

        real(wp), intent(in   ) :: x_ab
        real(wp), intent(in   ) :: y_ab
        real(wp), intent(  out) :: F_ab(2)

        real(wp) :: dx,dy
        real(wp) :: r
        real(wp) :: U_ab_M1,U_ab_P1

        dx = sigma*1.e-16_wp
        dy = sigma*1.e-16_wp

        !x-force
        r = sqrt(((x_ab-dx)*(x_ab-dx))+(y_ab*y_ab))
        call potentialEnergy(r,U_ab_M1)
        r = sqrt(((x_ab+dx)*(x_ab+dx))+(y_ab*y_ab))
        call potentialEnergy(r,U_ab_P1)
        F_ab(1) = -(U_ab_P1-U_ab_M1)/(2._wp*dx)

        !y-force
        r = sqrt((x_ab*x_ab)+((y_ab-dy)*(y_ab-dy)))
        call potentialEnergy(r,U_ab_M1)
        r = sqrt((x_ab*x_ab)+((y_ab+dy)*(y_ab+dy)))
        call potentialEnergy(r,U_ab_P1)
        F_ab(2) = -(U_ab_P1-U_ab_M1)/(2._wp*dy)

    end subroutine
    
end module