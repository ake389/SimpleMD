module timestepping
    use inputs, only: wp
    implicit none
    
    public :: update_position
    public :: update_velocity
    private :: compute_v_RHS
    public :: compute_dt

contains

    subroutine update_position(x,y,u,v,dt)
        use inputs, only: Np
        implicit none

        real(wp), intent(inout) :: x(Np)
        real(wp), intent(inout) :: y(Np)
        real(wp), intent(in   ) :: u(Np)
        real(wp), intent(in   ) :: v(Np)
        real(wp), intent(in   ) :: dt

        real(wp) :: RHS(Np,2)

        RHS(:,1) = u
        RHS(:,2) = v
        x = x + (dt*RHS(:,1))
        y = y + (dt*RHS(:,2))
        
    end subroutine

    subroutine update_velocity(x,y,u,v,dt)
        use inputs, only: Np
        implicit none

        real(wp), intent(in   ) :: x(Np)
        real(wp), intent(in   ) :: y(Np)
        real(wp), intent(inout) :: u(Np)
        real(wp), intent(inout) :: v(Np)
        real(wp), intent(in   ) :: dt

        real(wp) :: RHS(Np,2)

        call compute_v_RHS(x,y,RHS)
        u = u + (dt*RHS(:,1))
        v = v + (dt*RHS(:,2))
        
    end subroutine

    subroutine compute_v_RHS(x,y,RHS)
        use inputs, only: Np,m_p
        use interaction, only: force
        implicit none

        real(wp), intent(in   ) :: x(Np)
        real(wp), intent(in   ) :: y(Np)
        real(wp), intent(  out) :: RHS(Np,2)

        real(wp) :: F(2),F_ab(2)
        integer :: ii,jj

        do ii = 1,Np
            F = 0._wp
            do jj = 1,Np
                call force((x(ii)-x(jj)),(y(ii)-y(jj)),F_ab)
                F = F + F_ab
            enddo
            RHS(ii,:) = F/m_p
        enddo
        
    end subroutine

    subroutine compute_dt(x,y,u,v,dt)
        use inputs, only: Np,CFL
        implicit none

        real(wp), intent(in   ) :: x(Np)
        real(wp), intent(in   ) :: y(Np)
        real(wp), intent(in   ) :: u(Np)
        real(wp), intent(in   ) :: v(Np)
        real(wp), intent(  out) :: dt

        integer :: ii,jj
        real(wp) :: r,theta,s,Q

        Q = 100000._wp

        do ii = 1,Np
            do jj = 1,Np
                r = sqrt(((x(ii)-x(jj))**2)+((y(ii)-y(jj))**2))
                theta = atan((y(ii)-y(jj))/(x(ii)-x(jj)))
                s = sqrt(((u(ii)-u(jj))**2)+((v(ii)-v(jj))**2))*cos(theta)
                Q = min(Q,CFL*r/s)
            enddo
        enddo

        dt = min(max(Q,0.01_wp),1._wp)
        
    end subroutine

    subroutine sanitize_positions(x,y)
        use inputs, only: Np,sigma
        implicit none

        real(wp), intent(inout) :: x(Np)
        real(wp), intent(inout) :: y(Np)

        integer :: ii,jj

        do ii = 1,Np
            do jj = 1,Np
                if (sqrt(((x(ii)-x(jj))**2)+((y(ii)-y(jj))**2)) .le. sigma) then
                    x(ii) = x(ii) + (2._wp*sigma)
                    y(jj) = y(jj) - (2._wp*sigma)
                endif
            enddo
        enddo
        
    end subroutine
    
end module