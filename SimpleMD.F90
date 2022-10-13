program SimpleMD
    use inputs, only: wp,Np,tMax
    use initial_conditions, only: set_initial_conditions
    use timestepping, only: compute_dt,update_position,update_velocity,sanitize_positions
    use walls, only: check_wall_bounce
    use plotting, only: plot
    implicit none

    real(wp) :: x(Np)
    real(wp) :: y(Np)
    real(wp) :: u(Np)
    real(wp) :: v(Np)

    real(wp) :: t
    real(wp) :: dt

    integer :: tt

    call set_initial_conditions(x,y,u,v)
    call check_wall_bounce(x,y,u,v)
    call plot(0,0._wp,x,y)

    t = 0._wp
    tt = 0
    do while ((t .le. tMax) .and. (tt .lt. 10000))
        write(*,*) tt,t,dt
        call compute_dt(x,y,u,v,dt)
        call update_position(x,y,u,v,dt/2._wp)
        call check_wall_bounce(x,y,u,v)
        ! call sanitize_positions(x,y)
        call update_velocity(x,y,u,v,dt)
        call check_wall_bounce(x,y,u,v)
        call update_position(x,y,u,v,dt/2._wp)
        call check_wall_bounce(x,y,u,v)
        ! call sanitize_positions(x,y)
        call plot(tt+1,t+dt,x,y)
        tt = tt + 1
        t = t + dt
    enddo

    write(*,*) 'Complete'
    
end program 
