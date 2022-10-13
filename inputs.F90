module inputs
    implicit none

    ! input units are:
    ! L - pm (1e-12m)
    ! m - yg (1e-24g)
    ! t - ps (1e-12s)
    ! E - zJ (1e-21J)

    integer, parameter :: wp = selected_real_kind(15,307)

    integer, parameter :: Np = 20

    ! He
    real(wp), parameter :: sigma = 2.576e2_wp
    real(wp), parameter :: eps = 10.2_wp*1.38e-2_wp !(eps/k_B)*k_B
    real(wp), parameter :: m_p = 6.6464731e-3_wp

    real(wp), parameter :: li(2) = (/0._wp, 0._wp/)
    real(wp), parameter :: hi(2) = (/sigma*100._wp, sigma*100._wp/)

    !Mie Potential
    real(wp), parameter :: n = 12._wp
    real(wp), parameter :: m = 6._wp

    real(wp), parameter :: CFL = 0.5_wp
    real(wp), parameter :: tMax = 1000._wp
    
end module