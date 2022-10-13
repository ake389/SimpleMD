module plotting
    use inputs, only: wp
    implicit none

    public :: plot
    
contains

    subroutine plot (tt,t,x,y)
        use inputs, only: Np
        implicit none

        integer, intent(in   ) :: tt
        real(wp), intent(in   ) :: t
        real(wp), intent(in   ) :: x(Np)
        real(wp), intent(in   ) :: y(Np)

        integer :: ii

        character(len=100) :: f,st

        write(st,'(I9.9)') tt
        f = '/home/jposey/Desktop/pltMD/plt'//trim(st)//'.dat'
  
        open(1,file = f)
        write(1,"(f15.7,A,(I9.3))") t,',',tt
        do ii = 1,Np
            write(1,"(f15.7,A,f15.7)") x(ii),',',y(ii)
        enddo

    end subroutine
    
end module