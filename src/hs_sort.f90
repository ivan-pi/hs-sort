module hs_sort

  implicit none

  integer, parameter :: dp = kind(1.0d0)

  interface
    subroutine isort(n,list,key)
      import dp
      integer, intent(in) :: n
      integer, intent(inout) :: list(n)
      real(dp), intent(in) :: key(n)
    end subroutine
    subroutine qsort(n,list,key)
      import dp
      integer, intent(in) :: n
      integer, intent(inout) :: list(n)
      real(dp), intent(in) :: key(n)
    end subroutine
    subroutine hsort(n,list,key)
      import dp
      integer, intent(in) :: n
      integer, intent(inout) :: list(n)
      real(dp), intent(in) :: key(n)
    end subroutine
  end interface

end module