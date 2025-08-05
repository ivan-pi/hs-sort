module hs_sort

   implicit none

   integer, parameter, public :: dp = kind(1.0d0)
   private
   public :: hsort, isort, qsort
   public :: is_sorted
   public :: ASCENDING, DESCENDING

   enum, bind(C)
      enumerator :: ASCENDING = 1
      enumerator :: DESCENDING = 2
   end enum

contains
   subroutine isort(list, key)
      integer, intent(inout) :: list(:)
      real(dp), intent(in) :: key(:)

      if (.not. is_sorted(list)) error stop "List is not sorted!"
      call isort_pure(list, key)
   end subroutine isort
   subroutine qsort(list, key)
      integer, intent(inout) :: list(:)
      real(dp), intent(in) :: key(:)

      if (.not. is_sorted(list)) error stop "List is not sorted!"
      call qsort_pure(list, key)
   end subroutine qsort

   subroutine hsort(list, key)
      integer, intent(inout) :: list(:)
      real(dp), intent(in) :: key(:)
      if (.not. is_sorted(list)) error stop "List is not sorted!"
      call hsort_pure(list, key)
   end subroutine hsort

   pure function is_sorted(array, order) result(sorted)
      implicit none

      integer, intent(in) :: array(:)
      integer, intent(in), optional :: order
      logical :: sorted
      integer :: sort_order
      integer :: i

      sorted = .true.
      sort_order = ASCENDING  ! default to ascending

      if (present(order)) then
         sort_order = order
      end if

      select case (sort_order)
      case (DESCENDING)
         do i = 1, size(array) - 1
            if (array(i + 1) > array(i)) then
               sorted = .false.
               return
            end if
         end do
      case default  ! ASCENDING or any other value
         do i = 1, size(array) - 1
            if (array(i + 1) < array(i)) then
               sorted = .false.
               return
            end if
         end do
      end select

   end function is_sorted

   pure subroutine hsort_pure(list, key)
!     ORDER INTEGERS STORES IN 'LIST' IN ASCENDING SEQUENCE OF THEIR KEY
!     VALUES STORED IN KEY
!
!     list - array of integers to be sorted
!     key  - array of real numbers used as keys for sorting
!
!     NOTES:
!
!     USES QUICKSORT ALGORITHM, EXCEPT FOR SEGMENTS OF THE LIST SHORTER
!     THAN 'NCUT' FOR WHICH A SECOND PASS INSERTION SORT IS USED. 'NCUT'
!     SHOULD BE SET TO AN INTEGER OF THE ORDER OF 12 FOR MAXIMUM
!     EFFICIENCY, BUT MAY BE SYSTEM DEPENDENT.
!
!     ROUTINE SORTS LISTS UP TO LENGTH 2**MAXSTK
      implicit none
      integer, intent(inout) :: list(:)
      real(dp), intent(in) :: key(:)

      integer, parameter :: maxstk = 32, ncut = 12
      integer :: ll, lr, lm, nl, nr, ltemp, stktop, i, j, k
      integer :: lstack(maxstk), rstack(maxstk)
      real(dp) :: guess, value
      logical :: quicksort_done
      integer :: n
      n = size(list, 1)
      ll = 1
      lr = n
      stktop = 0

      quicksort_done = .false.

      do while (.not. quicksort_done)

         if ((lr - ll) >= ncut) then
            nl = ll
            nr = lr
            lm = (ll + lr)/2
            guess = key(list(lm))

            do
               do while (key(list(nl)) < guess)
                  nl = nl + 1
               end do

               do while (guess < key(list(nr)))
                  nr = nr - 1
               end do

               if (nl < (nr - 1)) then
                  ltemp = list(nl)
                  list(nl) = list(nr)
                  list(nr) = ltemp
                  nl = nl + 1
                  nr = nr - 1
               else
                  exit
               end if
            end do

            if (nl <= nr) then
               if (nl < nr) then
                  ltemp = list(nl)
                  list(nl) = list(nr)
                  list(nr) = ltemp
               end if
               nl = nl + 1
               nr = nr - 1
            end if

            stktop = stktop + 1
            if (nr < lm) then
               lstack(stktop) = nl
               rstack(stktop) = lr
               lr = nr
            else
               lstack(stktop) = ll
               rstack(stktop) = nr
               ll = nl
            end if

         else if (stktop /= 0) then
            ll = lstack(stktop)
            lr = rstack(stktop)
            stktop = stktop - 1

         else
            quicksort_done = .true.
         end if

      end do

      ! Second pass: Insertion sort for final ordering

      j = 1
      k = list(1)
      value = key(k)
      do i = 2, min(n, ncut)
         if (key(list(i)) < value) then
            j = i
            value = key(list(i))
         end if
      end do
      list(1) = list(j)
      list(j) = k

      do i = 2, n
         j = i
         k = list(i)
         value = key(k)
         do while (j > 1 .and. value < key(list(j - 1)))
            list(j) = list(j - 1)
            j = j - 1
         end do
         list(j) = k
      end do

   end subroutine hsort_pure

   pure subroutine isort_pure(list, key)
!     ORDER INTEGERS STORES IN 'LIST' IN ASCENDING SEQUENCE OF THEIR KEY
!     VALUES STORED IN KEY
!
!     list - array of integers to be sorted
!     key  - array of real numbers used as keys for sorting
!
!     NOTES:
!     USES INSERTION SORT - EFFICIENT ONLY FOR 'N' VALUES LESS THAN
!     ABOUT 12 (ALTHOUGH MAY BE SYSTEM DEPENDENT)
      implicit none
      integer, intent(inout) :: list(:)
      real(dp), intent(in) :: key(:)

      integer :: n
      integer :: i, j, k
      real(dp) :: value

      ! Find the index with the minimum key and move it to the front
      n = size(list, 1)
      j = 1
      k = list(1)
      value = key(k)
      do i = 2, n
         if (key(list(i)) < value) then
            j = i
            value = key(list(i))
         end if
      end do
      list(1) = list(j)
      list(j) = k

      ! Insertion sort
      do i = 2, n
         j = i
         k = list(i)
         value = key(k)
         do while (j > 1 .and. value < key(list(j - 1)))
            list(j) = list(j - 1)
            j = j - 1
         end do
         list(j) = k
      end do

   end subroutine isort_pure

   subroutine qsort_pure(list, key)
!     ORDER INTEGERS STORES IN 'LIST' IN ASCENDING SEQUENCE OF THEIR KEY
!     VALUES STORED IN KEY
!
!     list - array of integers to be sorted
!     key  - array of real numbers used as keys for sorting
!
!     USES QUICKSORT ALGORITHM, EFFICIENT FOR 'N' VALUES GREATHER THAN
!     ABOUT 12 (ALTHOUGH MAY BE SYSTEM DEPENDENT)
!
!     ROUTINE SORTS LISTS UP TO LENGTH 2**MAXSTK
      implicit none
      integer, intent(inout) :: list(:)
      real(dp), intent(in) :: key(:)

      integer, parameter :: maxstk = 32
      integer :: ll, lr, lm, nl, nr, ltemp, stktop
      integer :: lstack(maxstk), rstack(maxstk)
      real(dp) :: guess
      logical :: done

      ll = 1
      lr = size(list, 1)
      stktop = 0
      done = .false.

      do while (.not. done)
         if (ll < lr) then
            nl = ll
            nr = lr
            lm = (ll + lr)/2
            guess = key(list(lm))

            ! Partition the array
            do
               do while (key(list(nl)) < guess)
                  nl = nl + 1
               end do

               do while (guess < key(list(nr)))
                  nr = nr - 1
               end do

               if (nl < (nr - 1)) then
                  ltemp = list(nl)
                  list(nl) = list(nr)
                  list(nr) = ltemp
                  nl = nl + 1
                  nr = nr - 1
               else
                  exit
               end if
            end do

            if (nl <= nr) then
               if (nl < nr) then
                  ltemp = list(nl)
                  list(nl) = list(nr)
                  list(nr) = ltemp
               end if
               nl = nl + 1
               nr = nr - 1
            end if

            ! Push the larger sub-list to the stack and continue with the smaller
            stktop = stktop + 1
            if (nr < lm) then
               lstack(stktop) = nl
               rstack(stktop) = lr
               lr = nr
            else
               lstack(stktop) = ll
               rstack(stktop) = nr
               ll = nl
            end if

         else if (stktop > 0) then
            ll = lstack(stktop)
            lr = rstack(stktop)
            stktop = stktop - 1
         else
            done = .true.
         end if
      end do

   end subroutine qsort_pure

end module hs_sort
