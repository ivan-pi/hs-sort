module test_suite1
   use testdrive, only: new_unittest, unittest_type, error_type, check, test_failed
   use hs_sort
   use, intrinsic :: iso_fortran_env, only: error_unit
   implicit none
   private

   public :: collect_suite1

contains

!> Collect all exported unit tests
   subroutine collect_suite1(testsuite)
      !> Collection of tests
      type(unittest_type), allocatable, intent(out) :: testsuite(:)
      integer, parameter :: ntests = 4

      allocate (testsuite(ntests))
      testsuite(1) = new_unittest("isort", test_isort)
      testsuite(2) = new_unittest("qsort", test_qsort)
      testsuite(3) = new_unittest("hsort", test_hsort)
      testsuite(4) = new_unittest("unsorted", test_unsorted_list)

   end subroutine collect_suite1

   subroutine test_isort(error)
      type(error_type), allocatable, intent(out) :: error
      real(dp), allocatable :: key(:)
      integer, allocatable :: indices(:)
      integer, parameter :: length = 10
      integer, parameter :: expected(10) = [7, 9, 3, 4, 2, 10, 1, 5, 8, 6]
      integer :: i
      allocate (key(length), indices(length))
      key = [17.0_dp, 8.0_dp, 1.0_dp, 3.0_dp, 42.0_dp, 420.0_dp, -17.0_dp, 80.0_dp, 0.0_dp, 10.0_dp]
      indices = [(i, i=1, length)]

      call isort(indices, key)

      call check(error, all(indices == expected), .true., "Sorting failed!")
      if (allocated(error)) return

   end subroutine test_isort

   subroutine test_qsort(error)
      type(error_type), allocatable, intent(out) :: error
      real(dp), allocatable :: key(:)
      integer, allocatable :: indices(:)
      integer, parameter :: length = 10
      integer, parameter :: expected(10) = [7, 9, 3, 4, 2, 10, 1, 5, 8, 6]
      integer :: i
      allocate (key(length), indices(length))
      key = [17.0_dp, 8.0_dp, 1.0_dp, 3.0_dp, 42.0_dp, 420.0_dp, -17.0_dp, 80.0_dp, 0.0_dp, 10.0_dp]
      indices = [(i, i=1, length)]

      call qsort(indices, key)

      call check(error, all(indices == expected), .true., "Sorting failed!")
      if (allocated(error)) return

   end subroutine test_qsort

   subroutine test_hsort(error)
      type(error_type), allocatable, intent(out) :: error
      real(dp), allocatable :: key(:)
      integer, allocatable :: indices(:)
      integer, parameter :: length = 10
      integer, parameter :: expected(10) = [7, 9, 3, 4, 2, 10, 1, 5, 8, 6]
      integer :: i
      allocate (key(length), indices(length))
      key = [17.0_dp, 8.0_dp, 1.0_dp, 3.0_dp, 42.0_dp, 420.0_dp, -17.0_dp, 80.0_dp, 0.0_dp, 10.0_dp]
      indices = [(i, i=1, length)]

      call hsort(indices, key)

      call check(error, all(indices == expected), .true., "Sorting failed!")
      if (allocated(error)) return

   end subroutine test_hsort

   subroutine test_unsorted_list(error)
      type(error_type), allocatable, intent(out) :: error
      integer, allocatable :: indices(:)
      integer, parameter :: length = 10
      integer :: i
      allocate (indices(length))
      indices = [(i, i=length, 1, -1)]

      call check(error, is_sorted(indices), .false.)
      if (allocated(error)) return

      call check(error, is_sorted(indices, ASCENDING), .false.)
      if (allocated(error)) return

      call check(error, is_sorted(indices, DESCENDING), .true.)
      if (allocated(error)) return

      indices = [(i, i=1, length)]
      call check(error, is_sorted(indices), .true.)

      if (allocated(error)) return
      call check(error, is_sorted(indices, ASCENDING), .true.)
      if (allocated(error)) return

      call check(error, is_sorted(indices, DESCENDING), .false.)
      if (allocated(error)) return

   end subroutine test_unsorted_list

end module test_suite1
