 !  ==========================================================================  !
 !  Gabriel Martos                                                              !
 !  Copyright (C) 2018                                                          !
 !  --------------------------------------------------------------------------  !
 !  This program is free software; you can redistribute it and/or modify        !
 !  it under the terms of the GNU General Public License as published by        !
 !  the Free Software Foundation; either version 2 of the License, or           !
 !  (at your option) any later version.                                         !
 !                                                                              !
 !  This program is distributed in the hope that it will be useful,             !
 !  but WITHOUT ANY WARRANTY; without even the implied warranty of              !
 !  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the               !
 !  GNU General Public License for more details.                                !
 !  ==========================================================================  !


subroutine kernrbf_(t, l, n, sigma, K)
  implicit none
  
  integer :: i, j, ll  
  integer, intent(in) :: l, n
  double precision, intent(in), dimension(l) :: t
  double precision, intent(in) :: sigma
  double precision, intent(out), dimension(n) :: K
  
  ll = 1
    do i = 1, l, 1
      do j = 1, l, 1
            K(ll) = exp(sigma*((t(i)-t(j)))**2)
            ll = ll + 1   
     end do
    end do
end subroutine
