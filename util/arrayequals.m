% 
% CECILIA2050 - Scenarios for 2050 for a 2-degrees world
% Copyright (c) 2014 Instituute of Environmental Sciences (CML) Universiteit Leiden
%  
% This program is free software; you can redistribute it and/or modify it under
% the terms of the GNU General Public License as published by the Free Software
% Foundation; either version 2 of the License, or (at your option) any later
% version.
%  
% This program is distributed in the hope that it will be useful, but WITHOUT
% ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS
% FOR A PARTICULAR PURPOSE. See the GNU General Public License for more
% details.
%  
% You should have received a copy of the GNU General Public License along with
% this program; if not, write to the Free Software Foundation, Inc., 51
% Franklin Street, Fifth Floor, Boston, MA 02110-1301, USA.
% 

%
% Check if two matrices with floating point values 
% contain the same numbers. Takes into account that 
% floating point values have limited number of digits
% 
% retval = arrayequals(a, b)
% retval = arrayequals(a, b, delta)
%
% Returns 1 if the matrices are equal. Returns 0 if the dimensions 
% and or values are not the same. 
%
% Need in workspace: 
% a = matrix
% b = matrix to be compared with a.
% delta = optional; maximum relative deviation between two numbers as percentage. 
%         If not given the maximum deviation allowed between two numbers is equal to 
%         the precision of the value from b
%
function retval = arrayequals(a, b, delta) 

  switch nargin
    case 2
      default = 1;
    case 3
      default = 0;
    otherwise
      error('number of arguments not correct');
  endswitch
  
  retval = 1; 

  if(rows(a) != rows(b))
    disp('row dimensions do not match');
    retval = 0;
  endif

  if(columns(a) != columns(b))
    disp('column dimensions do not match');
    retval = 0;
  endif
 
  % compare floating point values default manner
  if (default == 1 && retval == 1) 
    for i=1:rows(a)
      for j=1:columns(a)
        tolfloor = eps(max(abs(b(i,j)),abs(a(i,j))));
        if (abs(a(i,j)-b(i,j)) > tolfloor) 
          % printf('values do not match: %g and %g \n', a(i,j) , b(i,j));
          retval = 0;
        endif
      endfor
    endfor
  endif
  
  % compare floating point values using user supplied maximum relative deviation
  if (default == 0 && retval == 1) 
    for i=1:rows(a)
      for j=1:columns(a)
        tolfloor = eps(max(abs(b(i,j)),abs(a(i,j))));
        value = delta/100 * max(abs(a(i,j)),abs(b(i,j))) + tolfloor;
        if (abs(a(i,j)-b(i,j)) > value) 
            % printf('values do not match: %g and %g \n', a(i,j) , b(i,j));
            retval = 0;  
        endif
      endfor
    endfor
  endif


  endfunction