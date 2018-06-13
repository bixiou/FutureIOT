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

% Checks if the supply - use system has negatives in the wrong places
% Intermediate use and supply table should only contain positive numbers
% total product output and total industry output should contain only positive numbers
%
% 
% retval= checksanity(U, V, W, Y)
%
% returns 1 if the system is sane and returns 0 if the system is not sane
%
% Need in workspace: 
% U = intermediate use table (product by industry) at basic price = matrix
% V = make table (industry by product) at basic price = matrix
% W = factor inputs table at basic price = matrix
% Y = final use table at basic price = matrix
%

function retval= checksanity(U, V, W, Y, dim)

  % initialisation
  retval = 1;
  p_count = rows(U);
  i_count = columns(U);
  y_count = columns(Y);
  chck_fu = 0;   % check for negatives in final use also. 0 = do not check, 1 = do check final use
  
  % total product and industry outputs
  q1 = tpou(U, Y);
  g1 = tiou(U, W);
  q2 = tpom(V);
  g2 = tiom(V);

  % negatives in intermediate use table
  if (min(min(U)) < 0)
     printf('intermediate use table contains negative values\n');
     retval= 0;
  endif

  % negatives in make table
  if (min(min(V)) < 0)
    printf('make table contains negative values\n');
    retval = 0;
  endif  
  
  % negative total product use
  if (min(q1) < 0)
    printf('total product use contains negative values\n');
    retval = 0;
  endif  
  
  % negative total industry use
  if (min(g1) < 0)
    printf('total industry use contains negative values\n');
    retval = 0;
  endif  
  
  % check for inconsistencies between requisted total product use and total product supply
  printf('\n');
  for i=1:p_count
    if (q1(i) == 0 && q2(i) != 0)
      printf('inconsistency at product %d: total product use = %f and total product supply = %f ', i, q1(i),  q2(i));
      retval = 0;
    endif
  
    if (q1(i) != 0 && q2(i) == 0)
      printf('inconsistency at product %d: total product use = %f and total product supply = %f ', i, q1(i),  q2(i));
      retval = 0;
    endif
  endfor
  
  % check for inconsistencies between requisted total industry use and total industry supply
  printf('\n');
  for i=1:i_count
    if (g1(i) == 0 && g2(i) != 0)
      printf('inconsistency at industry %d: requested total industry use = %f and total industry supply = %f ', i, g1(i),  g2(i));
      retval = 0;
    endif
  
    if (g1(i) != 0 && g2(i) == 0)
      printf('inconsistency at industry %d: requested total industry use = %f and total industry supply = %f ', i, g1(i),  g2(i));
      retval = 0;
    endif
  endfor
  
  
  if (chck_fu == 1) 

    % negative total final use
    totalfinaluse = sum(Y,2);
    for i=1:rows(Y)
      if (totalfinaluse(i) <0 )
	printf('total final use of product %d negative: total final use = %f\n', i, totalfinaluse(i));
	retval = 0;
      endif
    endfor

    % check for sum final use of product being negative in a region
    for i=1:rows(Y)
      for j=0:dim-1
      
	y_idx0 = 1 + j * y_count/dim;
	y_idx1 = (j + 1) * y_count/dim;
	
	if (sum(Y(i,y_idx0:y_idx1)) < 0)
	  printf('negative at sum final use of product %d in region %d\n', i, j);
	endif
      
      endfor
    endfor
  endif
  
  
endfunction