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
% test the checkbalance function
% function retval = test_checkbalance()
%
% Returns 1 if checkbalance.m performs as expected. Returns 0 
% if the result is not correct.
%
function retval = test_checkbalance()
  result = 0;
  
  dim = 1;
  
  U = [5,8,0.5,0.5; ...
       6,3,1.5,0; ...
       0.5,1,2,8; ...
       1,0.5,9,9];
  
  V  = [13.5, 6.5, 0, 0; ...
        7.5, 13, 0, 0; ...
        0, 0, 11.5, 6.5; ...
        0, 0, 7, 21.5];
  
  W = [2.5,4.5,1,4; ...
       5,3.5,4,7];
       
  Y = [5.5, 1.5; ...
       8, 1; ...
       1, 6; ...
       1, 7.5];
  
  % feed well balanced system:
  expected = 1;
  observed = checkbalance(U, V, W, Y, dim);
  if (expected == observed)
    result = result + 1;
  endif
  
  % feed unbalanced system 
  W(1,1) = 10;
  expected = 0;
  observed = checkbalance(U, V, W, Y, dim);
  if (expected == observed)
    result = result + 1;
  endif
  
  if (result == 2) 
    printf('checkbalance.m\tokay\n');
    retval = 1;
  else
    printf('checkbalance.m\tnot okay\n');
    retval = 0;
  endif

endfunction