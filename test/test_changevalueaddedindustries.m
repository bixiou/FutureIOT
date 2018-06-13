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
% test the changevalueaddedindustries function
% function retval = test_changevalueaddedindustries()
%
% Returns 1 if changevalueaddedindustries.m performs as expected. Returns 0 
% if the result is not correct.
%
function retval = test_changevalueaddedindustries()
  
  dim = 2;
  
  W = [5.5, 8, 1, 1; ...
       1.5, 1, 6, 7.5];
       
  colIdx = [2, 4];
  
  fu = [1.2, 1.5];

  expected = [ 3.9, 9.6, 0.5, 1.5; ...
               1.3, 1.2, 2.25,  11.25];      
              
  observed  = changevalueaddedindustries(W, colIdx, fu, dim);

  if (arrayequals(expected, observed, 0.001) == 1) 
    printf('changevalueaddedindustries.m\tokay\n');
    retval = 1;
  else
    printf('changevalueaddedindustries.m\tnot okay\n');
    retval = 0;
  endif

endfunction