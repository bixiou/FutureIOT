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
% test the gdpchange scenario function
% function retval = test_gdpchange()
%
% Returns 1 if gdpchange.m performs as expected. Returns 0 
% if the result is not correct.
%

function retval = test_gdpchange()

  diff = 0.01;
  
  result = 0;
  
  h = [1.5; 1.8];
  
  dim = 2;

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

  M = [10, 10, 10, 10];
  
  FM = [4, 4];
  
  [Unew, Vnew, Wnew, Ynew, Mnew, FMnew]= gdpchange(U, V, W, Y, M, FM, h, dim);
  
  % check factor inputs
  expected = [3.75, 6.75, 1.8, 7.2; ...
              7.5, 5.25, 7.2, 12.6];             
  if (arrayequals(expected, Wnew, diff) == 1) 
    result = result + 1;
  endif
  
  % check final use
  expected = [7.9377, 2.2800; ...
              11.8025, 1.5538; ...
              1.7452,   11.0283; ...
              1.7645, 13.9379];
              
  if (arrayequals(expected, Ynew, diff) == 1) 
    result = result + 1;
  endif
  
  % check intermdediate use
  expected = [7.43018, 11.91199, 0.86517, 0.86159; ...
              8.98785, 4.50288, 2.61637, 0; ...
              0.77633, 1.55575, 3.61583, 14.40336; ...
              1.55565, 0.77937, 16.30263, 16.23505];
              
  if (arrayequals(expected, Unew, diff) == 1) 
    result = result + 1;
  endif
  
  % check supply
  expected = [20.14737, 9.85263, 0, 0; ...
              11.13928, 19.61072 , 0, 0; ...
              0, 0, 20.62271, 11.77729; ...
              0, 0, 12.50211, 38.79789];
          
  if (arrayequals(expected, Vnew, diff) == 1) 
    result = result + 1;
  endif
  
  if (result == 4) 
    printf('gdpchange.m\tokay\n');
    retval = 1;
  else
    printf('gdpchange.m\tnot okay\n');
    retval = 0;
  endif
  

endfunction