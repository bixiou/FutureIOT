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
% test the changefinaldemandproducts function
% function retval = test_changefinaldemandproducts()
%
% Returns 1 if changefinaldemandproducts.m performs as expected. Returns 0 
% if the result is not correct.
%
function retval = test_changefinaldemandproducts()
  
  dim = 2;
  
  Y = [5.5, 1.5; ...
       8, 1; ...
       1, 6; ...
       1, 7.5];
       
  rowIdx = [2; 4];
  
  fu = [1.2; 1.5];

  expected = [3.72308, 0.71; ...
              9.6, 1.2; ...
              0.67692, 2.84; ...
              1.5, 11.25];

  observed  = changefinaldemandproducts(Y, rowIdx, fu, dim);

  if (arrayequals(expected, observed, 0.001) == 1) 
    printf('changefinaldemandproducts.m\tokay\n');
    retval = 1;
  else
    printf('changefinaldemandproducts.m\tnot okay\n');
    retval = 0;
  endif

endfunction