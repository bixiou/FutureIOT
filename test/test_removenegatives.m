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
% test the removenegatives function
% function retval = test_removenegatives()
%
% Returns 1 if removenegatives.m performs as expected. Returns 0 
% if the result is not correct.
%
function retval = test_removenegatives()

  A = [-5, 1.5; ...
       8, 1; ...
       1, -1E-10; ...
       1, 7.5];

  expected = [0, 1.5; ...
       8, 1; ...
       1, 0; ...
       1, 7.5];

  observed  = removenegatives(A);

  if (arrayequals(expected, observed) == 1) 
    printf('removenegatives.m\tokay\n');
    retval = 1;
  else
    printf('removenegatives.m\tnot okay\n');
    retval = 0;
  endif

endfunction