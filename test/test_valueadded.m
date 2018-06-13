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
% test the valueadded function
% function retval = test_valueadded()
%
% Returns 1 if valueadded.m performs as expected. Returns 0 
% if the result is not correct.
%
function retval = test_valueadded()

  dim = 2;
  
  W = [2.5, 4.5, 1, 4; ...
       5, 3.5, 4, 7];

  expected = [15.5;16];

  observed  = valueadded(W, dim);

  if (arrayequals(expected, observed) == 1) 
    printf('valueadded.m\tokay\n');
    retval = 1;
  else
    printf('valueadded.m\tnot okay\n');
    retval = 0;
  endif

endfunction