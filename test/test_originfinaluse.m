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
% test the originfinaluse function
% function retval = test_originfinaluse()
%
% Returns 1 if originfinaluse.m performs as expected. Returns 0 
% if the result is not correct.
%
function retval = test_originfinaluse()

  dim = 2;
  
  Y = [5.5, 1.5, 3, 2;   ...
       8, 1, 4, 6; ...
       1, 6, 5, 2; ...
       1, 7.5, 1, 2];

  expected = [0.1176470588
              0.8823529412];

  observed  = originfinaluse(Y, 2, 2, dim);

  if (arrayequals(expected, observed, 0.01) == 1) 
    printf('originfinaluse.m\tokay\n');
    retval = 1;
  else
    printf('originfinaluse.m\tnot okay\n');
    retval = 0;
  endif

endfunction