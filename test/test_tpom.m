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
% test total product output from make table function
% function retval = test_tpom()
%
% Returns 1 if tpom.m performs as expected. Returns 0 
% if the result is not correct.
%
function retval = test_tpom()

  % make table industry by product
  V  = [13.5, 6.5, 0, 0; ...
        7.5, 13, 0, 0; ...
        0, 0, 11.5, 6.5; ...
        0, 0, 7, 21.5];

  expected = [21;19.5;18.5;28];

  observed = tpom(V);
  
  if (arrayequals(expected, observed) == 1) 
    printf('tpom.m\tokay\n');
    retval = 1;
  else
    printf('tpom.m\tnot okay\n');
    retval = 0;
  endif

endfunction