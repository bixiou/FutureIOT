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
% test total industry output from use table function
% function retval = test_tiou()
%
% Returns 1 if tiou.m performs as expected. Returns 0 
% if the result is not correct.
%
function retval = test_tiou()
  U = [5,8,0.5,0.5; ...
       6,3,1.5,0; ...
       0.5,1,2,8; ...
       1,0.5,9,9];
     
  W = [2.5,4.5,1,4; ...
       5,3.5,4,7];
     
  expected = [20;20.5;18;28.5];

  observed = tiou(U, W);
  
  if (arrayequals(expected, observed) == 1) 
    printf('tiou.m\tokay\n');
    retval = 1;
  else
    printf('tiou.m\tnot okay\n');
    retval = 0;
  endif

endfunction