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
% test the sumtiou function 
% function retval = test_sumtiou()
%
% Returns 1 if sumtiou.m performs as expected. Returns 0 
% if the result is not correct.
%
function retval = test_sumtiou()

  dim = 2;
  
  U = [13.5, 6.5, 2, 1; ...
       7.5, 13, 0.5, 2; ...
       4.5, 2, 11.5, 6.5; ...
       1, 0.8, 7, 21.5];
       
  W = [9, 2, 10, 2];

  expected = [59.8; ...
              64];

  observed = sumtiou(U, W, dim);

  if (arrayequals(expected, observed) == 1) 
    printf('sumtiou.m\tokay\n');
    retval = 1;
  else
    printf('sumtiou.m\tnot okay\n');
    retval = 0;
  endif

endfunction