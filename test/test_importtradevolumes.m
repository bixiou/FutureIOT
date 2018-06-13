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
% test the importtradevolumes function
% function retval = test_importtradevolumes()
%
% Returns 1 if importtradevolumes.m performs as expected. Returns 0 
% if the result is not correct.
%
function retval = test_importtradevolumes()

  result = 0; 
  
  dim = 2;
  
  U = [5,8,0.5,0.5; ...
       6,3,1.5,0; ...
       0.5,1,2,8; ...
       1,0.5,9,9];
  
  Y = [5.5, 1.5; ...
       8, 1; ...
       1, 6; ...
       1, 7.5];
       
       
  [It_observed, Dt_observed] = importtradevolumes(U, Y, dim);
  
  It_expected = [0, 2.5;
                 3, 0];
  
  Dt_expected = [0, 2.5;
                 2, 0];

  if (arrayequals(It_expected, It_observed) == 1) 
    result = result + 1;
  endif
      
  if (arrayequals(Dt_expected, Dt_observed) == 1)
    result = result + 1;
  endif
                 
  if (result == 2) 
    printf('importtradevolumes.m\tokay\n');
    retval = 1;
  else
    printf('importtradevolumes.m\tnot okay\n');
    retval = 0;
  endif


endfunction