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
% Calculate the share of the main product in the total output of an industry
% using the make table
% shares = sharemainproduct(V)
%
% returns share of main product in the total output of each industry as column vector
%
% Need in workspace: 
% V = make table (industry by product) at basic price = matrix
%
function shares = sharemainproduct(V)

  g =  sum(V, 2);
  
  for i=1:rows(V)
    
    if (g(i) != 0)
      shares(i,1) = V(i,i) / g(i);
    else
      shares(i,1) = 0;
    endif
  
  endfor

endfunction