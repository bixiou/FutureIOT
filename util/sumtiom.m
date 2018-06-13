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
% Calculate sum total industry output per region from make table
% sum_g = sumtiom(V)
%
% returns sum total industry output per region as column vector with length region count
%
% Need in workspace: 
% V = make table (industry by product) at basic price = matrix
% dim = number of regions distinguished
%
function sum_g = sumtiom(V, dim)

  g =  tiom(V);
  
  i_count = rows(V)/dim;
  
  for i = 0:dim-1 
    idx0 = 1 + i * i_count;
    idx1 = (i + 1) * i_count; 
    
    sum_g(i+1,1) = sum(g(idx0:idx1,1));
    
  endfor  


endfunction