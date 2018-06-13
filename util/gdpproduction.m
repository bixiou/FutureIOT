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
% Calculate GDP for a multiregional supply-use system according the production approach
% gdp = gdpproduction(U, Y, dim)
%
% returns the GDP according production approach for each region in the multiregional supply-use system
%
% Need in workspace: 
% U = multiregional intermediate use table (product by industry) at basic price = matrix
% Y = multiregional final use table at basic price = matrix
% dim = number of regions distinguished
%
function gdp = gdpproduction(U, Y, dim)

  p_count = rows(U)/dim;
  i_count = columns(U)/dim;
  y_count = columns(Y)/dim;

  for i = 0:dim-1
    y_idx0 = 1 + i * y_count;
    y_idx1 = (i + 1) * y_count;
    p_idx0 = 1 + i * p_count;
    p_idx1 = (i + 1) * p_count;
    i_idx0 = 1 + i * i_count;
    i_idx1 = (i + 1) * i_count;
    gdp_production(i + 1, 1) = sum(sum(U(p_idx0:p_idx1,:))) + ...
      sum(sum(Y(p_idx0:p_idx1,:))) - ...
      sum(sum(U(:,i_idx0:i_idx1)));   
  endfor
   
  gdp = gdp_production;
  
endfunction
