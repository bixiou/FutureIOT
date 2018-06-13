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
% Calculate GDP for a multiregional supply-use system according the income approach
% gdp = gdpincome(W, dim)
%
% returns the GDP according income approach for each region in the multiregional supply-use system
%
% Need in workspace: 
% W = multiregional factor inputs table at basic price = matrix
% dim = number of regions distinguished
%

function gdp = gdpincome(W, dim)

  i_count = columns(W)/dim;

  for i = 0:dim-1 
    idx0 = 1 + i * i_count;
    idx1 = (i + 1) * i_count; 
    gdp_income(i + 1,1) =  sum(sum(W(:,idx0:idx1)));
  endfor
  
  gdp = gdp_income;

endfunction
