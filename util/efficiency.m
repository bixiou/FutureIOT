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
% Calculate the efficiency of the industry which is defined as total industry
% output divided by the total products used by that industry.
% eff = efficiency(U, V)
%
% returns the effiency of each industry as column vector. When an industry 
% does not exist 1 is returned
%
% Need in workspace: 
% U = multiregional intermediate use table (product by industry) at basic price = matrix
% V = multiregional make table at basic price = matrix
%

function eff = efficiency(U, V)

  % total industry output
  indoutput = sum(V, 2);
  
  % total product use by industry
  induse = transpose(sum(U, 1));
  
  % efficiency
  eff = indoutput ./ induse; 
  eff(isnan(eff))= 1;      % take out NaN and replace with 1 where industry does not exist
  
  
endfunction
