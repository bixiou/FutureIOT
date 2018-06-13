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
% Calculate total electricity use by private households.
% elec use = elecfinaluse(Y, dim)
%
% returns the total electricity use by private households either
% domestically produced or imported as column vector
%
% Need in workspace: 
% Y  = final demand table (products by final use categories) = matrix
%

function elecuse = elecfinaluse(Y, elecIds, dim)

  % initialisation
  elecuse = zeros(dim, 1);
  y_count = columns(Y)/dim;
  p_count = rows(Y)/dim;


  % loop over private household categories in each region
  for i=0:dim-1
    
    y_idx = 1 + (y_count * i);
    
    % loop over electricity sectors (domestic + foreign)
    for j=0:dim-1   % loop over all regions
      for k=1:rows(elecIds)
	
	p_idx = elecIds(k) + (p_count * j);
	
	elecuse(i+1,1) = elecuse(i+1, 1) +  Y(p_idx, y_idx);
  
      endfor
    endfor
  endfor
  
  
endfunction
