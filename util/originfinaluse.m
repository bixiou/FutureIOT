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
% Calculate what the origin is of a product as demanded by a certain
% final demand category
% fractions = originfinaluse(Y, y_idx, p_idx, dim)
%
% returns a column vector length dim that contains which fraction
% of a product is provided by which region for a certain final demand category
%
% Need in workspace: 
% Y  = final demand table (products by final use categories) = matrix
% y_idx = the index (base 1) of the final demand category
% p_idx = the index (base 1) of the product category
% dim = the number of regions in the system
%

function fractions = originfinaluse(Y, y_idx, p_idx, dim)

  % initialisation
  y_count = columns(Y)/dim;
  p_count = rows(Y)/dim;

  fractions = zeros(dim, 1);
  
  sumvalue = 0;
  
  for i=0:dim-1 
    row_idx = p_idx + (i * p_count);
    sumvalue =  sumvalue + Y(row_idx, y_idx);
    fractions(i+1,1) =  Y(row_idx, y_idx);
  endfor
  
 
  for i=1:rows(fractions)
    if (sumvalue != 0.0)
      fractions(i) = fractions(i) / sumvalue;
    else 
      fractions(i) = 0.0;
    endif
  endfor
  
endfunction
