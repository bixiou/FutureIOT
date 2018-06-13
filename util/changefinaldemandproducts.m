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

% Calculate a final uses table where the total final use of certain products
% is scaled to a certain level while adjusting proportionally the final uses
% of the remaining products so that total final use (in a region) remains the
% same.
%
% Ynew = changefinaldemandproducts(Y, row_idx, fu_factors)
%
% Returns an altered final uses table where the final demand for the requested 
% rows is scaled according the fu_factors and all other rows are proportionally
% scaled so that the total final demand does not change. 
%
% Need in workspace: 
% Y          = final uses table at basic price = matrix 
% row_idx    = the index of the rows where the final use of the products has 
%              to be scaled with fu_factors without changing total final 
%              use = column vector
% fu_factors = the scaling factor for the final use of products = column vector
% dim        = number of regions
%

function Ynew = changefinaldemandproducts(Y, row_idx, fu_factors, dim)

  % check input
  [numRows numCols] = size(row_idx);
  if numCols ~= 1
    error('ERROR: row_idx not a column vector')
  endif
  
  [numRows numCols] = size(fu_factors);
  if numCols ~= 1
    error('ERROR: fu_factors not a column vector')
  endif 

  % initialisation
  Ytemp = Y(row_idx, :);  % store final product uses at row_idx

  % calculate orginal column sum final use (row vector)
  sum_finaluses_original = sum(Y,1);
  
  % adjust the requested rows with the given factor for each row
  for i=1:rows(Ytemp)
    Ytemp(i,:) = Ytemp(i,:) .* fu_factors(i);
  endfor
 
  % calculate sum values adjusted rows
  sum_Ytemp = sum(Ytemp, 1);
  
  % calculate the factor that is needed to scale all other 
  % final use of products except row_idx so that the column
  % sum of final use remains unaltered.
  factors = (sum_finaluses_original - sum_Ytemp) ./ (sum_finaluses_original - sum(Y(row_idx, :),1));
  
  % calculate new final demand  
  for i=1:columns(Y)
    Ynew(:,i) = Y(:,i) .* factors(i);
  endfor
  Ynew(row_idx,:) = Ytemp;
  
  % calculate new final use in each region 
  sumfinaluses_new = finaluses(Ynew, dim);
  
  % check if original final use in each region 
  % is the same as in the new final uses matrix
  % if not raise an error
  diff = abs(sum(sum_finaluses_original) - sum(sumfinaluses_new));
  if diff > 1E-3
    error('sum final use in region has changed while adjusting the final use of a product.');
  endif
  
endfunction