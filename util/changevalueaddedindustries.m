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
% Calculate a value added table where the total value added of certain 
% industries is scaled to a certain level while adjusting proportionally 
% the value added of the remaining industries so that total value added 
% (in each region) remains the same.
%
% Wnew = changevalueaddedindustries(W, column_idx, va_factors, dim)
%
% Returns an altered value added table where the value added for the requested 
% column is scaled according the va_factors and all other columns are 
% proportionally scaled so that the total value added (per region) does 
% not change. 
%
% Need in workspace: 
% W          = value added table at basic price = matrix 
% column_idx = the index of the columns where the value added of the industries
%	       has to be scaled with va_factors without changing total value 
%              added = row vector
% va_factors = the scaling factor for the value added of industries = row vector
% dim        = number of regions 
%
function Wnew = changevalueaddedindustries(W, column_idx, va_factors, dim)

  % check input
  [numRows numCols] = size(column_idx);
  if numRows ~= 1
    error('ERROR: column_idx not a row vector')
  endif
  
  [numRows numCols] = size(va_factors);
  if numRows ~= 1
    error('ERROR: va_factors not a row vector')
  endif  


  % initialisation
  Wnew = W;
  Wtemp = W(:,column_idx);      % store value added industry at column_idx
  i_count = columns(W) / dim;
  
  
  % adjust the requested column in the region block with the given factors
  for i=1:columns(Wtemp)
    Wtemp(:,i) = Wtemp(:,i) .* va_factors(i);
  endfor
  
  
  % find out in which region column_idx resides 
  for i=1:columns(column_idx)
    region_idx(i) = idivide(column_idx(i), i_count, "ceil"); 
  endfor
  
  
  % loop over the regions find out if there are one or more
  % columns that need to be scaled and do so
  for i=1:dim
    idx = (region_idx == i);
    
    if (sum(idx) > 0)   % one or more columns to be adjusted in this region
      
      column_idx_region = column_idx(idx); % column_idx_region contains the column idx for this region
      va_factors_region = va_factors(idx); % va_factors_region contains the factors in this region
      Wtemp_region = Wtemp(:,idx);         % value of the adjusted columns in this region
    
      % get the value added block for that region
      idx0 = 1 + (i_count * (i - 1));
      idx1 = i_count * i;
      local_column_idx = column_idx_region - (i_count * (i -1));
      Wregion = W(:,idx0:idx1);
    
      % calculate orginal row sum value added in this region (column vector)
      sumvalueadded_orginal = sum(Wregion, 2);
	
      % calculate row sum values of adjusted columns
      sum_Wtemp_region = sum(Wtemp_region, 2);
	
      % calculate the factor that is needed to scale all other 
      % value added in this region of industries except column_idx so that the row
      % sum of value added in the region remains unaltered.
      factors = ( sumvalueadded_orginal - sum_Wtemp_region) ./ (sumvalueadded_orginal - sum(W(:,column_idx_region),2));
	
      % calculate new value added in this region
      for i = 1:rows(Wregion)
	Wregion_new(i,:) = Wregion(i,:) .* factors(i);
      endfor
      Wregion_new(:,local_column_idx) = Wtemp_region;
	
      % add adapted value added block into complete value added matrix
      Wnew(:,idx0:idx1) = Wregion_new;
    endif
  endfor
  
  
  % calculate new final use in each region 
  sumva_original = sum(W, 1);
  sumva_new = sum(Wnew, 1);
  
  
  % check if original final use in each region 
  % is the same as in the new final uses matrix
  % if not raise an error
  diff = abs(sum( sumva_original) - sum( sumva_new));
  if diff > 1E-3
    error('sum value added has changed while adjusting the value added of an industry.');
  endif
  
  
endfunction