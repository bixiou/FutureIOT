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

function mix = elecProdMix(V, elecIdx, dim)

  % calculate electricity production mix in each region
  sum_product_output = sum(V, 1); 
  for i=1:dim
    idx = elecIdx(i);
    elec_prod(:,i) = transpose(sum_product_output(idx));
    tot_elec_prod(i) = sum(sum_product_output(idx));
  endfor
  
  
  for i=1:dim
    mix(:,i) = elec_prod(:,i) ./ tot_elec_prod(i); 
  endfor


endfunction