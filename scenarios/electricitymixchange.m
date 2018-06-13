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
% Change to electricity production (and consumption) mix to a predefined
% mix irrespective of existing mix. The total volume of electricty 
% production & consumption will not change and total imports and exports 
% of electricity will not change.
% 
% [Unew, Vnew, Wnew, Ynew, Mnew] = electricitymixchange(U, V, W, Y, M, mix, dim)
%
% Returns adapted intermediate use, make, factor inputs and, final use matrix that
% contain the adapted electricity mix.
%
% Need in workspace: 
% U = intermediate use table (product by industry) at basic price = matrix
% V = make table (industry by product) at basic price = matrix
% W = factor inputs table at basic price = matrix
% Y = final use table at basic price = matrix
% mix = contains for each region the requested mix (electricity products by region) = matrix
%       columns sum up to 1. 
% dim = number of regions in the multiregional supply-use system
%

function [Unew, Vnew, Wnew, Ynew, Mnew] = electricitymixchange(U, V, W, Y, M, mix, dim)

  % initialisation
  p_count = rows(U)/dim;
  i_count = columns(U)/dim;
  y_count = columns(Y)/dim;
  w_count = rows(W);
  elecIdx = [84;85;86;87;88;89;213;214;215;216;217;218;342;343;344;345;346;347;471;472;473;474;475;476];
  
  % store original value of intermediate use
  Uold = sum(sum(U));
  
	    
  % get the electricty use from the intermediate use and
  % final use table
  elecUse = [U(elecIdx,:), Y(elecIdx,:)];
  
  
  % calculate total electricity use
  totElecUsePerActivity = transpose(sum(elecUse, 1));
  totElecUsePerProduct = sum(elecUse, 2);
  
  
  % calculate new electricity mix
  for i=0:dim-1
    idx0 = 1 + (i * rows(mix));
    idx1 = rows(mix) + (i * rows(mix)); 
    tot = sum(totElecUsePerProduct(idx0:idx1,:));
    newtot = tot * mix(:, i +1);
    totElecUsePerProductNew(idx0:idx1,:) = newtot; 
  endfor
  
  
  % calculate new electricity inputs into every activity
  gras_set.maxiter = 1000;  
  gras_set.tol = 2;   % 1 = absolute tol criterium, 2 = relative tol. criterium, 3 = improvement tol. criterium 
  gras_set.criterium = 1E-5;
  elecUse = gras(elecUse, totElecUsePerProductNew, totElecUsePerActivity, gras_set);
  
  
  % split new electricity inputs into intermediate use and final use part.
  elecUseIntermediate = elecUse(:,1:dim*i_count);
  elecUseFinal = elecUse(:,dim*i_count + 1:dim*i_count + dim*y_count); 
 
  % push into intermediate use and final use table
  for i=1:rows(elecIdx)
  idx = elecIdx(i)
  U(idx,:) = elecUseIntermediate(i,:);
  Y(idx,:) = elecUseFinal(i,:);
  endfor
  
  % changed electricty supply mix must lead to changes in the use of
  % products, value added and emissions by the electricity production sectors
  sfactors = totElecUsePerProductNew ./ totElecUsePerProduct;
  for i = 1: rows(elecIdx)
    idx = elecIdx(i);
    U(:,idx) = U(:,idx) .* sfactors(i);
    W(:,idx) = W(:,idx) .* sfactors(i);
    M(:,idx) = M(:,idx) .* sfactors(i);
  endfor
  
    
  % new total product use to supply table
  q1 = tpou(U, Y);
  q2 = tpom(V);
  sfactors = q1 ./ q2;
  sfactors(isnan(sfactors))=1;    % take out NaN and replace by 1  
  for i=1:rows(q1)
    V(:,i) = V(:,i) .* sfactors(i);
  endfor
  
  
  % new total industry output carried over to use table
  g1 = tiou(U, W);
  g2 = tiom(V);
  sfactors = g2 ./ g1;
  sfactors(isnan(sfactors))=1;    % take out NaN and replace by 1 
  for i=1:rows(g1)
    U(:,i) = U(:,i) .* sfactors(i);
    W(:,i) = W(:,i) .* sfactors(i);
    M(:,i) = M(:,i) .* sfactors(i);
  endfor
  
  
  % Step 2 rebalance starts with supply table given total supply products and
  % total output industries which is 
  while (checkbalance(U, V, W, Y, dim) == 0) 
    [U, V, W, Y] = rebalance(U, V, W, Y, dim);
  endwhile

  
  % transfer to output
  Unew = U;
  Vnew = V;
  Wnew = W;
  Ynew = Y;
  Mnew = M;
  
	
endfunction  
