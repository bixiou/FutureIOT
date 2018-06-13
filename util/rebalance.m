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
% Rebalancing the supply use tables. After some of the scenario change operations the 
% supply-use tables are not perfectly balanced. This script will rebalance the supply-use 
% tables again. Balancing of the supply-use tables is done without changing the supply 
% table. The total product output and total industry output of the supply table are used 
% as starting point for the balancing of the intermediate use, value added and final use table.
% 
% [Unew, Vnew, Wnew, Ynew] = rebalance(U, V, W, Y, dim)
%
% Need in workspace: 
% U = intermediate use table (product by industry) at basic price = matrix
% V = make table (industry by product) at basic price = matrix
% W = factor inputs table at basic price = matrix
% Y = final use table at basic price = matrix
% dim = the number of regions in the multiregional supply-use system
% 

function [Unew, Vnew, Wnew, Ynew] = rebalance(U, V, W, Y, dim)

  % initialisation
  p_count = rows(U)/dim;
  i_count = columns(U)/dim;
  y_count = columns(Y)/dim;
  w_count = rows(W)/dim;


  if (checksanity(U, V, W, Y, dim) == 0)
    input('hit enter to continu after sanity check');
  endif
  
  % First step balance intermediate use + final use

    % total industry and product output calculated from supply table
    q2 = tpom(V);
    g2 = tiom(V);
  
  
    % calculate value added per activity
    va_activities = sum(W,1);

  
    % substract value added industries from total industry output (g2)
    iu_activities = transpose(g2) - va_activities;
  
  
    % if value iu_activities is very close to zero and the column sum 
    % of the intermediate use table is zero set the iu_activities to zero
    % apparently the valued added of the activity is about equal to the
    % supply of that industry. Make the Value added of that industry exactly 
    % the same as the request output of that industry
    colsums = sum(U,1);
    k = 0;
    colIdx = [0];
    va_factors = [0];
    for i = 1:columns(colsums)
      if (colsums(1, i) == 0 && iu_activities(1, i) != 0 )
         fprintf('Setting the requested total intermediate industry use %e of activity no %d to 0\n', iu_activities(1,i), i)
	 k = k + 1;
	 colIdx(k) = i;
         va_factors(k) = g2(i) / sum(W(:,i),1);
      endif
    endfor
    
    if (k != 0)
      W = changevalueaddedindustries(W, colIdx, va_factors, dim);   
      va_activities = sum(W,1);
      iu_activities = transpose(g2) - va_activities;
      iu_activities(1,colIdx) = 0;
    endif
    
    % scale final use per region to the value added in each region
    sumfinaluses = finaluses(Y, dim);
    sumvalueadded = valueadded(W, dim);
    factors = sumvalueadded ./sumfinaluses;
    for i=0:dim-1   
      idx0 = 1 + i * y_count;
      idx1 = (i + 1) * y_count; 
      Y(:,idx0:idx1) = Y(:,idx0:idx1) * factors(i+1,1);
    endfor
   
   
    % calculate sum final use per final activity
    fu_activities = sum(Y,1);
  
  
    % glue together intermediate use & final use matrix
    extended_use = [U, Y];
  
  
    % glue together sum intermediate use & sum final use 
    % and transform it into a column vector
    colsums = [iu_activities, fu_activities];
    colsums = transpose(colsums);                       
  
  
    % rowsums equal to total product output from make table
    rowsums = q2;

  
    % scale extended use to sum rowsum
    diff_factor = sum(sum(extended_use))/sum(rowsums)
    extended_use = extended_use ./ diff_factor;
    
  
    % balance extended use table
    gras_set.maxiter = 1000;  
    gras_set.tol = 3;   % 1 = absolute tol criterium, 2 = relative tol. criterium, 3 = improvement tol. criterium 
    gras_set.criterium = 0.001;
    extended_use = gras(extended_use, rowsums, colsums, gras_set);
  
  
    % extract separate intermediate use and final use table
    % from the extended_use table
    U =  extended_use(:,1:dim*i_count);
    Y = extended_use(:,dim*i_count + 1:dim*i_count + dim*y_count);


  % Second step balance intermediate use
  
    % substract final use products from total product output (q2)
    pu_intermediate = q2 - sum(Y,2);
 
    % if value pu_intermediate is very close to zero and the row sum 
    % of the intermediate use table is zero. set the pu_intermediate to zero
    % apparently the final use of the product is about equal to the
    % supply of that product. Set the final use of the product to total
    % supply of the product.
    rowsums = sum(U,2);
    k = 0;
    rowIdx = [0];
    fu_factors = [0];
    for i = 1:rows(rowsums)
      if (rowsums(i, 1) == 0 && pu_intermediate(i,1) != 0 )
        fprintf('Setting the requested sum intermediate use %e of product no %d to 0\n', pu_intermediate(1,i), i)
        k = k + 1;
        rowIdx(k) = i;
        fu_factors(k) = q2(i) / sum(Y(i,:),2);
      endif
    endfor  
    
    if (k != 0)  
      Y = changefinaldemandproducts(Y, rowIdx, fu_factors);            
      pu_intermediate = q2 - sum(Y,2);
      pu_intermediate(rowIdx,1) = 0;
    endif
    
  
    % scale value added per region to the final use in each region
    sumfinaluses = finaluses(Y, dim);
    sumvalueadded = valueadded(W, dim);
    factors = sumfinaluses ./ sumvalueadded
    for i=0:dim-1   
      idx0 = 1 + i * i_count;
      idx1 = (i + 1) * i_count;
      W(:,idx0:idx1) = W(:,idx0:idx1) * factors(i+1,1);
    endfor
  
  
    % calculate value added per intermediate industry activity
    va_activities = sum(W,1);

  
    % substract value added industries from total industry output (g2)
    iu_activities = transpose(g2) - va_activities;
   
   
   
  
    % if value iu_activities is very close to zero and the column sum 
    % of the intermediate use table is zero set the iu_activities to zero
    % apparently the valued added of the activity is about equal to the
    % supply of that industry. Set the value added exactly to the total
    % industry output.
    colsums = sum(U,1);
    k = 0;
    colIdx = [0];
    va_factors = [0];
    for i = 1:columns(colsums)
      if (colsums(1, i) == 0 && iu_activities(1, i) != 0 )
        fprintf('Setting the requested total intermediate activity %e of activity no %d to 0\n', iu_activities(1,i), i)
        k = k + 1;
        colIdx(k) = i;
        va_factors(k) = g2(i) / sum(W(:,i),1);  
      endif
    endfor
    
    if (k != 0)
      W = changevalueaddedindustries(W, colIdx, va_factors, dim); 
      va_activities = sum(W,1);
      iu_activities = transpose(g2) - va_activities;
      iu_activities(1,colIdx) = 0;
    endif
    
    % set rowsums of intermediate use to calculated intermediate product use 
    rowsums = pu_intermediate;
 
 
    % rowsums equal to total industry output from make table
    % and make it a columnvector
    colsums = transpose(iu_activities);

    
    % balance intermediate use table
    gras_set.maxiter = 1000;  
    gras_set.tol = 3;       
    gras_set.criterium = 0.0005;
    U = gras(U, rowsums, colsums, gras_set);

    
  % Third step print some essential information on the properties of the balanced system  
 
    % check the properties of the balanced system
    checkbalance(U, V, W, Y, dim);
    
    
  % Fourth step pass to output
  
    Unew = U;
    Vnew = V;
    Wnew = W;
    Ynew = Y;
    
endfunction