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
% Change the efficiency in a very generic manner in a multiregional supply-use 
% system per region. Efficiency change for a sector is interpreted 
% as making the same amount of products using less product inputs. Given that 
% total industry and product output may not change, the remaining money goes 
% to factor inputs. The extra income is spent again. Efficiency improvement 
% ultimately leads to more material wealth, GDP growth and trade changes.
% 
% [Unew, Vnew, Wnew, Ynew, Mnew, FMnew, Rnew] = efficiencychange(U, V, W, Y, M, FM, R, h, dim)
%
% Returns adapted intermediate use, make, factor inputs, final use matrix,
% industry emissions, direct emissions which conform to the requested efficiency 
% change in each sector and are balanced.
%
% Need in workspace: 
% U = intermediate use table (product by industry) at basic price = matrix
% V = make table (industry by product) at basic price = matrix
% W = factor inputs table at basic price = matrix
% Y = final use table at basic price = matrix
% M = industry emissions table (emissions by industry) = matrix
% FM = direct emissions table (emissions by final use category) = matrix
% R = carbon resource extraction table (extractions by industry) = matrix
% h = factor for each  industry that gives the request change = column vector
% dim = number of regions in the multiregional supply-use system
%

function [Unew, Vnew, Wnew, Ynew, Mnew, FMnew, Rnew] = efficiencychange(U, V, W, Y, M, FM, R, h, dim)
  
  % initialisation
  p_count = rows(U)/dim;
  i_count = columns(U)/dim;
  y_count = columns(Y)/dim;
  w_count = rows(W);
  
   
  % apply efficiency change to intermediate use table to each sector
  for j = 1:columns(U)
    for i = 1:rows(U)
      Unew(i,j) = U(i,j) / h(j);
    endfor
  endfor 

  
  % absolute change in total intermediate industry use moved to factor inputs
  % total industry use remains the same
  delta_abs = sum(U,1) - sum(Unew,1);
  delta_rel = (sum(W,1) + delta_abs)./sum(W,1);
  delta_rel(isnan(delta_rel))=1;                % take out NaN and replace with 1 where industry sector does not exist
  for j = 1:columns(W)
    for i = 1:rows(W)
      Wnew(i,j) = W(i,j) * delta_rel(j);
    endfor
  endfor
  
  % absolute change in total intermediate industry use moved to final uses
  % total product use remains the same (but final consumption is increased)
  delta_abs = sum(U,2) - sum(Unew,2);
  delta_rel = (sum(Y,2) + delta_abs)./sum(Y,2);
  delta_rel(isnan(delta_rel))= 1;               % take out NaN and replace with 1 where product does not exist
  for j = 1:columns(Y)
    for i = 1:rows(Y)
      Ynew(i,j) = Y(i,j) * delta_rel(i);
    endfor
  endfor
  
  % total industry output and total product output don't change
  % make table remains unaltered
  Vnew = V;
  
  % rebalance the system. 
  
      % First step balance the value added + intermediate use system
      
	% total industry and product output calculated from supply table
	q2 = tpom(Vnew);
	g2 = tiom(Vnew);
    
	% substract final use products from total product output (q2)
	% which gives the rowsums of the intermediate use table
	pu_intermediate = q2 - sum(Ynew,2);

      
	% change value added per region such that sum value added  
	% per region matches sum final demand per region
	% scale value added per region to the final use in each region
	sumfinaluses = finaluses(Ynew, dim);
	sumvalueadded = valueadded(Wnew, dim);
	fprintf('factors to scale value added per region to reflect final use per region:\n');
	factors = sumfinaluses ./ sumvalueadded
	for i=0:dim-1   
	  idx0 = 1 + i * i_count;
	  idx1 = (i + 1) * i_count;
	  Wnew(:,idx0:idx1) = Wnew(:,idx0:idx1) * factors(i+1,1);
	endfor
      
	% calculate sum value added per value added category
	va_categories = sum(Wnew,2);
      
	% glue together intermediate use & value added matrix
	extended_use = [Unew;  Wnew];
    
	% glue together row sum intermediate use & row sum value added 
	rowsums = [pu_intermediate;  va_categories];
  
	% colsums equal to total industry output from make table
	% and make it a columnvector
	colsums = g2;
    
	% scale extended use to sum rowsum
	fprintf('factor to scale extended intermediate use to requested column sums, should be close or equal to 1:\n');
	factor = sum(sum(extended_use))/sum(colsums)
	extended_use = extended_use ./ factor;
    
	% balance intermediate use table
	gras_set.maxiter = 1000;  
	gras_set.tol = 3;       
	gras_set.criterium = 0.0005;
	extended_use = gras(extended_use, rowsums, colsums, gras_set);

	% extract separate intermediate use and value added table
	% from the extended_use table
	Unew = extended_use(1:dim*p_count,:);
	Wnew = extended_use(dim*p_count+1:dim*p_count+ w_count,:);
      
     
      % Second step balance the intermediate use system
      
	% substract value added industries from total industry use (g2)   
	iu_activities = transpose(g2) - sum(Wnew,1);
  
	% if value iu_activities is very close to zero and the column sum 
	% of the intermediate use table is zero. set the iu_activities to zero
	% apparently the value added of the industry is about equal to the
	% supply by that industry.  Make the Value added of that industry exactly 
	% the same as the request output of that industry
	colsums = sum(Unew,1);
	k = 0;
	colIdx = [0];
	va_factors = [0];
	for i = 1:columns(colsums)
	  if (colsums(1, i) == 0 && iu_activities(1,i) != 0 )
	    fprintf('Setting the requested sum intermediate use %e of industry no %d to 0\n', iu_activities(1,i), i)
	    k = k + 1;
	    colIdx(k) = i;
	    va_factors(k) = g2(i) / sum(Wnew(:,i),1);
	  endif
	endfor
	
	if (k != 0)
	  Wnew = changevalueaddedindustries(Wnew, colIdx, va_factors, dim);
	endif
	
	
	% scale final use in each region to value added in each region
	% prevent final use of a product becoming so large that it is 
	% larger than total supply which means that we would end up with
	% negative elements in the intermediate use table. 
	sumfinaluses = finaluses(Ynew, dim);
	sumvalueadded = valueadded(Wnew, dim);
	fprintf('factors to scale final use per region to reflect value added per region:\n');
	factors = sumvalueadded ./ sumfinaluses
	
	  % first step apply factors to final uses
	  for i=0:dim-1   
	    idx0 = 1 + i * y_count;
	    idx1 = (i + 1) * y_count;
	    Ynew(:,idx0:idx1) = Ynew(:,idx0:idx1) * factors(i+1,1);
	  endfor
	  
	  % create column vector with position where final use is larger than total supply
	  % 0 means there is a positive, 1 means a negative 
	  exclude = any((q2-sum(Ynew,2))<0,2); 
	  
	  % At the position where the negatives occur scale up total product output so that it matches
	  % final use. This value is used to adapt the supply table. The
	  % result is that there is no efficiency increase in the use of these products. 
	  for i=1:dim*p_count
	    if (exclude(i) == 1)
	      q2(i) = sum(Ynew(i:i,:),2) + sum(Unew(i:i,:),2);
	    endif
	  endfor
	  
	  % third step check for negatives
	  if (min(q2 - sum(Ynew, 2)) < 0)
	    error('scaling of final uses makes final use of one or more products larger than total supply of product'); 
	  endif
 
	% calculate final use per product
	fu_products = sum(Ynew,2);
	
	% substract final use products from total product output (q2)
	iu_products = q2 - fu_products;
	
    
	% if value iu_products is very close to zero and the row sum 
	% of the intermediate use table is zero, set the iu_products to zero
	% apparently the final use of the product is about equal to the
	% supply of that product. Set the final use of the product to total
        % supply of the product without changing total final use of products
        % recalculate iu_products
	rowsums = sum(Unew,2);
	k = 0;
	rowIdx = [0];
	fu_factors = [0];
	for i = 1:rows(rowsums)
	  if (rowsums(i, 1) == 0 && iu_products(i, 1) != 0 )
	    fprintf('Setting the requested total intermediate product use %e of product no %d to 0\n', iu_products(i,1), i)
	    k = k + 1;
	    rowIdx(k) = i;
	    fu_factors(k) = q2(i) / sum(Ynew(i,:),2);
	  endif
	endfor  
	
	if (k != 0)
	  Ynew = changefinaldemandproducts(Ynew, rowIdx, fu_factors);
	  fu_products = sum(Ynew,2);
	  iu_products = q2 - fu_products;
	  iu_products(rowIdx, 1) = 0;
	endif    
	
	% because iu_products might have been changed in previous step scale intermediate use to sum rowsum
	% sum q2 is a little bit   
	fprintf('factor to scale intermediate use to requested row sums, should be close or equal to 1:\n');
	factor = sum(rowsums) / sum(iu_products)
	iu_products = iu_products .* factor;
	
	
	% set rowsums of intermediate use to calculated intermediate product use 
	rowsums = iu_products;
  
  
	% colsums equal to intermediate industry use and make it a column vector
	colsums = transpose(sum(Unew,1));
    
	% balance intermediate use table
	% balance supply table
	gras_set.maxiter = 1000;  
	gras_set.tol = 3;       
	gras_set.criterium = 0.0005;
	Unew = gras(Unew, rowsums, colsums, gras_set);  

    
      % Third step balance the make table
      
	colsums = tpou(Unew, Ynew); 
	rowsums = tiou(Unew, Wnew);
	gras_set.maxiter = 1000;  
	gras_set.tol = 3;       
	gras_set.criterium = 0.0005;
	Vnew = gras(Vnew, rowsums, colsums, gras_set);

	
      % Rebalance if necessary
      while (checkbalance(Unew, Vnew, Wnew, Ynew, dim) == 0) 
	[Unew, Vnew, Wnew, Ynew] = rebalance(Unew, Vnew, Wnew, Ynew, dim);
      endwhile
      
      
      % reduced use of products (fuel, fertilizer etc) brings reduced emissions accordingly
      ufactor = transpose(sum(Unew,1) ./ sum(U,1));
      ufactor(isnan(ufactor))=1;                 % take out NaN and replace by 1       
      for idx = 1:columns(M)
	Mnew(:,idx) = M(:,idx) .* ufactor(idx);
      endfor

      
      
      % increased emissions from private final demand sector
      % as a result of increased consumption
      FMnew = zeros(rows(FM), columns(FM));
      yfactor = transpose(sum(Ynew,1) ./ sum(Y,1));
      yfactor(isnan(yfactor))=1;                 % take out NaN and replace by 1 
      for i=0:dim-1
	y_idx = (y_count * i) + 1;
	FMnew(:,y_idx) = FM(:,y_idx) .* yfactor(y_idx);
      endfor   
      
	  
      % changed total output of sectors (if at all) leads to changed extraction
      g2 = tiom(V);
      g2new = tiom(Vnew);
      vfactor = g2new ./ g2;
      vfactor(isnan(vfactor))=1;      	    % take out NaN and replace by 1 
      for i = 1:rows(vfactor)
	Rnew(:,i) = R(:,i) .* vfactor(i);
      endfor
      
	
endfunction  
