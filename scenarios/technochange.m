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
% Change the transactions according the technological change factors such
% that new balanced SUTs are created.
% 
% [Unew, Vnew, Wnew, Ynew, Mnew, FMnew] = technochange(U, V, W, Y, M, FM, TF, EM, dim)
%
% Returns adapted intermediate use, make, factor inputs and final use matrix 
% industry emissions and direct emission which conform to the requested 
% techno change in each sector and are balanced.
% 
% Need in workspace: 
% U = intermediate use table (product by industry) at basic price = matrix
% V = make table (industry by product) at basic price = matrix
% W = factor inputs table at basic price = matrix
% Y = final use table at basic price = matrix
% TF = technofactor table (product by industry) giving the requested changes = matrix
% EM = CO2 emission modifiers = column vector
% dim = number of regions in the multiregional supply-use system
%

function [Unew, Vnew, Wnew, Ynew, Mnew, FMnew ] = technochange(U, V, W, Y, M, FM, TF, EM, dim)

  % initialisation
  p_count = rows(U)/dim;
  i_count = columns(U)/dim;
  y_count = columns(Y)/dim;
  w_count = rows(W);
 
  
  % apply techno change factors to intermediate use table
  for j = 1:columns(U)
    for i = 1:rows(U)
      Unew(i,j) = U(i,j) * TF(i, j);
    endfor
  endfor 
 
 
  % calculate new total product use and carry over 
  % changed product use to supply table    
  q1 = tpou(Unew, Y);
  q2 = tpom(V);
  sfactors = q1 ./ q2;
  sfactors(isnan(sfactors))=1;    % take out NaN and replace by 1  
  for i=1:rows(q1)
    Vnew(:,i) = V(:,i) .* sfactors(i);
  endfor
 
 
  % new total industry output carried over to use table
  g1 = tiou(Unew, W);
  g2 = tiom(Vnew);
  sfactors = g2 ./ g1;
  sfactors(isnan(sfactors))=1;    % take out NaN and replace by 1 
  for i=1:rows(g1)
    Unew(:,i) = Unew(:,i) .* sfactors(i);
    Wnew(:,i) = W(:,i) .* sfactors(i);
    Mnew(:,i) = M(:,i) .* sfactors(i);
  endfor
 
        
      
	% scale final use per region to the value added in each region
	sumfinaluses = finaluses(Y, dim);
	sumvalueadded = valueadded(Wnew, dim);
	factors = sumvalueadded ./sumfinaluses;
	for i=0:dim-1   
	  idx0 = 1 + i * y_count;
	  idx1 = (i + 1) * y_count; 
	  Ynew(:,idx0:idx1) = Y(:,idx0:idx1) * factors(i+1,1);
	endfor
  
	% calculate preliminary total product use and total industry used
	q1 = tpou(Unew, Ynew);
	g1 = tiou(Unew, Wnew);
	
	% scale supply matrix to sum q1
	sfactor = sum(q1) /sum(sum(V));
	Vnew =  sfactor .* V;
	
	% balance make table
	gras_set.maxiter = 1000;  
	gras_set.tol = 3;       
	gras_set.criterium = 0.0005;
	Vnew = gras(Vnew, g1, q1, gras_set);  
	
    
    % Balance use table using total industry output and
    % product output from supply table
	
	% total industry and product output calculated from supply table
	q2 = tpom(Vnew);
	g2 = tiom(Vnew);
      
      
	% calculate value added per activity
	va_activities = sum(Wnew,1);

      
	% substract value added industries from total industry output (g2)
	iu_activities = transpose(g2) - va_activities;
      
      
	% if value iu_activities is very close to zero and the column sum 
	% of the intermediate use table is zero set the iu_activities to zero
	% apparently the valued added of the activity is about equal to the
	% supply of that industry. 
	colsums = sum(Unew,1);
	k = 0;
	colIdx = [0];
	va_factors = [0];
	for i = 1:columns(colsums)
	  if (colsums(1, i) == 0 && iu_activities(1, i) != 0 )
	    fprintf('Setting the requested total intermediate activity %e of activity no %d to 0\n', iu_activities(1,i), i)
	    k = k + 1;
	    colIdx(k) = i;
	    va_factors(k) = g2(i) / sum(Wnew(:,i),1);
	  endif
	endfor
	
	if (k != 0)
	  Wnew = changevalueaddedindustries(Wnew, colIdx, va_factors, dim);   
	  va_activities = sum(Wnew,1);
	  iu_activities = transpose(g2) - va_activities;
	  iu_activities(1,colIdx) = 0;
	endif
	
	
	% scale final use per region to the value added in each region
	sumfinaluses = finaluses(Ynew, dim);
	sumvalueadded = valueadded(Wnew, dim);
	factors = sumvalueadded ./sumfinaluses;
	for i=0:dim-1   
	  idx0 = 1 + i * y_count;
	  idx1 = (i + 1) * y_count; 
	  Ynew(:,idx0:idx1) = Ynew(:,idx0:idx1) * factors(i+1,1);
	endfor
      
      
	% calculate sum final use per final activity
	fu_activities = sum(Ynew,1);
      
      
	% glue together intermediate use & final use matrix
	extended_use = [Unew, Ynew];
      
      
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
	Unew =  extended_use(:,1:dim*i_count);
	Ynew = extended_use(:,dim*i_count + 1:dim*i_count + dim*y_count);


      % Second step balance intermediate use
      
	% substract final use products from total product output (q2)
	pu_intermediate = q2 - sum(Ynew,2);
    
	% if value pu_intermediate is very close to zero and the row sum 
	% of the intermediate use table is zero. set the pu_intermediate to zero
	% apparently the final use of the product is about equal to the
	% supply of that product. 
	rowsums = sum(Unew,2);
	k = 0;
	rowIdx = [0];
	fu_factors = [0];
	for i = 1:rows(rowsums)
	  if (rowsums(i, 1) == 0 && pu_intermediate(i,1) != 0 )
	    fprintf('Setting the requested sum intermediate use %e of product no %d to 0\n', pu_intermediate(1,i), i)
	    k = k + 1;
	    rowIdx(k) = i;
	    fu_factors(k) = q2(i) / sum(Ynew(i,:),2); 
	  endif
	endfor
	
	if (k != 0)  
	  Ynew = changefinaldemandproduct(Ynew, rowIdx, fu_factors);            
	  pu_intermediate = q2 - sum(Ynew,2);
	  pu_intermediate(rowIdx,1) = 0;
	endif
      
      
	% scale value added per region to the final use in each region
	sumfinaluses = finaluses(Ynew, dim);
	sumvalueadded = valueadded(Wnew, dim);
	factors = sumfinaluses ./ sumvalueadded
	for i=0:dim-1   
	  idx0 = 1 + i * i_count;
	  idx1 = (i + 1) * i_count;
	  Wnew(:,idx0:idx1) = Wnew(:,idx0:idx1) * factors(i+1,1);
	endfor
      
      
	% calculate value added per intermediate industry activity
	va_activities = sum(Wnew,1);

      
	% substract value added industries from total industry output (g2)
	iu_activities = transpose(g2) - va_activities;
      
      
	% if value iu_activities is very close to zero and the column sum 
	% of the intermediate use table is zero set the iu_activities to zero
	% apparently the valued added of the activity is about equal to the
	% supply of that industry. 
	colsums = sum(Unew,1);
	k = 0;
	colIdx = [0];
	va_factors = [0];
	for i = 1:columns(colsums)
	  if (colsums(1, i) == 0 && iu_activities(1, i) != 0 )
	    fprintf('Setting the requested total intermediate activity %e of activity no %d to 0\n', iu_activities(1,i), i)
	    k = k + 1;
	    colIdx(k) = i;
	    va_factors(k) = g2(i) / sum(Wnew(:,i),1);    
	  endif
	endfor
	
	if (k != 0)
	  Wnew = changevalueaddedindustries(Wnew, colIdx, va_factors, dim);   
	  va_activities = sum(Wnew,1);
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
	Unew = gras(Unew, rowsums, colsums, gras_set);
	
	
	if (checksanity(Unew, Vnew, Wnew, Ynew, dim) == 0 || checkbalance(Unew, Vnew, Wnew, Ynew, dim) == 0)
	  input('hit enter to continu rebalancing');
	endif
	
	% Rebalance if necessary
        while (checkbalance(Unew, Vnew, Wnew, Ynew, dim) == 0) 
	  [Unew, Vnew, Wnew, Ynew] = rebalance(Unew, Vnew, Wnew, Ynew, dim);
	endwhile
       
      % Third step change the GHG emissions
      
	% get Ids and carbon content of relevant products
	fossilIds = dlmread('fossilfuelproductgroups.txt', '\t', [2, 2, 51, 2]);
	fossilC = dlmread('fossilfuelproductgroups.txt', '\t', [2, 3, 51, 3]);
	
	
	% take original 2000 CO2 industry emissions
	% available in preprocess/M.txt in the first row of that matrix
	M_2000 = dlmread('../preprocess/M.txt', '\t', [0, 0, 0, 515]);
      
      
	% take original 2000 fossil fuel intermediate consumption
	% available in preprocess/U.txt at row id allFossilIds
	U_2000 = dlmread('../preprocess/U.txt', '\t');
	fossil_2000 = U_2000(fossilIds,:);

	
	% take 2050 fossil fuel intermediate consumption before 
	% technochange is implemented and write to file (for check)
	fossil_2050_2a = U(fossilIds,:);
	dlmwrite('fossil_2050_2a.txt', fossil_2050_2a, '\t');
	
	
	% take 2050 fossil fuel intermediate consumption after at 
	% techno change at row id allFossilIds
	fossil_2050 = Unew(fossilIds,:);
        dlmwrite('fossil_2050_2b.txt', fossil_2050, '\t');
      
      
	% using the fossilC coefficients translate fossil fuel use into
	% CO2 emissions
	for i = 1 : rows (fossilC)
	  carbondioxide_2000(i, :) = fossil_2000(i, :) * fossilC(i);
	  carbondioxide_2050(i, :) = fossil_2050(i, :) * fossilC(i);
	endfor
	sum_carbondioxide_2000 = sum(carbondioxide_2000, 1);
	sum_carbondioxide_2050 = sum(carbondioxide_2050, 1);
      
      
	% calculate the relative change of carbondioxide 2000 / 2050
	carbondioxide_factor = sum_carbondioxide_2050 ./ sum_carbondioxide_2000;
	carbondioxide_factor(isnan(carbondioxide_factor))= 1; 
    
      
	% use relative change to calculate 2050 CO2 
	% industry carbon dioxide emissions
	for i = 1 : columns(M)
	  M(1, i) = M_2000(1, i) * carbondioxide_factor(i);
	endfor      
	

	% Next change industry CO2 emissions according CCS implementation 
	% and bio carbon sequestration
	for i=1:columns(M)
	  Mnew(1,i) = M(1,i) * EM(i, 1);
	endfor
	
	
	
	% CH4 and N2O emissions scaled according change in intermediate product use
      
	% reduced use of products (fuel, fertilizer etc) brings reduced CH4 and N2O emissions accordingly
	ufactor = transpose(sum(Unew,1) ./ sum(U,1));
	ufactor(isnan(ufactor))=1;                 % take out NaN and replace by 1       
	for idx = 1:columns(M)
	  Mnew(2:3,idx) = Mnew(2:3,idx) .* ufactor(idx);
	endfor

	
	% changed emissions from private final demand sector changes
	% as a result of changes in consumption no specific technology
	% considerations
	FMnew = zeros(rows(FM), columns(FM));
	yfactor = transpose(sum(Ynew,1) ./ sum(Y,1));
	yfactor(isnan(yfactor))=1;                 % take out NaN and replace by 1 
	for i=0:dim-1
	  y_idx = (y_count * i) + 1;
	  FMnew(:,y_idx) = FM(:,y_idx) .* yfactor(y_idx);
	endfor   
	
	
endfunction  
