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
% Cecilia 2050
% 
% Preprocessing the supply use tables as delivered by the Exiobase system. By 
% default, these are not well balanced while we need to have balanced tables 
% to implement the scenarios. The  initial supply use tables are not balanced 
% because the rRoW region is inserted in the most simple way after the tradelinking
% procedure without further checks on balance.
%
% Balancing of the supply-use tables is done without changing the supply table. 
% The total product output and total industry output of the supply table are 
% used as starting point for the balancing of the intermediate use, factor 
% inputs and final use table.
% 
% The input are the multi-regional tables adresssed with the original name 
% and original structure coming from the Exiobase system. The output are 
% 7 tables that are written to text in tab delimited
% text format:
%   1. U.txt - intermediate use table
%   2. V.txt - make table
%   3. W.txt - value added table
%   4. Y.txt - final use table
%   5. M.txt - direct GHG emissions from industry
%   6. FM.txt - direct emissions from households
%   7  R.txt - fossil carbon resource extraction
%
% The SUTs are well balanced and can be used to implement the scenarios
%

% settings
clear all;
more off;
utildir = strcat(pwd(),'/../util/');
addpath(utildir);
dim = 4;  % number of regions

% read files
U = dlmread('mrUseAggregated.txt', '\t', 2, 3);
Vtranspose = dlmread('mrSupplyAggregated.txt', '\t', 2, 3);
V = transpose(Vtranspose);
W = dlmread('mrFactorInputsAggregated.txt', '\t', [2, 2, 10, 517]);   % only read value added part
Y = dlmread('mrFinalDemandAggregated.txt', '\t', 2, 3);
M = dlmread('mrEmissionsAggregated.txt', '\t',[ 2, 3, 4, 518]);       % only upper three rows contain GHGs
FM = dlmread('mrFDEmissionsAggregated.txt', '\t', [2, 3, 4, 30]);     % only upper three rows contain GHGs
R = dlmread('mrMaterialsAggregated.txt', '\t', [202, 2, 207, 517]);   % 6 rows contain fossil carbon extraction
mix = dlmread('mix.txt', '\t', 1, 2);  


checksanity(U,V,W,Y,dim);
input('hit enter to continu after sanity check');


% initialisation
p_count = rows(U)/dim;
i_count = columns(U)/dim;
y_count = columns(Y)/dim;
w_count = rows(W)/dim;


% some sanitation
U = removenegatives(U);
V = removenegatives(V);


% the tables exported from EXIOBASE are not well balanced.
% make a well balanced system before putting in the scenario assumptions
% value added matrix and make matrix remain unaltered


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
  factors = sumvalueadded ./sumfinaluses
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

  
  % balance intermediate table
  gras_set.maxiter = 1000;  
  gras_set.tol = 3;         % 1 = use absolute tolerance criterium, 2 = use relative tolerance criterium 
  gras_set.criterium = 0.0005;
  extended_use = gras(extended_use, rowsums, colsums, gras_set);
  
  
  % extract separate intermediate use and final use table
  % from the extended_use table
  U =  extended_use(:,1:dim*i_count);
  Y = extended_use(:,dim*i_count + 1:dim*i_count + dim*y_count);
 
  
  % substract final use products from total product output (q2)
  pu_intermediate = q2 - sum(Y,2);
 

  % if value pu_intermediate is very close to zero and the row sum 
  % of the intermediate use table is zero. set the pu_intermediate to zero
  % apparently the final use of the product is about equal to the
  % supply of that product. Subsequently set the final use of the product 
  % to the total supply of the product without altering the total final use
  % in a region
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
  % supply of that industry. 
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
  
  
  % glue together sum intermediate use & sum value added categories 
  rowsums = pu_intermediate;
 
 
  % rowsums equal to total industry output from make table
  % and make it a columnvector
  colsums = transpose(iu_activities);
  
 
  % balance intermediate use table
  gras_set.maxiter = 1000;  
  gras_set.tol = 3;       
  gras_set.criterium = 0.0005;
  U = gras(U, rowsums, colsums, gras_set);

  
  % Rebalance if necessary
  while (checkbalance(U, V, W, Y, dim) == 0) 
    [U, V, W, Y] = rebalance(U, V, W, Y, dim);
  endwhile
  
  
  checkcarbonbalance(M, FM, R)
  
  % check the properties of the balanced system
  gdp_income = gdpincome(W, dim)
  gdp_expenditure = gdpexpenditure(U, Y, dim)
  gdp_production = gdpproduction(U, Y, dim)

  gdp_income - gdp_expenditure
  gdp_income - gdp_production
  
  checkbalance(U, V, W, Y,dim);
  sumtpom(V,dim)
  sumtpou(U,Y,dim)
  sumtiom(V,dim)
  sumtiou(U,W,dim)
  sumfinaluses = finaluses(Y, dim)
  sumvalueadded = valueadded(W, dim)
  finaluses(Y, dim) - valueadded(W, dim)  
    

  % export the balance tables
  dlmwrite('U.txt', U, '\t');
  dlmwrite('V.txt', V, '\t');
  dlmwrite('W.txt', W, '\t');
  dlmwrite('Y.txt', Y, '\t');
  dlmwrite('M.txt', M, '\t');
  dlmwrite('FM.txt', FM, '\t');
  dlmwrite('R.txt', R, '\t');