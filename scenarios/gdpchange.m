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
% Change the GDP in a multiregional supply-use system per region 
% without any further changes in the supply-use system. No structural changes, 
% no change in trade. GDP change is only implemented as change in volume of 
% domestic supply and domestic use.
% 
% [Unew, Vnew, Wnew, Ynew, Mnew, FMnew] = gdpchange(U, V, W, Y, M, FM, h, dim)
%
% Returns adapted intermediate use, make, factor inputs and final use matrix 
% which conform to the requested change in gdp in each region and are balanced.
%
% Need in workspace: 
% U = intermediate use table (product by industry) at basic price = matrix
% V = make table (industry by product) at basic price = matrix
% W = factor inputs table at basic price = matrix
% Y = final use table at basic price = matrix
% M = domestice industry emissions = matrix
% FM = final demand emissions = matrix
% h = factor for each region that gives the request change = column vector
% dim = the number of regions in the multiregional supply-use system
%

function [Unew, Vnew, Wnew, Ynew, Mnew, FMnew]= gdpchange(U, V, W, Y, M, FM, h, dim)

  % initialisation
  p_count = rows(U)/dim;
  i_count = columns(U)/dim;
  y_count = columns(Y)/dim;
  w_count = rows(W)/dim;
  init_interm_use = sum(U, 2);
  init_final_use = sum(Y, 2);
    
  % some checks
  checksanity(U, V, W, Y, dim);
  checkbalance(U, V, W, Y, dim);  
  
  
  % Step 1 apply gdpchange everywhere
  
    % apply to extensions
    for i = 0:dim-1
      i_idx0 = 1 + i * i_count;
      i_idx1 = (i + 1) * i_count;
      Wnew(:,i_idx0:i_idx1) =  h(i+1) * W(:,i_idx0:i_idx1); 
    endfor
    
    
    % apply to intermediate use - domestic and import use
    for i = 0:dim-1
      i_idx0 = 1 + i * i_count;
      i_idx1 = (i + 1) * i_count;        
      Unew(:,i_idx0:i_idx1) = h(i+1) * U(:,i_idx0:i_idx1);
    endfor
 
 
    % apply to final use - domestic final use & import final use
    interm_use = sum(Unew, 2);
    factors = interm_use ./ init_interm_use;
    factors(isnan(factors))=1;      % take out NaN and replace with 1 where product does not exist
    for i = 1:rows(Y)
      Ynew(i,:) = factors(i) .* Y(i,:);
    endfor
    sfactor =  sum(sum(Wnew)) / sum(sum(Ynew)) ; % make sure Wnew and Ynew sum up to same value
    Ynew = sfactor * Ynew;
    
    
   
  % Step 2 try to get the tables balanced again 
  
    % new total product and industry output from use
    q1 = tpou(Unew, Ynew);
    g1 = tiou(Unew, Wnew);
  
    % total volume make table equal to total volume industry output 
    sfactor = sum(g1) / sum(sum(V))
    Vnew = sfactor .* V;
  
    % balance  make table - rowsums are industry output
    gras_set.maxiter = 1000;  
    gras_set.tol = 3;         % 1 = use absolute tolerance criterium, 2 = use relative tolerance criterium 
    gras_set.criterium = 0.0001;
    Vnew = gras(Vnew, g1, q1, gras_set);
  
  
  % Step 3 rebalance if necessary
  while (checkbalance(Unew, Vnew, Wnew, Ynew, dim) == 0) 
    [Unew, Vnew, Wnew, Ynew] = rebalance(Unew, Vnew, Wnew, Ynew, dim);
  endwhile
  
  
  % Step 4 adjust emissions according increased industry use and final use
  
    % changed use of products (fuel, fertilizer etc) brings changes 
    % in emissions accordingly
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
  
  
  % some essential properties of balanced system
  gdp_income = gdpincome(Wnew, dim)
  gdp_expenditure = gdpexpenditure(Unew, Ynew, dim)
  gdp_production = gdpproduction(Unew, Ynew, dim)

  gdp_income - gdp_expenditure
  gdp_income - gdp_production
  gdp_expenditure - gdp_production 

  checkbalance(Unew, Vnew, Wnew, Ynew,dim);
  sumtpom(Vnew,dim)
  sumtpou(Unew,Ynew,dim)
  sumtiom(Vnew,dim)
  sumtiou(Unew,Wnew,dim)
  sumfinaluses = finaluses(Ynew, dim)
  sumvalueadded = valueadded(Wnew, dim)
  finaluses(Ynew, dim) - valueadded(Wnew, dim)  
  
endfunction