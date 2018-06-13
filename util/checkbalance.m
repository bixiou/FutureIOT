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
% Checks if a (multi-regional) supply - use system is balanced. The total
% product and industry output is checked, the total income and total 
% expenditure of each region should be the same, and GDP_income,
% GDP_expenditure and GDP_production in each regions should be the
% same.
%
% retval= checkbalance(U, V, W, Y, dim)
%
% returns 1 if the system is balanced and returns 0 if the system is not balanced.
% The precision of the balance can be adjusted with the criterium variable.
% Default maximum relative deviation between two numbers is set at 0.1%.
%
% Need in workspace: 
% U = intermediate use table (product by industry) at basic price = matrix
% V = make table (industry by product) at basic price = matrix
% W = factor inputs table at basic price = matrix
% Y = final use table at basic price = matrix
% dim = number of regions in multi-regional supply use tables
%

function retval= checkbalance(U, V, W, Y, dim)

  criterium = 0.1;
  retval= 1;

  if (rows(U) != rows(Y))
    error('number of products in intermediate use table does not match number of products in final use table');
    retval= 0;
  endif
  
  if(columns(U) != columns(W)) 
    error('number of industries intermediate use table does not match number of industries in factor inputs table'); 
    retval= 0;
  endif
  
  if(rows(U) != columns(V))
    error('number of products in intermediate use table does not match number of products in make table');
    retval= 0;
  endif
  
  if(columns(U) != rows(V))
    error('number of industries in intermediate use table does not match number of industries in make table');
    retval= 0;
  endif


  % total product and industry outputs
  q1 = tpou(U, Y);
  q2 = tpom(V);
  g1 = tiou(U, W); 
  g2 = tiom(V);

  % balance
  if (arrayequals(q1, q2, criterium) == 1)
     printf('product output balance okay\n');     
  else 
     printf('product output not balanced\n');
     retval= 0;
  endif
  
  if (arrayequals(g1, g2, criterium) == 1)
    printf('industry output balance okay\n');
  else 
    printf('industry output not balanced\n');
    retval = 0;
  endif  
  
  
  
  % value added and final uses in each region
  sumvalueadded = valueadded(W, dim);
  sumfinaluses = finaluses(Y, dim);
  
  % balance
  if (arrayequals(sumvalueadded, sumfinaluses, criterium) == 1)
    printf('value added and final use per region okay\n');
  else
    printf('value added and final use per region not okay\n');
    retval = 0;
  endif  
  
  
  
  % gdp in each region from different points of view
  gdp_income = gdpincome(W, dim);
  gdp_expenditure = gdpexpenditure(U, Y, dim);
  gdp_production = gdpproduction(U, Y, dim);
  
  % balance
  if (arrayequals(gdp_income, gdp_expenditure, criterium) == 1 && arrayequals(gdp_income, gdp_production, criterium) == 1)
    printf('GDP per region okay\n\n');
  else
    printf('GDP per region not okay\n\n');
    retval = 0;
  endif
  
  % maximum difference
  max_diff_product = max(abs(tpom(V) - tpou(U,Y)));
  max_diff_industry = max(abs(tiom(V) - tiou(U,W)));
  fprintf('Maximum absolute difference between total product supply and total product use: %e \n', max_diff_product);
  fprintf('Maximum absolute difference between total industry output and total industry input: %e \n', max_diff_industry);
  
  
endfunction