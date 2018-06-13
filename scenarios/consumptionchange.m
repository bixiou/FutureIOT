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
% Change the final use transactions according the consumption change factors such
% that new balanced SUTs are created.
% 
% [Unew, Vnew, Wnew, Ynew, Mnew, FMnew] = consumptionchange(U, V, W, Y, M, FM, CF, CE, dim)
%
% Returns adapted intermediate use, make, factor inputs and final use matrix 
% industry emissions and direct emission which conform to the requested 
% consumption change and are balanced.
% 
% Need in workspace: 
% U = intermediate use table (product by industry) at basic price = matrix
% V = make table (industry by product) at basic price = matrix
% W = factor inputs table at basic price = matrix
% Y = final use table at basic price = matrix
% CF = consumption factor table (product by two final use categories) = matrix
% CE = CO2 emission modifiers for private final consumption = column vector
% dim = number of regions in the multiregional supply-use system
%

function [Unew, Vnew, Wnew, Ynew, Mnew, FMnew ] = consumptionchange(U, V, W, Y, M, FM, CF, CE, dim)

  % initialisation
  p_count = rows(U)/dim;
  i_count = columns(U)/dim;
  y_count = columns(Y)/dim;
  w_count = rows(W);
  fossilIds = [18;20;52;55;92;97;147;149;181;184;221;226;276;278;310;313;350;355;405;407;439;442;479;484];
  fossilC = [21015504;4309356;3303646;2260691;1974093;24124;26877682;4002635;3061968;3323707;2158028;4724;22259512;23317201;3486961;2199381;3305499;104916;6402789;10696912;3570364;162803;3305499;5589];  % in kg C / M.EUR
 
  
  % read the year 2000 final demand and year 2000 direct emissions
      Y_2000 = dlmread('../preprocess/Y.txt', '\t');
      FM_2000 = dlmread('../preprocess/FM.txt', '\t');
 
 
  % General change final demand for products
  
	% change final demand according defined change factors 
	for i=1:rows(Y)
	  Ynew(i,:) = CF(i) .* Y(i,:);
	endfor
	
	% calculate new total product use and carry over 
	% changed product use to supply table    
	q1 = tpou(U, Ynew);
	q2 = tpom(V);
	sfactors = q1 ./ q2;
	sfactors(isnan(sfactors))=1;    % take out NaN and replace by 1  
	for i=1:rows(q1)
	  Vnew(:,i) = V(:,i) .* sfactors(i);
	endfor 
	
	
	% new total industry output carried over to use table
        g1 = tiou(U, W);
        g2 = tiom(Vnew);
        sfactors = g2 ./ g1;
        sfactors(isnan(sfactors))=1;    % take out NaN and replace by 1 
	for i=1:rows(g1)
	  Unew(:,i) = U(:,i) .* sfactors(i);
	  Wnew(:,i) = W(:,i) .* sfactors(i);
	  Mnew(:,i) = M(:,i) .* sfactors(i);
	endfor
	
	    
	% Rebalance if necessary
	while (checkbalance(Unew, Vnew, Wnew, Ynew, dim) == 0) 
	  [Unew, Vnew, Wnew, Ynew] = rebalance(Unew, Vnew, Wnew, Ynew, dim);
	endwhile
	         
      

  % adjust consumption emissions
  
  
      % take original 2000 CO2 final consumption emissions
      % available in preprocess/FM.txt in the first row of that matrix
      FM_2000 = dlmread('../preprocess/FM.txt', '\t', [0, 0, 0, 27]);
      
      % take original 2000 fossil fuel final consumption
      % available in preprocess/Y.txt at row id allFossilIds
      fossil_2000 = Y_2000(fossilIds,:);
      dlmwrite('fossil_2000.txt', fossil_2000, '\t');
      
      
      % take 2050 fossil fuel consumption at row id allFossilIds
      fossil_2050 = Ynew(fossilIds,:);
      dlmwrite('fossil_2050.txt', fossil_2050, '\t');
      
      
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
      % private household consumption emissions
      for i = 1 : columns(FM)
	FM(1, i) = FM_2000(1, i) * carbondioxide_factor(i);
      endfor      
      FMnew = FM;
      
          
	
endfunction  
