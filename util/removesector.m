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
% Remove a sector from the Supply-Use tables without changing dimensions of the system
% [Unew, Vnew, Wnew, Ynew, Mnew] = removesector(U, V, W, Y, M)
%
% returns a new set of Supply-Use tables where the sector has been removed
% Unew = use table (product by industry) with zeros in the row and column at the position of the removed sector 
% Vnew = make table (industry by product) with zeros in the row and column at the position of the removed sector
% Wnew = value added table (value added items by industry) with zeros in the column at the position of the removed sector 
% Ynew = final use table (products by final demand items) with zeros in the row at the position of the removed sector 
% Mnew = environmental extensions table (emissions by industry) with zeros in the column at the position of the removed sector 
%
% Need in workspace:
% idx = index of the sector to be removed - base 1
% U = intermediate use table (product by industry) at basic price = matrix
% V = make table (industry by product) at basic price = matrix
% W = factor inputs table at basic price = matrix
% Y = final use table at basic price = matrix
% M = environmental extensions table = matrix
%

function [Unew, Vnew, Wnew, Ynew, Mnew] = removesector(U, V, W, Y, M, idx)
  Unew = U;
  Vnew = V;
  Wnew = W;
  Ynew = Y;
  Mnew = M;
   
  Unew(idx,:) = 0;
  Unew(:,idx) = 0;
  Vnew(idx,:) = 0;
  Vnew(:,idx) = 0;
  Ynew(idx,:) = 0;
  Wnew(:,idx) = 0;
  Mnew(:,idx) = 0;
  
endfunction
