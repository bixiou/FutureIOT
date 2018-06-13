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
% Calculate total product output from use table
% q = tpou(U, Y)
%
% Need in workspace: 
% U = intermediate use table at basic price = matrix
% Y = final use table at basic price = matrix
%
function q = tpou(U, Y)

  if (rows(U) != rows(Y))
    error('rows intermediate use table does not match rows in final use table');  
  endif

  q = sum(U, 2) + sum(Y, 2);

endfunction