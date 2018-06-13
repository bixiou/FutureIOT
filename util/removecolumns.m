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
% Function to remove columns from a matrix. 
%
% function [reducedmatrix] = removecolumns(matrix,indices)
%
% Takes a matrix and a indices row vector and returns a matrix with the columns removed.
% The indices row vector must have the same number of elements as the number of columns of
% the matrix. The indices vector may contain zeros and ones. A zero indicates that the column
% has to be removed. A one indicates that the column has to remain 
%

function [reducedmatrix] = removecolumns(matrix, indices)

[numRows numCols] = size(matrix);
[n, m] = size(indices);

if n ~= 1    % test whether the indices vector is a row vector
  error('ERROR: Indices is not a row vector');
else
  if m ~= numCols % test whether number of elements in indices vector is the same as the number of columns
      error('ERROR: Number of columns in matrix is not the same as the length of the indices row vector');
  else

    % create row vector with indices of columns to delete
    deleteindices = not(indices); 
    deleteindices = find(deleteindices);

    % delete those columns
    reducedmatrix = matrix;
    reducedmatrix(:,deleteindices) =  [];

  end % else
end % else