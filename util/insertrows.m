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
% Function to insert rows with zero values into a matrix. 
%
% function [fullmatrix] = insertrows(matrix,indices)
%
% Takes a matrix and a indices row vector and returns a matrix with rows inserted at the
% places indicated by the indices. The inserted rows contain zero values.
%
% The matrix returned will have the same number of rows as the lenght of the indices vector.
% The indices vector may contain zeros and ones. A zero indicates where a zero values row has to 
% be inserted. A one indicates where an existing row of the supplied matrix has to be. The number
% of 1's in the indices row vector has has to be equal to the number of rows in the matrix supplied. 
%

function [fullmatrix] = insertrows(matrix, indices)

[numRows numCols] = size(matrix);
[n, m] = size(indices);

if n ~= 1    % test whether the indices vector is a row vector
  error('ERROR: Indices is not a row vector');
else
  numberNonZero = columns(find(indices));
  if numberNonZero ~= numRows % test whether number of 1's in indices vector is the same as the number of rows in the matrix
      error('ERROR: Number of rows in matrix is not the same as the number of 1s in the indices row vector');
  else
  
    % create matrix filles with zeros with new dimensions;
    fullmatrix = zeros(m, numCols);

    % create row vector with indices of columns to insert 
    insertindices = find(indices);

    % insert those columns
    fullmatrix(insertindices,:) =  matrix(:,:);

  end % else
end % else