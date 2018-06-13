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
% Function to calculate the inverse of a (diagonalized) vector possibly 
% containing zeros
%
% function [invvec] = invd(vec)
% Takes a column vector and returns a column vector
%
% Programmed by Dirk Stelder in Gauss - University of Groningen
% transferred to Matlab by Maaike Bouwmeester - University of Groningen
%

function [invvec] = invd(vec)

[numRows numCols] = size(vec);

if numCols ~= 1           % test whether the vector is a column vector
    error('ERROR: Not a column vector') 
else
    invvec = zeros(numRows,1);  % initialize return vector with zeros
    for row = 1 : numRows
        if vec(row,1) ~= 0      % if value is not 0, calculate 1/value
             invvec(row,1) = 1 / vec(row,1);           
        end
    end
end


        
