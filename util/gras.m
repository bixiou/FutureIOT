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
% GRAS algorithm 
% Anew = gras(A, u, v, settings)
%
% Need in workspace: 
% A = start matrix 
% u = new row totals (numRows,1) = column vector
% v = new column totals (numCols,1) = column vector
% settings = struct with four fiels 
%                maxiter  = maximum number of iterations  
%                tol = use absolute tolerance criterium = 1, use relative tolerance criterium = 2
%                abs_tol = absolute tolerance criterium
%                rel_tol = relative tolerance criterium
%
%
% Program of Bertus Talsma, adapted by Dirk Stelder in Gauss - University of Groningen
% transferred to Matlab - Maaike Bouwmeester - University of Groningen
% 
function [Anew] = gras(A, u, v, settings)

% initialisation
[numRows numCols] = size(A);  
Anew = A;

% settings
iterator = 1;
sm = 0.0000000000000001;
converged = 0;
maxiter = settings.maxiter;
criterium = settings.criterium;
tol = settings.tol;

P = zeros(numRows, numCols);
N = zeros(numRows, numCols);
r = ones(numRows, 1);       % unity vector of same size as u
s = ones(numCols, 1);       % unity vector of same size as v

rinv = invd(r);

% split of matrix A into matrix P (positive values) and matrix N (negative
% values)
for row = 1:numRows
    for col = 1:numCols
        if A(row,col) >= 0;
            P(row,col) = A(row,col);
        else
            N(row,col) = -A(row,col);
        end
    end
end

pp = zeros(numCols,1);
ppp = zeros(numRows,1);
nn = zeros(numCols,1);
nnn = zeros(numRows,1);

while iterator < maxiter
    disp(sprintf('PROGRESS: Iteration nr: %d', iterator))

    for col = 1:numCols
        pp(col,1) = ones(1,numRows) * (P(:,col) .* r);
        nn(col,1) = ones(1,numRows) * (N(:,col) .* rinv);
    end
    
    s = (v + sqrt(v.^2 + 4 * pp .* nn)) ./ (2 * pp + sm);
    sinv = invd(s);
    
    rownew = ((diag(r) * P * diag(s))-(diag(rinv) * N * diag(sinv))) * ones(numCols,1);
    eps1_abs = ones(1, numRows) * abs(rownew - u);
    rel = abs(rownew ./ (u + sm) - 1); 
    rel(u==0) = 0;
    

    [maxVal maxIndex] = max(rel);
    eps1_rel = maxVal';
    indexmaxr = maxIndex';
    
    disp(sprintf('PROGRESS: Total absolute row difference: %d', eps1_abs))
    disp(sprintf('PROGRESS: Maximum relative row difference %e in sector %d', eps1_rel, indexmaxr))
    
    for row = 1:numRows
        ppp(row) = (P(row,:) .* s') * ones(numCols,1);
        nnn(row) = (N(row,:) .* sinv') * ones(numCols,1);
    end
    
    r = (u + sqrt(u.^2 + 4 * ppp .* nnn)) ./ ( 2 * ppp + sm);
    ri = invd(r);
    
    colnew = (ones(1, numRows) * ((diag(r) * P * diag(s)) - (diag(rinv) * N * diag(sinv))))';
    eps2_abs = ones(1, numCols) * abs(colnew - v);
    rel = abs(colnew ./ (v + sm) - 1);
    rel(v==0) = 0;
   

    [maxVal maxIndex] = max(rel);
    eps2_rel = maxVal';
    indexmaxc = maxIndex';
    
    disp(sprintf('PROGRESS: Total absolute column difference: %d', eps2_abs))
    disp(sprintf('PROGRESS: Maximum relative column difference %e in sector %d', eps2_rel, indexmaxc))
    
    if (tol == 1)
        if (eps1_abs + eps2_abs) / 2 < criterium
            iterator = maxiter;
            converged = 1;
            disp(sprintf('PROGRESS: Converged'))
        else
            iterator = iterator + 1;
        end
    endif
    
    if (tol == 2)
        if (eps1_rel + eps2_rel) / 2 < criterium
            iterator = maxiter;
            converged = 1;
            disp(sprintf('PROGRESS: Converged'))
        else
            iterator = iterator + 1;
        end
    endif
    
     
    if (tol == 3)
        abs_tol_current = (eps1_abs + eps2_abs) / 2;
        
        if (iterator > 1)
          improvement = (abs_tol_previous - abs_tol_current) / abs_tol_previous
        
        
          if (improvement < criterium)
            iterator = maxiter;
            converged = 1;
            disp(sprintf('PROGRESS: Converged'))
          else
            iterator = iterator + 1;
            abs_tol_previous = abs_tol_current;
          end
        
        else
            iterator = iterator + 1;
            abs_tol_previous = abs_tol_current;
        end
    endif
    
    
    
end

if converged == 1
    P = diag(r) * A * diag(s);
    N = diag(1./r) * A * diag(1./s);
    
    for row = 1:numRows
        for col = 1:numCols
            if P(row,col) >= 0
                Anew(row,col) = P(row,col);
            else
                Anew(row,col) = N(row,col);
            end
        end
    end
else 
    disp(sprintf('PROGRESS: Not converged'))
end


        

