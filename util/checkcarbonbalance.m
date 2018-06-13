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
% Checks if the inputs by way of carbon extraction and carbon emissions
% is within reasonable bounds.
% retval= checkcarbonbalance(M, FM, R)
%
% returns 1 if carbon emissions are between 0 and 10% higher than carbon 
% resource extraction. If carbon extraction is larger than carbon emissions
% or carbon emissions are more than 10 higher than carbon extraction than
% it returns 0. Prints the percentage difference between carbon emissions
% and carbon extraction.

% Need in workspace: 
% M = emissions from industry sectors = matrix
% FM = emissions from households (direct emissions) = matrix
% R = fossil carbon resource extraction by industry sectors = matrix
%

function retval= checkcarbonbalance(M, FM, R)


  c_content_m = [0.2729, 0.7487, 0];
  c_content_r = [665640, 328440, 846000, 734400, 773500, 282064];
  criterium = 0.1;
  retval= 1;


  % total emissions
  mtot = sum(M, 2) + sum(FM, 2);

  % total resource use
  rtot = sum(R, 2);
  
  
  % total kg C emissions
  mtotc = c_content_m * mtot;
  
  
  % total kg C resource use
  rtotc = c_content_r * rtot;
  
  
  if (mtotc < rtotc) 
    retval = 0;
    reldiff = 100 * ((rtotc-mtotc)/rtotc); 
    fprintf('Carbon resource use is %i%% higher than carbon emissions\n', reldiff);
  else 
    reldiff = 100 * ((mtotc-rtotc)/mtotc); 
    fprintf('Carbon emission %i%% higher than carbon resource use\n', reldiff);
    if (reldiff > 10)
      retval = 0;
    else
      retval = 1;
    endif
  endif
  
  
  
endfunction