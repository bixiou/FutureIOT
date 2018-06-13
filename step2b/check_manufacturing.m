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




    % program settings
    clear all;
    more off;
    datadir = strcat(pwd(),'/../step2a/');
    utildir = strcat(pwd(),'/../util/');
    addpath(utildir);
    dim = 4;                        % number of regions
   

    
    cementIds = [64, 193, 322, 451];
    ironIdx = [66, 195, 324, 453];
    transportIdx = [102, 231, 360, 489];


    % read files
    Mold = dlmread(strcat(datadir,'Mend.txt'), '\t');
    Mnew = dlmread('Mend.txt', '\t');
 
 
    % get CO2 emissions cement
    cementOld = Mold(1, cementIds);
    cementNew = Mnew(1, cementIds);
    cementChange = transpose(100*((cementNew-cementOld)./cementOld))
    
    
    % get CO2 emissions basic ironIdx
    ironOld = Mold(1, ironIdx);
    ironNew = Mnew(1, ironIdx);
    ironChange = transpose(100*((ironNew-ironOld)./ironOld)) 
    
    
    % get CO2 emissions other land transport sevices
    transportOld = Mold(1, transportIdx);
    transportNew = Mnew(1, transportIdx);
    transportChange = transpose(100*((transportNew-transportOld)./transportOld)) 
