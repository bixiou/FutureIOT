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
% Taking the well balanced SUTs for 2050 which are a result of the BAU 2050 
% scenario implementation, the technological specific scenarios are 
% implemented in the SUTs by adjusting the transactions in a systematic way. 
%

% Step 1: program Initialisation

    % program settings
    clear -x time_start;
    more off;
    datadir = strcat(pwd(),'/../step2b/');
    utildir = strcat(pwd(),'/../util/');
    scendir = strcat(pwd(),'/../scenarios/');
    addpath(utildir);
    addpath(scendir);
    dim = 4;                        % number of regions
    fid = fopen ('log.txt', 'w');   % logging 


    % read files
    U = dlmread(strcat(datadir,'Uend.txt'), '\t');
    V = dlmread(strcat(datadir,'Vend.txt'), '\t');
    W = dlmread(strcat(datadir,'Wend.txt'), '\t');
    Y = dlmread(strcat(datadir,'Yend.txt'), '\t');
    M = dlmread(strcat(datadir,'Mend.txt'), '\t');
    FM = dlmread(strcat(datadir,'FMend.txt'), '\t');
    CF = dlmread('consumption_changes.txt', '\t');
    CE = dlmread('consumption_emission_modifiers.txt', '\t');
    
    if (checksanity(U,V,W,Y,dim) == 0)
      input('hit enter to continu after sanity check');
    endif


    % initialisation
    p_count = rows(U)/dim;
    i_count = columns(U)/dim;
    y_count = columns(Y)/dim;
    w_count = rows(W);
    m_count = rows(M);
 
 
    % properties of initial supply-use tables
    efficiency_initial = efficiency(U, V);
    gdp_income_init = gdpincome(W, dim);
    gdp_expenditure_init = gdpexpenditure(U, Y, dim);
    gdp_production_init = gdpproduction(U, Y, dim);
    [Itvolumes_init, Dtvolumes_init] = importtradevolumes(U, Y, dim);
	    
	    % export to log
	    fdisp(fid, 'Properties initial');
	    fdisp(fid, 'GDP income approach:');
	    fdisp(fid, gdp_income_init);
	    fdisp(fid, 'GDP expenditure approach:');
	    fdisp(fid, gdp_expenditure_init);
	    fdisp(fid, 'GDP production approach:');
	    fdisp(fid, gdp_production_init);
	    fdisp(fid, 'Intermediate international trade:');
	    fdisp(fid, Itvolumes_init);
	    fdisp(fid, 'Direct international trade:');
	    fdisp(fid, Dtvolumes_init);
	    fdisp(fid, 'Check balance:');
	    if (checkbalance(U, V, W, Y, dim) == 1) 
	      fdisp(fid, '   Balance OK');
	    else
	      fdisp(fid, '   Not balanced');
	    endif
	    fdisp(fid,'');
	    fdisp(fid,'');
   
    % apply techno changes to final use table
    % The consumption change factors (where there is a change i.e. not 1) 
    % have been defined with respect to 2000 and not with respect to 2050
    % BAU scenario. Thus in case there is coefficient that is not 1 or not 0
    % we have to look up the original 2000 final demand data that are available 
    % in the preprocess direcctory.
    [U, V, W, Y, M, FM] = consumptionchange(U, V, W, Y, M, FM, CF, CE, dim);
    
    
	    % export to log
	    fdisp(fid, 'Properties initial');
	    fdisp(fid, 'GDP income approach:');
	    fdisp(fid, gdp_income_init);
	    fdisp(fid, 'GDP expenditure approach:');
	    fdisp(fid, gdp_expenditure_init);
	    fdisp(fid, 'GDP production approach:');
	    fdisp(fid, gdp_production_init);
	    fdisp(fid, 'Intermediate international trade:');
	    fdisp(fid, Itvolumes_init);
	    fdisp(fid, 'Direct international trade:');
	    fdisp(fid, Dtvolumes_init);
	    fdisp(fid, 'Check balance:');
	    if (checkbalance(U, V, W, Y, dim) == 1) 
	      fdisp(fid, '   Balance OK');
	    else
	      fdisp(fid, '   Not balanced');
	    endif
	    fdisp(fid,'');
	    fdisp(fid,'');
  
  
  % export supply - use tables
  dlmwrite('Uend.txt', U, '\t');
  dlmwrite('Vend.txt', V, '\t');
  dlmwrite('Wend.txt', W, '\t');
  dlmwrite('Yend.txt', Y, '\t');
  dlmwrite('Mend.txt', M, '\t');
  dlmwrite('FMend.txt', FM, '\t');
  
  
  % output
  fclose(fid);
  