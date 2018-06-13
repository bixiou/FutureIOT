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
% Testing all utility functions
%


% settings
clear -x time_start;
more off;
utildir = strcat(pwd(),'/../util/');
scendir = strcat(pwd(),'/../scenarios/');
addpath(utildir);
addpath(scendir);


% count result
global success_cnt = 0;
global fail_cnt = 0;


% function to keep track of the number of failed and successful tests
function add_result(test_result)

  global success_cnt;
  global fail_cnt;
  
  if (test_result == 0)
    fail_cnt = fail_cnt + 1;
  elseif (test_result == 1)
    success_cnt = success_cnt + 1;
  else
    error('test result should be either 0 (fail) or 1 (success)'); 
  endif
  
endfunction


% run tests on utility functions
add_result(test_removenegatives());
add_result(test_tiou());
add_result(test_tiom());
add_result(test_tpou());
add_result(test_tpom());
add_result(test_finaluses());
add_result(test_valueadded());
add_result(test_gdpexpenditure());
add_result(test_gdpincome());
add_result(test_gdpproduction());
add_result(test_checkbalance());
add_result(test_totaltradevolume());
add_result(test_importtradevolumes());
add_result(test_sumtiom());
add_result(test_sumtiou());
add_result(test_sumtpou());
add_result(test_sumtpom());
add_result(test_invd());
add_result(test_gras());
add_result(test_sharemainproduct());
add_result(test_efficiency());
add_result(test_insertcolumns());
add_result(test_insertrows());
add_result(test_removecolumns());
add_result(test_removerows());
add_result(test_originfinaluse());
add_result(test_changefinaldemandproducts());
add_result(test_changevalueaddedindustries());


% run tests on scenario functions
add_result(test_gdpchange());
add_result(test_efficiencychange());


% final result
total = success_cnt + fail_cnt;
printf('%d out of %d checks okay\n',success_cnt, total);
printf('%d out of %d checks not okay\n',fail_cnt, total);



