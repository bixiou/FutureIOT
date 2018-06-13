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
% test the efficiencychange scenario function
% function retval = test_efficiencychange()
%
% Returns 1 if efficiencychange.m performs as expected. Returns 0 
% if the result is not correct.
%

function retval = test_efficiencychange()

  result = 0;
  
  diff = 0.01;
  
  h = [1.1; 1.1; 1.1; 1.1];
  
  dim = 2;

  U = [5,8,0.5,0.5; ...
       6,3,1.5,0; ...
       0.5,1,2,8; ...
       1,0.5,9,9];
  
  V  = [13.5, 6.5, 0, 0; ...
        7.5, 13, 0, 0; ...
        0, 0, 11.5, 6.5; ...
        0, 0, 7, 21.5];
  
  W = [2.5,4.5,1,4; ...
       5,3.5,4,7];
       
  Y = [5.5, 1.5; ...
       8, 1; ...
       1, 6; ...
       1, 7.5];
  
  M = [10, 10, 10, 10];
  
  FM = [4, 4];
  
  R = [1,1,1,1];

  [Unew, Vnew, Wnew, Ynew, Mnew, FMnew, Rnew]= efficiencychange(U, V, W, Y, M, FM, R, h, dim);
 
  Vexpected  = [13.5, 6.5, 0, 0; ...
                7.5, 13, 0, 0; ...
                0, 0, 11.5, 6.5; ...
                0, 0, 7, 21.5];
        
  Uexpected = [4.54466, 7.27109, 0.45000, 0.44936; ...
              5.45104, 2.72538, 1.34938, 0.00000; ...
              0.45950, 0.91896, 1.81996, 7.26953; ...
              0.91896, 0.45946, 8.18943, 8.17782];
                
  Wexpected = [2.8739, 5.1312, 1.2375, 4.5809; ...
               5.7520, 3.9939, 4.9537, 8.0224];
                 
  Yexpected = [6.5164, 1.7685; ...
               8.8707, 1.1034; ...
               1.1522, 6.8798; ...
               1.2116, 9.0427];
     
  if (arrayequals(Uexpected, Unew, diff) == 1) 
    result = result + 1;
  endif
  
  if (arrayequals(Vexpected, Vnew, diff) == 1) 
    result = result + 1;
  endif
  
  if (arrayequals(Wexpected, Wnew, diff) == 1) 
    result = result + 1;
  endif
    
  if (arrayequals(Yexpected, Ynew, diff) == 1) 
    result = result + 1;
  endif  
  
  
  if (result == 4) 
    printf('efficiencychange.m\tokay\n');
    retval = 1;
  else
    printf('efficiencychange.m\tnot okay\n');
    retval = 0;
  endif

  
endfunction  
  