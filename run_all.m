%   1. U.txt - intermediate use table
%   2. V.txt - make table
%   3. W.txt - value added table
%   4. Y.txt - final use table
%   5. M.txt - direct GHG emissions from industry
%   6. FM.txt - direct emissions from households
%   7  R.txt - fossil carbon resource extraction

clear all;
more off;

%pwd()
time_start = time();
cd step1;
step1;
cd ../step2a;
step2a
cd ../step2b;
step2b;
cd ../step3;
step3;
cd ../test;
tests;
fprintf('Duration:', (time()-time_start)/60, 'min')

utildir = strcat(pwd(),'/util/');
addpath(utildir);

% check if we find the same result as the original code 
%   should be 1 iff no matrix was modified => but is not
cd ../step3
U_original = dlmread('Uend_original.txt', '\t');
V_original = dlmread('Vend_original.txt', '\t');
W_original = dlmread('Wend_original.txt', '\t');
Y_original = dlmread('Yend_original.txt', '\t');
M_original = dlmread('Mend_original.txt', '\t');
FM_original = dlmread('FMend_original.txt', '\t');
%U = dlmread(strcat(pwd(),'/step3/','Uend.txt'), '\t');
U = dlmread('Uend.txt', '\t');
V = dlmread('Vend.txt', '\t');
W = dlmread('Wend.txt', '\t');
Y = dlmread('Yend.txt', '\t');
M = dlmread('Mend.txt', '\t');
FM = dlmread('FMend.txt', '\t');
max_relative_distance(U, U_original)
max_relative_distance(V, V_original)
max_relative_distance(W, W_original)
max_relative_distance(Y, Y_original)
max_relative_distance(M, M_original)
max_relative_distance(FM, FM_original)
%all(md5sum("Uend.txt")==md5sum("Uend_original.txt"))