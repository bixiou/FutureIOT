function max_distance(mat1, mat2, precision = 10^-3)
  all(all(abs(mat1 - mat2)<precision))
endfunction