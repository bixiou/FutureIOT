function max_relative_distance(mat1, mat2)
  max(max(2*abs(mat1 - mat2)./abs(mat1 + mat2)))
endfunction