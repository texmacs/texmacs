function tmp=mat2scm(M)
	tmp='(with "mode" "math" "math-display" "true" (matrix (tformat (table';
	[r,c]=size(M);
	for i=1:r
		tmp=[tmp,'(row '];
		for j=1:c
			tmp1=num2scm(M(i,j));
			tmp=[tmp,'(cell ', tmp1, ') '];
		end
		tmp=[tmp,') '];
	end
	tmp=[tmp,'))))'];
