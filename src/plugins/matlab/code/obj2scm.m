function tmp=obj2scm(n)
	switch (typeinfo(n))
		case ('matrix')
			tmp=mat2scm(n);
		case ('complex matrix')
			tmp=mat2scm(n);
		case ('bool matrix')
			tmp=mat2scm(n);
		case ('scalar')
			tmp=num2scm(n);
		case ('complex scalar')
			tmp=num2scm(n);
		case ('bool scalar')
			tmp=num2scm(n);
		case ('struct')
			tmp=struct2scm(n,1,'(with "mode" "math" (big "triangleup"))');
		case ('string')
			tmp=str2scm(n);
		otherwise
			tmp='';
	end
