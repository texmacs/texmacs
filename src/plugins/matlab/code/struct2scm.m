function tmp=struct2scm(n,c,b)

	global TMSTRUCT;

	if (TMSTRUCT) 
		tmp=struct2bullet(n,c);
	else
		if (nargin<3)
			b='name';
		end
		tmp=struct2tree(n,b);
	end
