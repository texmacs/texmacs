function tmp=struct2tree(n,name)

	if ((nargin<2))
		name='(with "mode" "math" (big "triangleup"))';
	end

	tmp=['(tree (with "color" "', 'black', '" ',name,') '];
	FIELDS=fieldnames(n);
	for i=1:length(FIELDS)
		key=char(FIELDS(i));
		val=getfield(n,key);
		switch (typeinfo(val))
			case ('struct')
				tmp1=struct2tree(val,key);
			otherwise
				tmp1=['(switch (document (with "color" "', 'black','" "',key,'")) (tuple (tmarker) (with "color" "', 'blue', '" ' ,obj2scm(val),'))) '];
		end
		tmp=[tmp,tmp1];
	end
	tmp=[tmp,')'];
