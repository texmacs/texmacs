function tmp=struct2bullet(n,c)

	if (c==1)
		tmp='(itemize-arrow (document ';
	else
		tmp='(itemize (document ';
	end

	FIELDS=fieldnames(n);
	for i=1:length(FIELDS)
		key=char(FIELDS(i));
		val=getfield(n,key);
		switch (typeinfo(val))
			case ('struct')
				tmp1=['(concat (item) (with "color" "', 'black', '" "', key, ' = ") ', struct2bullet(val,c+1),')'];
			otherwise
				tmp1=['(concat (item) (with "color" "', 'black', '" "', key, ' = ") ', obj2scm(val),')'];
		end
		tmp=[tmp,tmp1];
	end
	tmp=[tmp,'))'];
