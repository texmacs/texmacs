function tmdisp(M)
	if (length(getenv('TEXMACS_PATH'))>0)
		tmp1=obj2scm(M);
		if strcmp(tmp1,'')
			disp(M);
		else
			two=sprintf('%c',2); five=sprintf('%c',5);
			tmp=[two,'scheme:',tmp1,five];
			disp(tmp);
		end
	else
		disp(M);
	end
