function scheme(S)
	if (length(getenv('TEXMACS_PATH'))>0)
		two=sprintf('%c',2); five=sprintf('%c',5);
		tmp=[two,'scheme:',S,five];
		disp(tmp);
	else
		disp('TeXmacs and Scheme not available.');
	end
