function tmrepl()
	prompt=strcat(char(2),'channel:prompt',char(5),'matlab>> ',char(5));
	r=input(prompt,'s');
	ans='.';
	while ( 1 )
		if r(length(r))~=';'
			dispans=1;
		else
			dispans=0;
		end
		r=[39, 'texmacs',39,';' , r, ';'];
		eval(r,'disp(lasterr)');
		if dispans & isnewans(ans)
			tmdisp(ans);
		end
		r=input(prompt,'s');
	end

