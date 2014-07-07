function tmrepl()
	prompt=sprintf("%cchannel:prompt%coctave> %c",2,5,5);
	r=input(prompt, "s");
	ans="texmacs";
	while ( 1 )
		if r(length(r))!=";"
			dispans=1;
		else
			dispans=0;
		endif
                ans= "texmacs";
		r=sprintf("%s;",r);
		eval(r,"tmlasterr");
		if dispans && isnewans(ans)
                       tmdisp(ans);
		endif
		r=input(prompt,'s');
	endwhile
endfunction
