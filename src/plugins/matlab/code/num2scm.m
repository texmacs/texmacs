function tmp=num2scm(n)
	tmp='(with "mode" "math" "';
	if (isreal(n))
		tmp=[tmp, num2str(n), '" )'];
	else
		if (real(n) ~= 0)
			if (imag(n)>=0)
				op='+';
			else
				op='-';
			end
			tmp=['(with "mode" "math" "', num2str(real(n)),op,num2str(abs(imag(n))),'<cdot><b-i>")'];
		else
			tmp=['(with "mode" "math" "', num2str(imag(n)),'<cdot><b-i>")'];
		end
	end
