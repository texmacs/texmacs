function T=typeinfo(val)
	[M,N]=size(val);
	T='';
	if ischar(val)
		T='string';
	end
	if isstruct(val)
		T='struct';
	end
	if ~isreal(val) & isnumeric(val)
		T='complex ';
	end
	if islogical(val)
		T='bool ';
	end
	if isnumeric(val) & or(M>1,N>1)
		T=[T, 'matrix'];
	end
	if isnumeric(val) & M==1 & N==1
		T=[T, 'scalar'];
	end
	
