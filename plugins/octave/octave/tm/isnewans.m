function L=isnewans(A)
  if !strcmp(typeinfo (A), "string") && !strcmp(typeinfo (A), "sq_string") && !strcmp(typeinfo (A), "dq_string")
    L= 1;
  elseif !strcmp(A, "texmacs")
    L= 1;
  else
    L= 0;
  endif
endfunction
