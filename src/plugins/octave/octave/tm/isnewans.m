function L=isnewans(A)
        switch(typeinfo(A))
                case ("string")
                        if strcmp(A,"texmacs")
                                L=0;
                        else
                                L=1;
                        endif
                otherwise
                        L=1;
        endswitch
endfunction
