texmacslib = ""

function texout(v)
  DATA_BEGIN=ascii(2);
  DATA_END=ascii(5)
  mprintf(strsubst(DATA_BEGIN+"latex:$"+prettyprint(v)+'$'+DATA_END,'\','\\'))
endfunction

funcprot(0)

function %b_p(a)
texout(a)
endfunction

function %c_p(a)
texout(a)
  return
  t=''
  for k=1:size(a,2)
    t=t+part(a(:,k),1:max(length(a(:,k)))+1)
  end
  write(%io(2),t,'(a)')
endfunction

function %i_p(a)
texout(a)
endfunction

function %l_p(s)
  str=' '
  select type(s)
  case 16 then // case for lists : http://help.scilab.org/docs/5.3.2/en_US/type.html
    f=getfield(1,s),1,-1
    first=2
    write(%io(2),gettext('   tlist of type ')+f(1)+gettext(' with fields:'))
  case 17 then // case for typed lists
    f=getfield(1,s),1,-1
    first=2
    write(%io(2),gettext('   mlist of type ')+f(1)+gettext(' with fields:'))
  else
    f=[]
    first=1
    write(%io(2),gettext('   list with entries:'))
  end
  ll=lines()
  nf=size(f,'*');ns=lstsize(s)
  for k=first:max(nf,ns)
    if k<=nf then
      var=f(k),
      value=s(var)
      label=var
    else
      var=k,
      if k<=ns then value=s(k),else value='Undefined',end
      label=string(k)
    end
    txt='    '+label+': '
    if or(type(value)==[130 9])
      txt=txt+'['+strcat(string(size(value)),'x')+' '+typeof(value)+']'
    else
      str=sci2exp(value,ll(1))
      if size(str,'*')==1 then
	txt=txt+str
      else
	txt=txt+'['+strcat(string(size(value)),'x')+' '+typeof(value)+']'
      end
    end
    write(%io(2),txt,'(a)')
  end
endfunction

function %p_p(a)
texout(a)
endfunction

function %s_p(a)
texout(a)
endfunction

funcprot(1)

banner
