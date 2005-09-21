
##############################################################################
# Special symbols
##############################################################################

tmvars := table ():

tmvars[alpha] := `alpha`:
tmvars[beta] := `beta`:
tmvars[gamma] := `gamma`:
tmvars[delta] := `delta`:
tmvars[epsilon] := `epsilon`:
tmvars[zeta] := `zeta`:
tmvars[eta] := `eta`:
tmvars[theta] := `theta`:
tmvars[iota] := `iota`:
tmvars[kappa] := `kappa`:
tmvars[lambda] := `lambda`:
tmvars[mu] := `mu`:
tmvars[nu] := `nu`:
tmvars[xi] := `xi`:
tmvars[omicron] := `omicron`:
tmvars[pi] := `pi`:
tmvars[rho] := `rho`:
tmvars[sigma] := `sigma`:
tmvars[tau] := `tau`:
tmvars[upsilon] := `upsilon`:
tmvars[phi] := `phi`:
tmvars[psi] := `psi`:
tmvars[chi] := `chi`:
tmvars[omega] := `omega`:

tmvars[Alpha] := `Alpha`:
tmvars[Beta] := `Beta`:
tmvars[Gamma] := `Gamma`:
tmvars[Delta] := `Delta`:
tmvars[Epsilon] := `Epsilon`:
tmvars[Zeta] := `Zeta`:
tmvars[Eta] := `Eta`:
tmvars[Theta] := `Theta`:
tmvars[Iota] := `Iota`:
tmvars[Kappa] := `Kappa`:
tmvars[Lambda] := `Lambda`:
tmvars[Mu] := `Mu`:
tmvars[Nu] := `Nu`:
tmvars[Xi] := `Xi`:
tmvars[Omicron] := `Omicron`:
tmvars[Pi] := `Pi`:
tmvars[Rho] := `Rho`:
tmvars[Sigma] := `Sigma`:
tmvars[Tau] := `Tau`:
tmvars[Upsilon] := `Upsilon`:
tmvars[Phi] := `Phi`:
tmvars[Psi] := `Psi`:
tmvars[Chi] := `Chi`:
tmvars[Omega] := `Omega`:

tmvars[mathd] := `mathd`:
tmvars[mathe] := `mathe`:
tmvars[mathi] := `mathi`:
tmvars[Pi] := `pi`:
#tmvars[Pi] := `mathpi`:

##############################################################################
# Version dependent routines
##############################################################################

tmis_number := proc (x)
# op (0, x) = `integer` or op (0, x) = `Integer`
  type (x, numeric)
end:

tmis_atomic := proc (x)
  if tmmaple < 9 then
    type (x, string) or type (x, numeric)
  else
    type (x, atomic)
  fi
end:

tmis_opp := proc (x)
  op (0, x) = `tmopposite`
end:

tmis_frac := proc (x)
  op (0, x) = `Fraction` or
  op (0, x) = `fraction` or
  op (0, x) = `tmfraction`
end:

tmis_pow := proc (x)
  op (0, x) = `^`
end:

tm_pretreat_prod := proc (x)
local num, den;
  num := 1:
  den := 1:
  for i from 1 to nops (x) do
    if tmis_pow (op (i, x)) and tmis_opp (op ([i, 2], x)) then
      den := den * (op ([i, 1], x) ^ op ([i, 2, 1], x))
    else
      num := num * op (i, x)
    fi
  od:
  if den = 1 then
    num
  else
    tmfraction (num, den)
  fi
end:

##############################################################################
# Priorities
##############################################################################

tmp_sum  := 10:
tmp_prod := 20:
tmp_frac := 30:
tmp_pow  := 40:

##############################################################################
# Ordering terms
##############################################################################

tm_size := proc (x)
local i, s;
  if nops(x) = 0 then
    1
  elif nops (x) = 1 and evalb (op (1, x) = x) then
    1
  else
    s := 1:
    for i from 1 to nops (x) do
      s := s + tm_size (op (i, x))
    od:
    s
  fi
end:

tm_order := proc (x, y)
local sx, sy;
  sx := tm_size (x):
  sy := tm_size (y):
  if sx < sy then true
  elif sy < sx then false
  else evalb (sort ([x, y]) = [x, y])
  fi
end:

tmsum_order := proc (x, y)
  if tmis_number (x) or tmis_number (y) then
    if not (tmis_number (x)) then true
    elif not (tmis_number (y)) then false
    else tm_order (x, y)
    fi
  else
    tm_order (y, x)
  fi
end:

tmprod_order := proc (x, y)
  if tmis_number (x) or tmis_number (y) then
    if not (tmis_number (x)) then false
    elif not (tmis_number (y)) then true
    else tm_order (x, y)
    fi
  else
    tm_order (x, y)
  fi
end:

##############################################################################
# Print by case distinction
##############################################################################

tmout := proc (x, p)
local i, t, n, y, aux;
  t := op (0, x):
  n := nops (x):

  if p <= tmp_sum and t = `+` then
    if n = 1 then printf (`0`)
    else
      y := sort ([op (x)], tmsum_order):
      printf (`(concat`):
      for i from 1 to n do
        if tmisopposite (op (i, y)) = 1 then
          if i<>1 then printf (` \"-\" `) else printf (` \"<um>\" `) fi:
          tmout (tmremoveopposite (op (i, y)), tmp_sum+1)
        else
          if i<>1 then printf (` \"+\" `) else printf (` `) fi:
          tmout (tmremoveopposite (op (i, y)), tmp_sum)
        fi
      od:
      printf (`)`)
    fi

  elif p <= tmp_sum and t = `tmopposite` then
    printf (`(concat \"<um>\" `):
    tmout (op (1, x), tmp_sum+1):
    printf (`)`)

  elif p <= tmp_prod and t = `*` then
    aux := tm_pretreat_prod (x):
    if tmis_frac (aux) then tmout (aux, p)
    elif n = 1 then printf (`1`)
    else
      y := sort ([op (x)], tmprod_order):
      printf (`(concat `):
      for i from 1 to n do
        if i<>1 then printf (` \"*\" `) fi:
        tmout (op (i, y), tmp_prod)
      od:
      printf (`)`)
    fi

  elif p <= tmp_frac and tmis_frac (x) then
    printf (`(frac `):
    tmout (op (1, x), 0):
    printf (` `):
    tmout (op (2, x), 0):
    printf (`)`)

  elif p <= tmp_frac and tmis_pow (x) and tmis_opp (op (2, x)) then
    tmout (tmfraction (1, op (1, x) ^ op ([2, 1], x)), p)

  elif p <= tmp_pow and t = `^` then
    printf (`(concat `):
    tmout (op (1, x), tmp_pow+1):
    printf (` `):
    printf (`(rsup `):
    tmout (op (2, x), 0):
    printf (`)`):
    printf (`)`)

  elif t = `+` or tmis_opp (x) or t = `*` or tmis_frac (x) or t = `^` then
    printf (`(concat `):
    printf (`(left \"(\") `):
    tmout (x, 0):
    printf (` (right \")\")`):
    printf (`)`)

  elif t = `list` or t = `set` then
    printf (`(concat `):
    if t = `list` then
      printf (`(left \"[\")`)
    else
      printf (`(left \"{\")`)
    fi:
    for i from 1 to n do
      if i<>1 then printf (` \",\" `) fi:
      tmout (op (i, x), 0)
    od:    
    if t = `list` then
      printf (` (right \"]\")`)
    else
      printf (` (right \"}\")`)
    fi:
    printf (`)`)

  elif tmis_number (x) or t = `string` or t = `symbol` then
    if op (0, tmvars[x]) = 'symbol' or op (0, tmvars[x]) = 'string' then
      printf (`\"<%a>\"`, tmvars[x])
    else
      printf (`\"%a\"`, x)
    fi

  else
    printf (`(concat `):
    printf (`\"%a\" (left \"(\") `, t):
    for i from 1 to n do
      if i<>1 then printf (` \",\"`) fi:
      tmout (op (i, x), 0)
    od:
    printf (` (right \")\"))`)
  fi
end:

##############################################################################
# Top-level interface
##############################################################################

tmisopposite := proc (x)
  if op (0, x) = `tmopposite` then 1 else 0 fi
end:

tmremoveopposite := proc (x)
  if op (0, x) = `tmopposite` then op (1, x) else x fi
end:

tmrewrite := proc (x)
local i, t, n, y, u, v, p, q;
  t := op (0, x):
  n := nops (x):

  if tmis_frac (x) then
    p := tmrewrite (op (1, x)):
    q := tmrewrite (op (2, x)):
    y := (tmisopposite (p) + tmisopposite (q)) mod 2:
    u := tmremoveopposite (p):
    v := tmremoveopposite (q):
    if y = 0 then u/v else tmopposite (u/v) fi

  elif t = `*` then
    y := map (procname, x):
    if (`+` (op (map (tmisopposite, [op (y)]))) mod 2) = 0 then
      map (tmremoveopposite, y)
    else
      tmopposite (map (tmremoveopposite, y))
    fi

  elif t = `Complex` then
    if n = 1 then
      op (1, x) * `mathi`
    else
      op (1, x) + op (2, x) * `mathi`
    fi

  elif tmis_number (x) then
    if x < 0 then tmopposite (-x) else x fi

  elif tmis_atomic (x) then
    x

  else
    map (procname, x)

  fi
end:

##############################################################################
# Top-level interface
##############################################################################

tmprint1 := proc ()
local i;
  printf (`%c`, 2):
  printf (`scheme:(math (concat `):
  for i from 1 to nargs do
    if i>1 then printf (` "," `) fi:
    tmout (tmrewrite (args[i]), 0):
  od:
  printf (`))`):
  printf (`%c`, 5):
  printf (`\n`):
end:

tmprint2 := proc ()
local i;
  printf (`%c`, 2):
  printf (`latex:$`):
  for i from 1 to nargs do
    if i>1 then printf (`,`) fi:
    latex (args[i]);
  od:
  printf (`$`):
  printf (`%c`, 5):
  printf (`\n`):
end:

tmprint := tmprint2:

unprotect (print):
unprotect (lprint):
print := tmprint:
lprint := tmprint:
