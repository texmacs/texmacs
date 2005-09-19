
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

tmatomic := proc (x)
  if tmmaple < 9 then
    type (x, string) or type (x, numeric)
  else
    type (x, atomic)
  fi
end:

##############################################################################
# Print by case distinction
##############################################################################

tmout := proc (x, p)
local i, t, n, y;
  t := op (0, x):
  n := nops (x):
  y := sort ([op (x)]):

  if p <= 20 and t = `+` then
    if n = 1 then printf (`0`)
    else
      printf (`(concat`):
      for i from 1 to n do
        if tmisopposite (op (i, y)) = 1 then
          if i<>1 then printf (` \"-\" `) else printf (` \"<um>\" `) fi:
          tmout (tmremoveopposite (op (i, y)), 21)
        else
          if i<>1 then printf (` \"+\" `) else printf (` `) fi:
          tmout (tmremoveopposite (op (i, y)), 20)
        fi
      od:
      printf (`)`)
    fi

  elif p <= 20 and t = `tmopposite` then
    printf (`(concat \"<um>\" `):
    tmout (op (1, x), 21):
    printf (`)`)

  elif p <= 21 and t = `*` then
    if n = 1 then printf (`0`)
    else
      printf (`(concat `):
      for i from 1 to n do
        if i<>1 then printf (` \"*\" `) fi:
        tmout (op (i, y), 21)
      od:
      printf (`)`)
    fi

  elif p <= 22 and t = `Fraction` then
    printf (`(frac `):
    tmout (op (1, x), 22):
    printf (` `):
    tmout (op (2, x), 23):
    printf (`)`)

  elif p <= 23 and t = `^` then
    printf (`(concat `):
    tmout (op (1, x), 24):
    printf (` `):
    printf (`(rsup `):
    tmout (op (2, x), 0):
    printf (`)`):
    printf (`)`)

  elif t = `+` or t = `*` or t = `Fraction` or t = `^` then
    printf (`(concat `):
    printf (`(left \"(\")`):
    tmout (x, 0):
    printf (`(right \")\")`):
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
      printf (`(right \"]\")`)
    else
      printf (`(right \"}\")`)
    fi:
    printf (`)`)

  elif t = `integer` or t = `Integer` or t = `string` or t = `symbol` then
    if op (0, tmvars[x]) = 'symbol' or op (0, tmvars[x]) = 'string' then
      printf (`\"<%a>\"`, tmvars[x])
    else
      printf (`\"%a\"`, x)
    fi

  else
    printf (`(concat `):
    printf (`\"%a\" (left \"(\")`, t):
    for i from 1 to n do
      if i<>1 then printf (` \",\"`) fi:
      tmout (op (i, x), 0)
    od:
    printf (` (right \")\")`)
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

  if t = `Fraction` then
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

  elif t = `Integer` or t = `integer` then
    if x < 0 then tmopposite (-x) else x fi

  elif tmatomic (x) then
    x

  else
    map (procname, x)

  fi
end:

##############################################################################
# Top-level interface
##############################################################################

#tmprint := proc (x)
#  printf (`%c`, 2):
#  printf (`scheme:(math `):
#  tmout (tmrewrite (x), 0):
#  printf (`)`):
#  printf (`%c`, 5):
#  printf (`\n`):
#end:

tmprint := proc (x)
  printf (`%c`, 2):
  printf (`latex:$`):
  latex (x);
  printf (`$`):
  printf (`%c`, 5):
  printf (`\n`):
end:

unprotect (print):
unprotect (lprint):
print := tmprint:
lprint := tmprint:
