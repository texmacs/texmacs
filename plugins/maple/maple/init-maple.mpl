
##############################################################################
# Special symbols
##############################################################################

tmvars := table ():

tmvars[alpha] := `%alpha`:
tmvars[beta] := `%beta`:
tmvars[gamma] := `%gamma`:
tmvars[delta] := `%delta`:
tmvars[epsilon] := `%epsilon`:
tmvars[zeta] := `%zeta`:
tmvars[eta] := `%eta`:
tmvars[theta] := `%theta`:
tmvars[iota] := `%iota`:
tmvars[kappa] := `%kappa`:
tmvars[lambda] := `%lambda`:
tmvars[mu] := `%mu`:
tmvars[nu] := `%nu`:
tmvars[xi] := `%xi`:
tmvars[omicron] := `%omicron`:
tmvars[pi] := `%pi`:
tmvars[rho] := `%rho`:
tmvars[sigma] := `%sigma`:
tmvars[tau] := `%tau`:
tmvars[upsilon] := `%upsilon`:
tmvars[phi] := `%phi`:
tmvars[psi] := `%psi`:
tmvars[chi] := `%chi`:
tmvars[omega] := `%omega`:

tmvars[Alpha] := `%Alpha`:
tmvars[Beta] := `%Beta`:
tmvars[Gamma] := `%Gamma`:
tmvars[Delta] := `%Delta`:
tmvars[Epsilon] := `%Epsilon`:
tmvars[Zeta] := `%Zeta`:
tmvars[Eta] := `%Eta`:
tmvars[Theta] := `%Theta`:
tmvars[Iota] := `%Iota`:
tmvars[Kappa] := `%Kappa`:
tmvars[Lambda] := `%Lambda`:
tmvars[Mu] := `%Mu`:
tmvars[Nu] := `%Nu`:
tmvars[Xi] := `%Xi`:
tmvars[Omicron] := `%Omicron`:
tmvars[Pi] := `%Pi`:
tmvars[Rho] := `%Rho`:
tmvars[Sigma] := `%Sigma`:
tmvars[Tau] := `%Tau`:
tmvars[Upsilon] := `%Upsilon`:
tmvars[Phi] := `%Phi`:
tmvars[Psi] := `%Psi`:
tmvars[Chi] := `%Chi`:
tmvars[Omega] := `%Omega`:

tmvars[mathd] := `%mathd`:
tmvars[mathe] := `%mathe`:
tmvars[mathi] := `%mathi`:
tmvars[Pi] := `%mathpi`:

##############################################################################
# Special operators
##############################################################################

tmops := table ():

tmops[`+`] := `+`:
tmops[`-`] := `-`:
tmops[`*`] := `*`:
tmops[`/`] := `/`:
tmops[`^`] := `^`:
tmops[`list`] := `list`:
tmops[`set`] := `set`:

##############################################################################
# Version dependent routines
##############################################################################

tmis_fraction := proc (x)
  op (0, x) = `fraction` or op (0, x) = `Fraction`
end:

tmis_complex := proc (x)
  op (0, x) = `Complex`
end:

tmis_number := proc (x)
  type (x, numeric)
end:

tmis_atomic := proc (x)
  if tmmaple < 9 then
    type (x, string) or type (x, numeric)
  else
    type (x, atomic)
  fi
end:

tmis_indexed := proc (x)
  type (x, indexed)
end:

tmis_matrix := proc (x)
  type (x, matrix)
end:

tmis_series := proc (x)
  type (x, series)
end:

##############################################################################
# Print by case distinction
##############################################################################

tmout := proc (x)
local t, i, j, n;
  t := op (0, x):
  n := nops (x):

  if tmis_fraction (x) then
    printf (`(/ `):
    tmout (op (1, x)):
    printf (` `):
    tmout (op (2, x)):
    printf (`)`)

  elif tmis_complex (x) then
    if n = 1 then
      printf (`(* `):
      tmout (op (1, x)):
      printf (` %mathi)`)
    else
      printf (`(+ `):
      tmout (op (1, x)):
      printf (` (* `):
      tmout (op (2, x)):
      printf (`))`)
    fi

  elif tmis_atomic (x) then
    if tmis_atomic (tmvars[x]) then
      printf (`%s`, tmvars[x])
    else
      printf (`%a`, x)
    fi

  elif tmis_indexed (x) then
    printf (`(_ `):
    tmout (op (0, x)):
    printf (` (comma`):
    for i from 1 to n do
      printf (` `):
      tmout (op (i, x))
    od:
    printf (`))`)

  elif tmis_matrix (x) then
    printf (`(matrix`):
    for i from 1 to linalg[rowdim] (x) do
      printf (` (row`):
      for j from 1 to linalg[coldim] (x) do
        printf (` `):
        tmout (x[i,j])
      od:
      printf (`)`):
    od:
    printf (`)`)

  elif tmis_series (x) then
    printf (`(+&`):
    for i from 1 to n/2 do
      if evalb (op (2*i - 1, x) = O(1)) then
        printf (` (O (^ `):
        tmout (op (0, x)):
        printf (` `):
        tmout (op (2*i, x)):
        printf (`))`)
      else
        printf (` (* `):
        tmout (op (2*i - 1, x)):
        printf (` (^ `):
        tmout (op (0, x)):
        printf (` `):
        tmout (op (2*i, x)):
        printf (`))`)
     fi
    od:
    printf (`)`)

  else
    printf (`(`):
    if tmis_atomic (tmops[t]) then
      printf (`%s`, tmops[t])
    else
      tmout (op (0, x))
    fi:
    for i from 1 to n do
      printf (` `):
      tmout (op (i, x))
    od:
    printf (`)`)
  fi
end:

##############################################################################
# Top-level interface
##############################################################################

tmprint1 := proc ()
local i, n;
  n := nargs:
  printf (`%c`, 2):
  printf (`math:(comma`):
  for i from 1 to n do
    printf (` `):
    tmout (args[i])
  od:
  printf (`)`):
  printf (`%c`, 5):
  printf (`\n`)
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
  printf (`\n`)
end:

tmprint := tmprint2:

unprotect (print):
unprotect (lprint):
print := tmprint:
lprint := tmprint:
