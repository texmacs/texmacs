#!/usr/bin/env python3
###############################################################################
##
## MODULE      : tm_kernelspecs
## DESCRIPTION : list the installed Jupyter kernels
## COPYRIGHT   : (C) 2021 Jeroen Wouters
##
## This software falls under the GNU general public license version 3 or later.
## It comes WITHOUT ANY WARRANTY WHATSOEVER. For details, see the file LICENSE
## in the root directory or <http://www.gnu.org/licenses/gpl-3.0.html>.

try:
    import jupyter_client
except ImportError:
    exit()
specManager = jupyter_client.kernelspec.KernelSpecManager()
for kernel in specManager.find_kernel_specs().keys():
    print(kernel)
