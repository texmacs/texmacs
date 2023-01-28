#!/usr/bin/env python
# -*- coding: utf-8 -*-

import os, glob, platform, time
import inkex, tempfile, subprocess

class SetupTeXmacsPath(inkex.EffectExtension):

    def add_arguments(self, pars):
        pars.add_argument("--tab", type=str, default="")
        pars.add_argument("--texmacs_exec", type=str, default="")


    def effect(self):
        # Get all the options
        texmacs_path = self.options.texmacs_exec
        tab = self.options.tab
        UserPath = inkex.utils.get_user_directory()
        conf_file = UserPath+'/texmacs/texmacs_path.conf'
        if  (texmacs_path != None ) and  (texmacs_path != '' ) :
            with open(conf_file, 'w') as f:
              f.write(texmacs_path)
        else : 
            if os.path.isfile(conf_file):
                os.remove(conf_file)
 
#------------------------------------------------------------------------------
# Entry point
#------------------------------------------------------------------------------

if __name__ == "__main__":
    SetupTeXmacsPath().run()
