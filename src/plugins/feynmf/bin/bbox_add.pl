#!/usr/bin/perl 
# NOTE: The above path is machine dependent!!!

#------------------------------------------------------------------------
# file: bbox_add.pl
# 
# This script adds bounding boxes to any number of postscript files 
#  entered on the command line.
# 
#
# Author:  Dick Furnstahl
#          furnstahl.1@osu.edu
#
# Revision history:
#  0.90  17-Apr-00 --- original version, using ideas from the Perl Cookbook
#  0.91  19-May-00 --- upgraded documentation
#
# Requires:
#   * at least one filename on the command line
#   * wildcards are ok
#
# To do:
#   * check to make sure it IS a postscript file
#
# Notes:
#   * Requires gs with the bbox device option [6.0 or higher] 
#   * gs writes the BoundingBox specification to STDERR
#   * includes an option to specify the padding for the bounding box
#

# version number
$version = "0.91   [19-May-2000]";


# machine dependent variables
#  (none at present)

# load packages
use Getopt::Long;   # allow the extended processing of command line options

# set default options
$opt_padding = 3;   # add a 3 point border


# get the command-line options
GetOptions("help", "version", "keep", "padding=i");

# print out version if requested, then quit
if ($opt_version) {
   print "\nVersion number: $version\n";
   exit 1;
}

# print out help if requested, then quit
if ($opt_help) {
   print_help();
   exit 1;
}

# check for file names; quit if none specified
if (!$ARGV[0]) { 
   print "You must specify at least one file to convert.\n"; 
   print
 "Usage: bbox_add.pl [--help] [--version] [--keep] ";
   print "file(s)\n";
   exit 1;
}

# step through the filenames
foreach $old (@ARGV) {

#       run ghostscript with appropriate options, capturing STDERR
   my $gs_output = `gs -dNOPAUSE -dQUIET -dBATCH -sDEVICE=bbox $old 2>&1`;
   
#       pick off the bounding box specs from the output and add padding
   $gs_output =~ /%%BoundingBox:\s(\d+)\s(\d+)\s(\d+)\s(\d+)/;
   my $xl = $1 - $opt_padding;
   my $yl = $2 - $opt_padding;
   my $xu = $3 + $opt_padding;
   my $yu = $4 + $opt_padding;
   my $new_bbox = "%%BoundingBox: $xl $yl $xu $yu"; 
   
#    debugging statement
#   print "$new_bbox\n";

   open(OLD, "< $old")  or die "can't open $old: $!";  # open the ps file
   
   $new = "$old.tmp";   # add ".tmp" to the ps filename to make temporary name
   open(NEW, "> $new")  or die "can't open $new: $!";
   select(NEW);         # new default filehandle for print

   while (<OLD>) {       # step through original postscript file
      if (/^%%BoundingBox:/) {
        # don't print anything (so this line gets deleted) 
      } elsif (/^%%EndComments/) {
         print "$new_bbox\n";
         print NEW       or die "can't write $new: $!";
      } else {
         print NEW       or die "can't write $new: $!";
      }
   }

   close(OLD)           or die "can't close $old: $!";
   close(NEW)           or die "can't close $new: $!";
   if ($opt_keep) {
      rename($old, "$old.orig")  or die "can't rename $old to $old.orig: $!";
   }
   rename($new, $old)         or die "can't rename $new to $old: $!";

}



#*************************************************************************
#*************************************************************************


# ====================================================================
# name: print_help
# This subroutine prints the help info.
# ====================================================================
sub print_help{        # print the help info
   print <<END;
   
 The script bbox_add.pl adds a tight bounding box to postscript files.
  It uses gs with the bbox option (that means version 6.0 or later).

 Usage: bbox_add.pl [--help] [--version] [--keep] file(s)
  
 The only mandatory command-line arguments are the names of the files
  to be processed.  Wildcards are allowed (e.g. *.tex will process all
  of the TeX files.)
  
 The options are
   --help                print this help
   --version             version number
   --keep                keep original file, with ".orig" appended to name
   --padding=i           add an "i" point border around box (default is 3)
 
END
}


