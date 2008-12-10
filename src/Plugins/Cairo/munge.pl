#!/usr/local/bin/perl
# use this program to generate interface informations for the cairo library
# to be used in the glue code
# output: list of declarations for proxy function pointers, macros to link symbols, converted source for cairo_renderer.cpp

# by M. Gubinelli, 12/2008

use strict;

my $acc = "";
my %b;


my $filename = shift;

while ($filename) {
  open (FILE, "<", $filename)  or  die "Failed to read file $filename : $! \n";
  my $whole_file;
  {
    local $/;
    $whole_file = <FILE>;
  }
  close(FILE);

  while ($whole_file =~ m#cairo_public([^;]*)#sg) {
    $_ = $1 ;
    s/[\n\t ]+/ /g;
    s/^ //;
    m/ (cairo_[a-z_]*)/;
    my $l = $1;  
    s/ (cairo_[a-z_]*)/ \(\*tm_\1\)/;
    #m/(tm_cairo_[a-z_]*)/;  
    $b{$l} = $_;
    $acc .= $l . " --> " . $_ . ";\n";
  }

  $filename = shift;
}

my $decl;
my $source_file;
$filename = "cairo_renderer.cpp";
open (FILE, "<", $filename)  or  die "Failed to read file $filename : $! \n";
{
  local $/;
  $source_file = <FILE>;
}
close(FILE);

#print $source_file;

my $acc_int;
my $acc_link;
my $acc_dynlink;


foreach $decl (keys %b) {
  if ($source_file =~ /$decl/) {
    #    print $decl . ";\n";
    $acc_int .= "extern " . $b{$decl} . ";\n";
    $acc_link .= "tm_" . $decl . " = " . $decl . ";\n";
    $acc_dynlink .= "CAIRO_LINK(" . $decl . ", tm_" . $decl . ");\n";
};
}

print $acc_int . "\n" ;
print $acc_link . "\n" ;
print $acc_dynlink . "\n" ;

foreach $decl (keys %b) {
  $source_file =~ s/$decl/tm_$decl/g;
}

$filename = "cairo_renderer.cpp.transated";
open (FILE, ">", $filename)  or  die "Failed to read file $filename : $! \n";
print FILE $source_file;
close(FILE);



#$acc ~= s/