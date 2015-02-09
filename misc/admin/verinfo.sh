#!awk -f
# extract version essentials
# Denis RAUX LIX 2015
#
function max(ar,a,b) {if(ar[a]>ar[b]) return a;else return b;}
$1 == "?" { next; }
$1 ~/^[MA]/ {cnt[$2]++; mfiles[$2] = $NF"\n"mfiles[$2] }
$1 ~ /[0-9]+/ { cnt[$1]++; files[$1] = $NF"\n"files[$1] } 
END {
	base=0;
	for (x in cnt) { base=max(cnt,x,base); }
	print "\n**** base revision " base " ****";
	print "other files";
	delete files[base];
	for (f in files) { print f ":\n" files[f]}
	print "modified files";
	for (f in mfiles) { print f ":\n" mfiles[f]}
}