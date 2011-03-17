
BINS=`diff -r i386/distr/TeXmacs.app ppc/distr/TeXmacs.app | grep Binary | awk '{print($3)}'`

for BIN in $BINS
do
A=${BIN#*/}
lipo -create -output $A ppc/$A i386/$A x86_64/$A
done

