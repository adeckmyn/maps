#!/bin/sh
for i in *.line
do
cat $i | tr -s ' \t' '\012' | grep -n EOR | \
awk -F: '{if($1-p>m)m=$1-p;p=$1} \
	END{print NR,m}' > ${i}stats
done

for i in *.gon
do
cat $i | sed 's/^[ 	]//' | tr -s ' 	' '\012' | \
awk '{if ($0!="EOR") {nw++} else {nr++; if (nw>maxnw) maxnw=nw; nw=0}}
END{print nr,maxnw}' > ${i}stats
done
