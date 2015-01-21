Formula=$2
Out=time$1.txt
Start=$(date +"%s")
echo "Start time : $Start" > $Out

/home/xiaohe/SW/offline-log-analysis/existingApp/monpoly-1.1.2/monpoly -sig insert.sig -formula $Formula -negate -log /home/xiaohe/workspace/DATA/MeasureBaseTime/ldcc4Monpoly_buggy >> $Out

End=$(date +"%s")
echo "Finish time : $End" >> $Out
Diff=$(( $End - $Start ))
echo "\nIt took Monpoly (opt) $Diff seconds to parse all the events in the 9M log file!" >> $Out
