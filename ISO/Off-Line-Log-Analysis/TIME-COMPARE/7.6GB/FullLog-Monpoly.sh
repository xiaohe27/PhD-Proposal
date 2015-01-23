Out=timeForCompleteLog-Mon-Siebel.txt
Start=$(date +"%s")
echo "Start time : $Start" > $Out

/home/xiaohe/SW/offline-log-analysis/existingApp/monpoly-1.1.2/monpoly -sig insert.sig -formula insert.mfotl -negate -log /home/xiaohe/workspace/DATA/MeasureBaseTime/ldccComplete_MonpolyStyle_addMore >> $Out

End=$(date +"%s")
echo "Finish time : $End" >> $Out
Diff=$(( $End - $Start ))
echo "\nIt took Monpoly (opt) $Diff seconds to parse all the events in the 7.6 GB log file!" >> $Out
