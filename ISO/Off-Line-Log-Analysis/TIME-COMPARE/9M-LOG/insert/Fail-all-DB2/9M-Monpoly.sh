Start=$(date +"%s")
echo "Start time : $Start" > time.txt

/home/xiaohe/SW/offline-log-analysis/existingApp/monpoly-1.1.2/monpoly -sig insert.sig -formula insert.mfotl -negate -log /home/xiaohe/SW/offline-log-analysis/ldcc4Monpoly > OutputNumOfInsert2DB2EventsIn9MLog.txt

End=$(date +"%s")
echo "Finish time : $End" >> time.txt
Diff=$(( $End - $Start ))
echo "\nIt took $Diff seconds to output the insert 2 db2 events in the 9M log file!" >> time.txt
