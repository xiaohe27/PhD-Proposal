Start=$(date +"%s")
echo "Start time : $Start" > time.txt

/home/xiaohe/SW/offline-log-analysis/existingApp/monpoly-1.1.2/monpoly -sig insert.sig -formula violate.mfotl -negate -log /home/xiaohe/workspace/DATA/MeasureBaseTime/ldcc4Monpoly_buggy

End=$(date +"%s")
echo "Finish time : $End" >> time.txt
Diff=$(( $End - $Start ))
echo "\nIt took Monpoly (opt) $Diff seconds to parse all the events in the 9M log file!" >> time.txt
