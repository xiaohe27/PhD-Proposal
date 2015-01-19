Out="time4MonpolyC-ocamlc.txt"

Start=$(date +"%s")
echo "Start time : $Start" > $Out

/home/xiaohe/UIUC-WorkSpace/HackedMonpoly/monpoly_c -sig sig -formula insert.mfotl -negate -log /home/xiaohe/UIUC-WorkSpace/DATA/ldcc4Monpoly >> $Out

End=$(date +"%s")
echo "\nFinish time : $End" >> $Out
Diff=$(( $End - $Start ))
echo "\nIt took Monpoly-c (compiled by ocamlc) $Diff seconds to count the number of log entries in the 9M log file!" >> $Out
