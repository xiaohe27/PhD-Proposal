Start=$(date +"%s")
echo "Start time : $Start" > time.txt

/home/xiaohe/UIUC-WorkSpace/HackedMonpoly/monpoly_c -sig sig -formula insert.mfotl -negate -log /home/xiaohe/UIUC-WorkSpace/DATA/ldcc4Monpoly >> time.txt

End=$(date +"%s")
echo "\nFinish time : $End" >> time.txt
Diff=$(( $End - $Start ))
echo "\nIt took Monpoly-c (opt) $Diff seconds to count the number of log entries in the 9M log file!" >> time.txt
