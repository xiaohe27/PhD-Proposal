Start=$(date +"%s")
echo "Start time : $Start" > time.txt

/home/xiaohe/SW/monpoly-1.1.2/monpoly -sig sig -formula insert.mfotl -negate -log /DATA/ldcc4Monpoly_BaseExecTime >> time.txt

End=$(date +"%s")
echo "\nFinish time : $End" >> time.txt
Diff=$(( $End - $Start ))
echo "\nIt took Monpoly (opt) $Diff seconds to find the single violation (the last entry) in the 9M log file!" >> time.txt
