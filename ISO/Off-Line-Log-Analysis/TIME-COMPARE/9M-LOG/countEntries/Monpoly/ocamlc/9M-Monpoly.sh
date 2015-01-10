Start=$(date +"%s")
echo "Start time : $Start" > time.txt

/home/xiaohe/workspace/monpoly-m/monpoly_c -sig ../sig -formula ../insert.mfotl -negate -log /home/xiaohe/SW/offline-log-analysis/ldcc4Monpoly >> time.txt

End=$(date +"%s")
echo -e "\nFinish time : $End" >> time.txt
Diff=$(( $End - $Start ))
echo -e "\nIt took $Diff seconds to count the number of log entries in the 9M log file!" >> time.txt
