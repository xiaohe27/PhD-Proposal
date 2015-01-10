Start=$(date +%s)
echo "Start time : $Start" > time.txt

java -cp /home/xiaohe/Projects/LogAnalyzer/production:$CLASSPATH fsl.uiuc.Main insert.sig insert.fl /home/xiaohe/SW/offline-log-analysis/ldcc4Monpoly >> time.txt

End=$(date +%s)
echo "\nFinish time : $End" >> time.txt
Diff=$(( $End - $Start ))
echo "\nIt took my log analyzer $Diff seconds to count the number of log entries in the 9M log" >> time.txt
