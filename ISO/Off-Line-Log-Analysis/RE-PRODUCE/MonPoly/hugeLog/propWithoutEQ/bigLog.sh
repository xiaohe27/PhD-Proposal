Start=$(date +"%s")
echo "Start time : $Start" > time.txt

./csv2log 4 ldcc.csv | existingApp/rv13-experiments/monpoly/monpoly -sig sig -formula insert.mfotl -negate > insert2HugeLog.txt

End=$(date +"%s")
echo "Finish time : $End" >> time.txt
Diff=$(( $End - $Start ))
echo "\nIt took $Diff seconds to analyze the insert property of the huge log ldcc.csv" >> time.txt
