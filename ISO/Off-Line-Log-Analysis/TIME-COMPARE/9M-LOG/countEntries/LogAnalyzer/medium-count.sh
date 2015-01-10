Start=$(date +"%s")
echo "Start time : $Start" > time.txt

sh logAnalyzer-3args.sh /home/xiaohe/UIUC-WorkSpace/DATA/ldcc4Monpoly

End=$(date +"%s")
echo "Finish time : $End" >> time.txt
Diff=$(( $End - $Start ))
echo "\nIt took $Diff seconds to count all the entries in the 9M log ldcc4Monpoly" >> time.txt
