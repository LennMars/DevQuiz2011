for n in ($seq 1 5000)
do
./camlprog.opt $n >> ./answers/$n.txt
done