InitialTime=2000
MaxTime=18000
TimeStep=2000
ProblemSize=20
Iterations=30
Model=mas_skel

for ((t=$InitialTime; t<=$MaxTime; t+=$TimeStep)); do
    echo -n $t >> result.txt
    echo -n $'\t' >> result.txt
    for ((iter=0; iter<$Iterations; iter++)); do
        ./emas --time $t --genetic_ops traffic_ops --model $Model --problem_size $ProblemSize
        echo -n $'\t' >> result.txt
     done
     echo "" >> result.txt
done
