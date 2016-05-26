InitialTime=5000
MaxTime=40000
TimeStep=5000
ProblemSize=20
Iterations=10
MutationRate=0.25
MutationRange=0.20
Model=mas_skel

for ((t=$InitialTime; t<=$MaxTime; t+=$TimeStep)); do
    echo -n $t >> result.txt
    echo -n $'\t' >> result.txt
    for ((iter=0; iter<$Iterations; iter++)); do
        ./emas --time $t --genetic_ops traffic_ops --model $Model --problem_size $ProblemSize --mutation_rate $MutationRate --mutation_range $MutationRange
        echo -n $'\t' >> result.txt
     done
     echo "" >> result.txt
done
