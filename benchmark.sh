InitialTime=18000
MaxTime=30000
TimeStep=10000
ProblemSize=20
Iterations=10
MutationRate=0.20
MutationRange=0.20
Model=mas_skel

echo -n $InitialTime >> result.txt
echo -n $'\t' >> result.txt
for ((iter=0; iter<$Iterations; iter++)); do
    ./benchmark --time $InitialTime --genetic_ops traffic_ops --model $Model --problem_size $ProblemSize --mutation_rate $MutationRate --mutation_range $MutationRange
    echo -n $'\t' >> result.txt
 done
 echo "" >> result.txt
