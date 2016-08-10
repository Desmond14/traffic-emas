InitialTime=18000
MaxTime=30000
TimeStep=10000
ProblemSize=20
Iterations=1
MutationRate=0.20
MutationRange=0.20
Model=mas_sequential

echo -n $InitialTime >> result.txt
echo -n $'\t' >> result.txt
for ((iter=0; iter<$Iterations; iter++)); do
    ./benchmark --time $InitialTime --genetic_ops traffic_ops --model $Model --problem_size $ProblemSize --mutation_rate $MutationRate --mutation_range $MutationRange --cars_number 25 --randomization_chance 0.01
    echo -n $'\t' >> result.txt
 done
 echo "" >> result.txt
