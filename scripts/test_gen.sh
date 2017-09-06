readonly SCRIPTROOT=$( cd $( dirname $0 ); pwd )
readonly PROJROOT=$( cd $SCRIPTROOT/..; pwd )

source $SCRIPTROOT/rainbow.sh

# Without the trap, the terminal gets messed up
# and displays escape codes for the arrow keys.
# Maybe due to the rainbow library.
trap exit INT

fail=$(echored 'Fail')
pass=$(echogreen 'Pass')

fail_count=0
pass_count=0

exit_code=0

test_iterations=3

for depth in {1..12}
do
    for total in -100 555555 7 3 100 0 1 2 999 12345234
    do
        for test_iter in $(eval echo {1..$test_iterations})
        do
            echo "Testing depth $depth and value $total [$test_iter of $test_iterations]"
            result=$(bash rungen.sh $depth $total | bc)
            if [ "$result" -ne "$total" ]
            then
                expected=$(echored ": expected $total but got $result")
                printf " ${fail}${expected}\n"
                fail_count=$(($fail_count + 1))
                exit_code=1
            else
                printf " ${pass}\n"
                pass_count=$(($pass_count + 1))
            fi
        done
    done
done

total_tests=$(($pass_count + $fail_count))

printf "\n==========================================\n\n"
printf "Total Tests: $total_tests\n"
printf "Total ${pass}: ${pass_count}\n"
printf "Total ${fail}: ${fail_count}\n"

exit $exit_code
