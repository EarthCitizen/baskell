readonly SCRIPTROOT=$( cd $( dirname $0 ); pwd )
readonly PROJROOT=$( cd $SCRIPTROOT/..; pwd )

source $SCRIPTROOT/rainbow.sh

# Without the trap, the terminal gets messed up
# and displays escape codes for the arrow keys.
# Maybe due to the rainbow library.
trap exit INT

function check_failure() {
    if [ "$1" -ne "0" ]
    then
        echored 'bc error:'
        echo "$2"
        exit 1
    fi
}

fail=$(echored 'Fail')
pass=$(echogreen 'Pass')

fail_count=0
pass_count=0

exit_code=0

test_iterations=3

for depth in {1..12}
do
    for total in -100 -101.1 555555 505.505 7 700.700 3 3.33333 100 101.0 0 1 1.2 2 2.999 999 999.909 12345234 12345234.0001
    do
        for test_iter in $(eval echo {1..$test_iterations})
        do
            echo "Testing depth $depth and value $total [$test_iter of $test_iterations]"
            expr="$(bash $SCRIPTROOT/rungen.sh $depth $total)"
            # echo '======================================'
            # echo "$expr"
            # echo '======================================'
            result=$(bc -l <<< "$expr")
            # echo '======================================'
            # echo "$result"
            # echo '======================================'
            result_exit=$?
            check_failure $result_exit "$result"
            compare=$(bc -l <<< "($result - $total) < 0.0001")
            compare_exit=$?
            check_failure $compare_exit "$compare"
            # echo "$result == $total"
            if [ "$compare" -ne "1" ]
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
