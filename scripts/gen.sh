set -e

readonly SCRIPTROOT=$( cd $( dirname $0 ); pwd )
readonly PROJROOT=$( cd $SCRIPTROOT/..; pwd )

cd $PROJROOT

expr=$(stack runghc -- test/Gen.hs $1 $2 | sed  's/e+*/\*10\^/g')

echo '==========================='
echo Value: $2
echo '==========================='
echo $expr
echo '==========================='
echo Result: $(bc -l <<< $expr)
echo '==========================='
