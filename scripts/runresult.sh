set -e

readonly SCRIPTROOT=$( cd $( dirname $0 ); pwd )
readonly PROJROOT=$( cd $SCRIPTROOT/..; pwd )

cd $PROJROOT

expr=$(bash $PROJROOT/scripts/rungen.sh $1 $2)

echo '==========================='
echo Value: $2
echo '==========================='
echo $expr
echo '==========================='
echo Result: $(bc -l <<< $expr)
echo '==========================='
