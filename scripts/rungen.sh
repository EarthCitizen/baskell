set -e

readonly SCRIPTROOT=$( cd $( dirname $0 ); pwd )
readonly PROJROOT=$( cd $SCRIPTROOT/..; pwd )

cd $PROJROOT

stack runghc -- $PROJROOT/test/Gen.hs $1 $2 | sed  's/e+*/\*10\^/g'
