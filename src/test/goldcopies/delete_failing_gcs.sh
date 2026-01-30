# Used to reset gold copies once you make a change that is supposed to change them
# and you manually conclude that the change is fine.
# Just run ./src/test/goldcopies/delete_failing_gcs.sh
# Ideally the things you are deleted are in git beforehand.

SCRIPT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"
(
cd $SCRIPT_DIR
cat ./gc-errors-files  | xargs -o -I {} rm -i ./gc-files/{}
)
