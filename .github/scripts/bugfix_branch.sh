# bash
BRANCH_NAME=$1
echo "branch name is: $BRANCH_NAME"
if [[ $BRANCH_NAME =~ ^fix/ ]];then
  echo "Bug fix can only run on branches prefixed with: fix/, but is running on branch: $"
  exit 1
fi

prefix=$(echo $BRANCH_NAME | cut -d "/" -f 1)
echo "branch name prefix part is: $prefix"
version=$(echo $BRANCH_NAME | cut -d "/" -f 2)
echo "branch name version part is: $version"

if [ $(git tag -l $version) ]; then
  echo "Running bug fix on for version $version."
else
  echo "Running bug fix on non existing version: $BRANCH_NAME"
  exit 1
fi
