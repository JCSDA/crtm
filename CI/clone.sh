#!/bin/bash 

# clones specific branch of a given repo
# if branch does not exist clones develop
 
git_user=$1
git_token=$2
org_repo_name=$3
branch_name=$4
save_name=$5
save_dir=$6
branch_name_default=$7

repo_name="$(cut -d'/' -f2 <<<$org_repo_name)"
org_name="$(cut -d'/' -f1 <<<$org_repo_name)"


if [ "${branch_name}" = "develop" ] || [ "${branch_name}" = "master" ]; then

  echo "merging into develop or master"
  branch_name_clone=${branch_name_default}
  org_repo_name_clone=${org_name}
  echo "==============================================================================="
  echo "  Merge into develop or master"
  echo "  Clone " $org_repo_name_clone "/" $repo_name
  echo "==============================================================================="
  git clone -b $branch_name_clone https://$git_user:$git_token@github.com/$org_repo_name_clone/$repo_name $save_dir/$save_name

else

  # check jcsda-internal for branch
  git ls-remote --heads --exit-code https://$git_user:$git_token@github.com/jcsda-internal/$repo_name $branch_name
  exit_code_internal=$?

  git ls-remote --heads --exit-code https://$git_user:$git_token@github.com/jcsda/$repo_name $branch_name
  exit_code=$?

  # if branch exists in both jcsda-internal and jcsda it will clone jcsda-internal
  # it searches in jcsda only if org_name is jcsda

  if test "${exit_code_internal}" == "0"; then
    echo ${branch_name} " branch found in jcsda-internal"
    branch_name_clone=${branch_name}
    org_repo_name_clone="jcsda-internal"

  # search in jcsda only if cloning from jcsda
  elif [ "${exit_code}" == "0" ] && [ "${org_name}" == "jcsda" ]; then
    echo ${branch_name} " branch found in jcsda"
    branch_name_clone=${branch_name}
    org_repo_name_clone="jcsda"

  else
    echo ${branch_name} " branch does not exist in jcsda-internal or jcsda"
    echo "clone " ${branch_name_clone}
    branch_name_clone=${branch_name_default}
    org_repo_name_clone=${org_name}
  fi
  echo "==============================================================================="
  echo "Clone " $org_repo_name_clone "/" $repo_name " branch " $branch_name_clone
  echo "==============================================================================="
  git clone --depth 1 -b $branch_name_clone https://$git_user:$git_token@github.com/$org_repo_name_clone/$repo_name $save_dir/$save_name

fi
