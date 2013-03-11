#!/bin/bash

export FIX_DATE=`pwd`/../fix-date.sh
[ -x $FIX_DATE ] || exit 1
# xen-api fails a "git fsck" because it has bad timezone data in commit timestamps::
# '@ <Jonathan Ludlam <Jonathan.Ludlam@eu.citrix.com> > 1255377615 +0100'
git filter-branch -f --commit-filter '
  echo "id = $GIT_COMMIT committer date = $GIT_COMMITTER_DATE" >> /tmp/foo
  echo "id = $GIT_COMMIT author date = $GIT_AUTHOR_DATE" >> /tmp/foo
  GIT_COMMITTER_DATE2=$(${FIX_DATE} "${GIT_COMMITTER_DATE}")
  GIT_AUTHOR_DATE2=$(${FIX_DATE} "${GIT_AUTHOR_DATE}")
  if [ "${GIT_COMMITTER_DATE}" != "${GIT_COMMITTER_DATE2}" ];
  then
     echo "Rewriting ${GIT_COMMITTER_DATE} > ${GIT_COMMITTER_DATE2}" >> /tmp/foo
     export GIT_COMMITTER_DATE="${GIT_COMMITTER_DATE2}"
  fi
  if [ "${GIT_AUTHOR_DATE}" != "${GIT_AUTHOR_DATE2}" ];
  then
     echo "Rewriting ${GIT_AUTHOR_DATE} > ${GIT_AUTHOR_DATE2}" >> /tmp/foo
     export GIT_AUTHOR_DATE="${GIT_AUTHOR_DATE2}"
  fi
  git commit-tree "$@";
' --tag-name-filter cat -- --all
rm -rf .git/refs/original/ && git reflog expire --all &&  git gc --aggressive --prune
