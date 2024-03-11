#!/bin/bash
# -------------------------------------------------------------------------------------
# Script to run mypy with reporting thru reviewdog, called from .pre-commit-config.yaml
# -------------------------------------------------------------------------------------
# Variables:
# REVIEWDOG_REPORTER can be set to github-pr-check or github-pr-review for GitHub CI
# GITHUB_BASE_REF is the base branch for the pull request, defaults to origin/master
# GITHUB_REF is the current branch, defaults to HEAD, for diffing against the base branch
# https://docs.github.com/en/actions/learn-github-actions/variables#default-environment-variables
# -------------------------------------------------------------------------------------
# Usage:
# source reviewdog.sh  # Run a sample command to test reviewdog_mypy and reviewdog_pytype
# ./reviewdog.sh mypy   # Run mypy with reviewdog <changed_files>
# ./reviewdog.sh pytype # Run pytype with reviewdog <changed_files>
# -------------------------------------------------------------------------------------
# Example to test GitHub CI with reviewdog in a pull request (PR) check or review:
# REVIEWDOG_REPORTER=github-pr-check ./reviewdog.sh mypy <changed_files>
# REVIEWDOG_REPORTER=github-pr-review ./reviewdog.sh pytype <changed_files>
# -------------------------------------------------------------------------------------
# To run this script in GitHub CI, add the following to your .github/workflows/*.yml file:
# jobs:
#   reviewdog-mypy:
#     runs-on: ubuntu-latest
#     steps:
#       - uses: actions/checkout@v4
#       - uses: actions/setup-python@v4
#       - run: reviewdog.sh mypy <changed_files>
# -------------------------------------------------------------------------------------

github_debug() {
    if [ -n "$GITHUB_JOB" ]; then
        echo "::debug::$*"
    fi
}

git_diff_cmd() {  # Run git diff with the right range
    GIT_DIFF_RANGE="${GITHUB_BASE_REF:-origin/master}..${GITHUB_REF:-HEAD}"
    git diff --ignore-space-change "$GIT_DIFF_RANGE" >.git/diff
    EXIT_CODE=$?
    # cat .git/diff  # Uncomment to debug the diff
    return $EXIT_CODE
}

check_diff() {
    git_diff_cmd
    if [ $EXIT_CODE -ge 128 ]; then
        rm .git/index
        git reset
        git_diff_cmd
    fi
    if [ $EXIT_CODE -ne 0 ]; then
        echo "Error: git diff failed with exit code $EXIT_CODE"
        exit $EXIT_CODE
    fi
}

install_reviewdog() {  # Install reviewdog
    if ! type -p reviewdog >/dev/null; then
        echo "Reviewdog is not installed, trying to install it to $PWD/.git"
        curl -sfL https://raw.githubusercontent.com/reviewdog/reviewdog/master/install.sh |
            sh -s -- -b "$PWD/.git"
    fi
}

reviewdog_mypy() {  # Run mypy with reviewdog
    install_reviewdog
    mypy_version=$(mypy --version) || pip install mypy
    check_diff
    github_debug "$mypy_version"
    mypy "$@" |
        reviewdog -name=mypy -reporter="${REVIEWDOG_REPORTER:-local}" \
            -efm "%E%f:%l: %t%*[^:]: %m" \
            -efm "%E%f: %t%*[^:]: %m" \
            -efm "%C%m" \
            -diff="cat .git/diff" -filter-mode file -level=warning -fail-on-error
}

reviewdog_pytype() {  # Run pytype with reviewdog
    install_reviewdog
    pytype_version=$(pytype --version) || pip install pytype
    check_diff
    github_debug "$pytype_version"
    # shellcheck disable=SC2068
    pytype $@ |
        reviewdog -name=pytype -reporter="${REVIEWDOG_REPORTER:-local}" \
            -efm "%EFile %f, line %l, in %m" \
            -efm "%C%m" \
            -diff="cat .git/diff" -filter-mode nofilter -level=warning -fail-on-error
    # Kept until the reviewdog github-pr-check reporter has successfully
    # reported the pytype errors in the GitHub CI pull request code review:
    # shellcheck disable=SC2068
    python3/tests/pytype_reporter.py $@ || exit $?
}

PATH="$PWD/.git:$PATH"
if [[ "${BASH_SOURCE[0]}" == "${0}" ]]; then
    command="$1"
    shift  # pre-commit may pass the changed files to check
    if [ -n "$GITHUB_JOB" ]; then  # Running in GitHub CI, enable the GitHub reporter
        export REVIEWDOG_REPORTER=github-pr-check
    fi
    python_scripts="$([ $# == 0 ] || file "$@" | grep "Python script" | cut -d: -f1)"
    if [ "$command" == "mypy" ]; then
        reviewdog_mypy   "$python_scripts" || exit $?
    elif [ "$command" == "pytype" ]; then
        reviewdog_pytype "$python_scripts" || exit $?
    fi
else
    # When the script is sourced, run a sample command to test reviewdog_mypy:
    REVIEWDOG_BASE_REF=57508663329e838f51167faf0a3a05f8eac352b8...5e6c780d \
        reviewdog_mypy   scripts/examples/python/XenAPI || echo Exit code: $?
    REVIEWDOG_BASE_REF=57508663329e838f51167faf0a3a05f8eac352b8...5e6c780d \
        reviewdog_pytype scripts/examples/python/XenAPI || echo Exit code: $?
fi
