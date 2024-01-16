#!/usr/bin/env python
import os
import re
import selectors
import shlex
import sys
from logging import INFO, basicConfig, info
from subprocess import PIPE, Popen
from typing import Dict, List, TextIO, Tuple

import pandas as pd  # type: ignore[import]
from toml import load


def generate_github_annotation(match: re.Match[str], branch_url: str) -> Tuple[str, Dict[str, str]]:
    lineno = match.group(2)
    code = match.group(5)
    func = match.group(3)
    msg = match.group(4)
    assert isinstance(msg, str)
    msg_splitpos = msg.find(" ", 21)
    file = match.group(1)
    linktext = os.path.basename(file).split(".")[0]
    source_link = f"[`{linktext}`]({branch_url}/{file}#L{lineno})"
    row = {
        "Location": source_link,
        "Function": f"`{func}`",
        "Error code": code,
        "Error message": msg[:msg_splitpos] + "<br>" + msg[msg_splitpos + 1 :],
        "Error description": "",
    }
    # https://docs.github.com/en/actions/using-workflows/workflow-commands-for-github-actions#setting-an-error-message
    return f"::error file={file},line={lineno},title=pytype: {code}::{msg}", row


def filter_line(line, row):
    if line.startswith("For more details, see"):
        row["Error code"] = f"[{row['Error code']}]({line[22:]})"
        return " " + line[22:]
    if not row["Error description"]:
        row["Error description"] = line.lstrip()
    else:
        row["Error description"] += " " + line.lstrip()
    return ", " + line


def skip_uninteresting_lines(line: str) -> bool:
    if not line or line[0] == "/" or line.startswith("FAILED:"):
        return True
    if line[0] == "[":
        pos = line.rfind(os.getcwd())
        printfrom = pos + len(os.getcwd()) + 1 if pos > 0 else line.index("]") + 2
        info("PROGRESS: " + line[1:].split("]")[0] + ": " + line[printfrom:])
        return True
    if line.startswith("ninja: "):
        line = line[7:]
    return bool(
        (
            line.startswith("Entering")
            or line.startswith("Leaving")
            or line.startswith("Computing")
            or line.startswith("Analyzing")
        )
    )


def run_pytype(command: List[str], branch_url: str, errorlog: TextIO, results):
    info(" ".join(shlex.quote(arg) for arg in command))
    # When run in tox, pytype dumps debug messages to stderr. Point stderr to /dev/null:
    popen = Popen(command, stdout=PIPE, stderr=PIPE, universal_newlines=True)
    assert popen.stdout and popen.stderr
    error = ""
    row = {}  # type: dict[str, str]
    sel = selectors.DefaultSelector()
    sel.register(popen.stdout, selectors.EVENT_READ)
    sel.register(popen.stderr, selectors.EVENT_READ)
    ok = True
    while ok:
        for key, _ in sel.select():
            line = key.fileobj.readline()  # type: ignore
            if not line:
                ok = False
                break
            if key.fileobj is popen.stderr:
                print(f"pytype: {line}", end="", file=sys.stderr)
                continue
            line = line.rstrip()
            if skip_uninteresting_lines(line):
                continue
            info(line)
            if row:
                if line == "" or line[0] == " " or line.startswith("For more details, see"):
                    if line:
                        error += filter_line(line, row)
                    continue
                errorlog.write(
                    error
                    + " (you should find an entry in the pytype results with links below)\n"
                )
                results.append(row)
                row = {}
                error = ""
            match = re.match(
                r'File ".*libs/([^"]+)", line (\S+), in ([^:]+): (.*) \[(\S+)\]', line
            )
            if match:
                error, row = generate_github_annotation(match, branch_url)
    if popen.stdout:
        popen.stdout.close()
    popen.wait()
    return popen.returncode, results


def run_pytype_and_parse_annotations(xfail_files: List[str], branch_url: str):
    """Send pytype errors to stdout and return results as pandas table

    Args:
        xfail_files (List[str]): list of files to exclude from pytype checks
        branch_url (str): Base URL of the git branch for file links in github annotations
    """
    base_command = [
        "pytype",
        "-j",
        "auto",
    ]
    if xfail_files:
        exclude_command = ["--exclude", " ".join(xfail_files)]
    else:
        exclude_command = []

    err_code, results = run_pytype(base_command + exclude_command, branch_url, sys.stderr, [])
    if err_code or len(results):
        return err_code if err_code > 0 else len(results), results
    for xfail_file in xfail_files:
        err_code, results = run_pytype(base_command + [xfail_file], branch_url, sys.stdout, results)
        if err_code == 0:
            print("No errors in", xfail_file)
    return err_code or len(results), results

def to_markdown(me, fp, returncode, results, branch_url):
    mylink = f"[{me}]({branch_url}/{me}.py)"
    pytype_link = "[pytype](https://google.github.io/pytype)"
    if len(results) or returncode:
        fp.write(f"\n#### {mylink} reports these {pytype_link} error messages:\n")
        fp.write(pd.DataFrame(results).to_markdown())
    else:
        fp.write(f"\n#### Congratulations, {mylink} reports no {pytype_link} errors.\n")
    fp.write("\n")


def setup_and_run_pytype_action(script_name: str):
    config = load("pyproject.toml")
    pytype = config["tool"].get("pytype")
    xfail_files = pytype.get("xfail", []) if pytype else []
    repository_url = config["project"]["urls"]["repository"].strip(" /")
    filelink_baseurl = repository_url + "/blob/master"

    # When running as a GitHub action, we want to use URL of the fork with the GitHub action:
    server_url = os.environ.get("GITHUB_SERVER_URL", None)
    repository = os.environ.get("GITHUB_REPOSITORY", None)
    if server_url and repository:
        # https://github.com/orgs/community/discussions/5251 only set on Pull requests:
        branch = os.environ.get("GITHUB_HEAD_REF", None) or os.environ.get("GITHUB_REF_NAME", None)
        filelink_baseurl = f"{server_url}/{repository}/blob/{branch}"
    ret_code, results = run_pytype_and_parse_annotations(xfail_files, filelink_baseurl)

    # Write the panda table to a markdown output file:
    summary_file = os.environ.get("GITHUB_STEP_SUMMARY", None)
    if summary_file:
        with open(summary_file, "w", encoding="utf-8") as fp:
            to_markdown(script_name, fp, ret_code, results, filelink_baseurl)
    else:
        to_markdown(script_name, sys.stdout, ret_code, results, filelink_baseurl)


if __name__ == "__main__":
    script_basename = os.path.basename(__file__).split(".")[0]
    basicConfig(format=script_basename + ": %(message)s", level=INFO)
    sys.exit(setup_and_run_pytype_action(script_name = script_basename))
