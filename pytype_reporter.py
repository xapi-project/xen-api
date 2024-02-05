#!/usr/bin/env python
"""GitHub action workflow Runner for pytype which works also locally without GitHub"""
import re
import selectors
import shlex
from logging import DEBUG, INFO, basicConfig, debug, info, fatal
from os import environ as env, getcwd
from os.path import basename
from subprocess import check_output, PIPE, Popen  # nosec:B404
from sys import argv, exit as sys_exit, stderr, stdout
from typing import Dict, List, TextIO, Tuple, TYPE_CHECKING
from warnings import catch_warnings, simplefilter

import toml

if TYPE_CHECKING:
    from typing import FileDescriptorLike

Config = dict[str, str]
Info = List[dict]
Ret = Tuple[int, Info]

DEFAULT_BRANCH_NAME = "main"
MORE = "For more details, see "
TRACE = "Called from (traceback):"

# The DeprecationWarning from import pandas is not relevant for this script:
with catch_warnings():
    simplefilter(action="ignore", category=DeprecationWarning)
    import pandas as pd  # type: ignore[import]


def populate_error_dict(config, message, filename, lineno, code) -> dict[str, str]:
    """Create a dictionary with relevant information about the error.

    :param config: The configuration dictionary of the script.
    :param message: The error message.
    :param filename: The name of the file where the error occurred.
    :param lineno: The line number where the error occurred.
    :return: A dictionary containing the split error message information.

    It creates a source link for the error location.
    The error code is formatted as a link to the pytype error code documentation.
    Finally, it returns a dictionary with the following keys and values:
    - "Location": The source link for the error location.
    - "Error code": The formatted error code.
    - "Error message": The split error message with line breaks inserted.
    """
    message_split = message.find(" ", 21)
    link_text = basename(filename).split(".")[0]
    tree_url = config["tree_url"]
    source_link = f"[`{link_text}:{lineno}`]({tree_url}/{filename}#L{lineno})"
    part2 = message[message_split + 1 :].replace(". ", ".<br>")
    if code[0] != "[":
        code = f"[{code}](https://google.github.io/pytype/errors.html#{code})"
    return {
        "Location": source_link,
        "Error code": code,
        "Error message": message[:message_split] + "<br>" + part2,
    }


def github_error(config: Config, line: str) -> Tuple[str, Dict[str, str], str]:
    """GitHub error handler for pytype.

    :param config (Config): The configuration dictionary of the script
    :param line (str): The error message line.
    :returns Tuple[str, Dict[str, str]]: Tuple containing the GitHub error message,
    an error dictionary, and the filename.

    The function takes an error message line and the branch URL as input.
    It extracts relevant information from the error message line, such as
    the filename, line number, function name, error code, and error message.
    It then creates a source link for the error location and formats the error
    information into a dictionary. Finally, it returns a specially formatted
    GitHub error message and the error dictionary.

    The GitHub error message is formatted using the "::error" syntax, which
    GitHub interprets to create an error message and print the message with
    a yellow "Error:" to the log. This message creates a code annotation,
    which associates the message with the line in the file in the repository.
    GitHub can show an annotation in PR review.
    """
    match = re.match(r'File "([^"]+)", line (\S+), in ([^:]+): (.*) \[(\S+)\]', line)
    if match:
        filename = match.group(1).replace(getcwd() + "/", "")
        lineno = match.group(2)
        func = match.group(3)
        message = match.group(4)
        code = match.group(5)
    else:
        match = re.match(r'File "([^"]+)", line (\S+): (.*) \[(\S+)\]', line)
        if match:
            filename = match.group(1).replace(getcwd() + "/", "")
            lineno = match.group(2)
            func = ""
            message = match.group(3)
            code = match.group(4)
        else:
            return "", {}, ""

    # Discard error messages configured in pyproject.toml:
    for ignore_string in config["discard_messages_matching"]:
        if re.match(ignore_string, message):
            return "", {}, ""

    error_dict = populate_error_dict(config, message, filename, lineno, code)
    if func:
        error_dict["Function"] = f"`{func}`"
    else:
        error_dict["Function"] = "syntax<br>error"
    typ = "notice" if "ignored" in code or "unsupported-operands" in code else "error"
    if code in ["attribute-error"]:
        typ = "warning"
    # Return a specially formatted annotation for stdout which GitHub interprets to
    # create an error message and print the message with a yellow "Error:" to the log.
    # This message create an code annotation, which can associate the message with the
    # line in the file in the repository and GitHub can show an annotation in PR review:
    # https://docs.github.com/en/actions/using-workflows/workflow-commands-for-github-actions#setting-an-error-message
    title = f"pytype: {code}"
    return (
        f"::{typ} file={filename},line={lineno},title={title}::{message}",
        error_dict,
        filename,
    )


def extend_error_description(line: str, error: dict[str, str]) -> str:
    """Update the error dictionary using the passed output line.

    :param line (str): The line of output from pytype.
    :param error (dict): The error dictionary to update.
    :returns str: message string for this script's output messages
    """
    if line.startswith(MORE):
        return ""
    if "Error description" in error:
        error["Error description"] += "<br>" + line.lstrip()
    else:
        error["Error description"] = line.lstrip()
    return ", " + line


def skip_uninteresting_lines(line: str) -> bool:
    """Skip uninteresting lines during pytype output processing.

    This function takes a line of text from pytype output and determines
    whether it is an uninteresting line that should be skipped.

    :param line (str): The line of text from pytype output.
    :returns bool: True if the line is uninteresting and should be skipped.

    Examples:
    >>> skip_uninteresting_lines("Entering function foo")
    False
    >>> skip_uninteresting_lines("FAILED: Type-checking failed")
    True
    >>> skip_uninteresting_lines("[1/10] Analyzing module bar")
    True
    """
    if not line or line[0] == "/" or line.startswith("FAILED:"):
        return True
    if line[0] == "[":
        pos = line.rfind(getcwd())
        print_from = pos + len(getcwd()) + 1 if pos > 0 else line.index("]") + 2
        info("Progress: " + line[1:].split("]")[0] + ": " + line[print_from:])
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


def report_on(config: Config, log: TextIO, command: List[str], results: Info) -> Ret:
    """Run a pytype command with the given arguments and parse its output.

    :param branch_url: The URL of the branch for file links in GitHub annotations.
    :param log: The file-like object to write log messages.
    :param command: The command to run pytype.
    :param results: The list to store the parsed error results.
    :return: Tuple with the pytype return code and the parsed error results.

    Runs the pytype command with the given arguments and captures output.
    - It processes the output line by line, filtering out uninteresting lines and
      extracting error information.
    - For each error, it creates a log message and an error dictionary.
    - The log message is written to the log file, and the error dictionary
      is appended to the results list.
    - The function also handles grouping of log lines for GitHub annotations.
    """
    info(" ".join(shlex.quote(arg) for arg in command))
    # When run in tox, pytype dumps debug messages to stderr. Point stderr to /dev/null:
    with Popen(  # nosec:B603
        command, stdout=PIPE, stderr=PIPE, universal_newlines=True
    ) as popen:
        assert popen.stdout and popen.stderr  # nosec:B101
        return (popen.returncode or 0), parse_annotations(config, popen, log, results)


def readline(fileobj):
    # type: (FileDescriptorLike) -> str
    """Convince pytype that fileobj is of type FileDescriptorLike"""

    return fileobj.readline()


def handle_grouping(filename: str, last_filename: str, log: TextIO):
    """Group log lines for each given file name in the GitHub Actions workflow.

    :param filename: The current filename being processed.
    :param last_filename: The last filename that was processed.
    :param log: The file-like object to write log messages.

    Ensures that log lines are grouped together in the GitHub Actions workflow.
    """
    if log != stdout:
        if filename != last_filename:
            if last_filename:
                print("::endgroup::", file=log)
            if filename:
                print(f"::group::{filename}", file=log)
            last_filename = filename
    return filename


def parse_annotations(c: Config, popen: Popen[str], log: TextIO, results: Info) -> Info:
    """Parse the output of pytype and extract error information.

    :param config: Configuration dictionary for this script.
    :param popen: The Popen object representing the subprocess running pytype.
    :param log: The file-like object to write log messages.
    :param results: The list to store the parsed error results.
    :return dict: The parsed error results.

    Runs the pytype command with the given arguments and captures output.
    - It processes the output line by line, filtering out uninteresting lines and
      extracting error information.
    - For each error, it creates a log message and an error dictionary.
    - The log message is written to the log file, and the error dictionary
      is appended to the results list.
    - The function also handles grouping of log lines for GitHub annotations.
    """
    log_message = ""
    error_dict = {}  # type: dict[str, str]
    default_selector = selectors.DefaultSelector()
    default_selector.register(popen.stdout, selectors.EVENT_READ)
    default_selector.register(popen.stderr, selectors.EVENT_READ)
    ok = True
    last_filename = ""
    while ok:
        for key, _ in default_selector.select():
            line = readline(key.fileobj)
            if not line:
                ok = False
                break
            if key.fileobj is popen.stderr:
                print(f"pytype: {line}", end="", file=stderr)
                continue
            line = line.rstrip()
            if skip_uninteresting_lines(line):
                continue
            if (
                not line.startswith(MORE)
                and not line.startswith(TRACE)
                and not line.startswith("File ")
                and not line.startswith("ninja:")
            ):
                info(line)
            if error_dict:
                if line == "":
                    continue
                if line[0] == " " or line.startswith(MORE) or line.startswith(TRACE):
                    log_message += extend_error_description(line, error_dict)
                    continue
                print(log_message, file=log)
                results.append(error_dict)
            log_message, error_dict, filename = github_error(config=c, line=line)
            # Grouping of log lines:
            # https://docs.github.com/en/actions/using-workflows/workflow-commands-for-github-actions#grouping-log-lines
            last_filename = handle_grouping(filename, last_filename, log)
    handle_grouping("", last_filename, log)
    if popen.stdout:
        popen.stdout.close()
    popen.wait()
    return results


def notice(config: Config, filename: str, title: str, message: str):
    """Print a notice message in the GitHub format to generate a code annotation.

    :param file (str): The file name path for the notice and the code annotation.
    :param title (str): The title of the notice and the code annotation.
    :param message (str): The message content of the notice the code annotation.

    The function prints a notice message in the format recognized by GitHub
    for Notices which also generate a code annotation.
    """
    annotate("notice", config, filename, title, message)


def annotate(kind: str, config: Config, filename: str, title: str, message: str):
    """Print a message in the GitHub format to generate a code annotation.

    :param file (str): The file name path for the message and the code annotation.
    :param title (str): The title of the message and the code annotation.
    :param message (str): The message content of the message the code annotation.

    The function prints a message message in the format recognized by GitHub
    for messages which also generate a code annotation.
    """
    script = config["script_name"]
    print(f"::{kind} file={filename},title={script}: {title}::{message}")


def run_pytype_and_parse(config: Config) -> Ret:
    """Send pytype errors to stdout and return results as pandas table

    :param config (dict[str,str]): dict of config settings
    :param branch_url (str): Base URL of the branch for links in GitHub annotations
    :returns Tuple[int, List[Dict[str, str]]]: A tuple containing the return code
    of the pytype command (0 if successful) and a list of error results.
        Each error result is a dictionary with the following keys:
            - 'Location': The URL link to the error location in the branch on GitHub.
            - 'Function': The name of the function where the error occurred.
            - 'Error code': The code of the error.
            - 'Error message': The error message.

    - Capture the pytype errors return the parsed results as a pandas table.
    - Take a list of files to exclude from pytype checks and the base URL of the
      git branch for file links in GitHub annotations as input.
    - The function constructs the pytype command with the base command and
      additional arguments.
    - If there are command line arguments provided, they are added to the command.
    - If there are xfail files specified, they are excluded from the pytype checks.
    - The function then calls the 'run_pytype' function to run the pytype command
      and parse the annotations.
    - If there are any errors or results, the function returns the error code
      (if non-zero) or the number of results and the results themselves.
    - If there are xfail files specified, the function runs pytype on each xfail
      file separately and prints a message if there are no errors.
    - Finally, the function returns the error code (if non-zero) or the number
      of results and the results themselves.
    """
    base_command = ["pytype", "--keep-going", "--jobs", "auto"]
    args = []
    config[
        "xfail_section_description"
    ] = """
The string "expected_to_fail" in {config["section"]}"""
    xfail_files = config["expected_to_fail"] if config else []
    if len(argv) > 1:
        args = argv[1:]
        xfail_files = []
    elif xfail_files:
        # Exclude the files expected to fail from the normal pytype call,
        # we will call pytype on each one separately below(pytype may abort):
        args = ["--exclude", " ".join(xfail_files)]

    if "GITHUB_OUTPUT" in env:
        print("::group::pytype for all files which are expected to pass")
    err, results = report_on(config, stderr, base_command + args, [])
    if "GITHUB_OUTPUT" in env:
        print("::endgroup::")
    # When the the regular non-xfail run does not pass without errors bail with them:
    if err or len(results):
        return (err, results) if err > 0 else (len(results), results)
    # Else continue with running pytype for the files marked xfail, record its results:
    for xfail in xfail_files:
        print(f"::group::pytype for {xfail}")
        number_of_collected_problems_before = len(results)
        try:
            err, results = report_on(config, stdout, base_command + [xfail], results)
        except (OSError, AssertionError) as e:
            print("::endgroup::")
            print(f"::error file={xfail},title=run_pytype() raised {type(e)}::{e}")
            return 5, results
        print("::endgroup::")
        if len(results) <= number_of_collected_problems_before:
            if err:
                print(f"::error file={xfail},title=pytype exited({err}),but no results")
            else:
                xfail_section = config["xfail_section_description"]
                fix_it = f"Please check and remove it from {xfail_section}"
                notice(config, xfail, "No error exit and no errors seen", fix_it)
    # Before the xfail run, we passed the regular run and for now pass xfail (expected)
    return 0, results


def generate_markdown(config: Config, out: TextIO, returncode: int, results: Info):
    """Generate a markdown report based on the results of running pytype.

    :param config: The configuration dictionary of the script.
    :param out: The file-like object to write the markdown report to.
    :param returncode: The return code of the pytype command.
    :param results: The list of error results from running pytype.
    """
    runner_link = script_name = config["script_name"]
    pytype_link = "pytype"
    if "PR_NUMBER" in env:
        runner_link = f"[{script_name}]({config['tree_url']}/{script_name}.py)"
        pytype_link = "[pytype](https://google.github.io/pytype)"
        out.write('<a id="pytype-check-results"></a>\n#Pytype check results\n')
        anchor_text = "Pytype check results"
        # Write the final report to the file GitHub provided by $GITHUB_STEP_SUMMARY:
        out.write(f"### [{anchor_text}]\nfor PR #{env.get('PR_NUMBER', '')}\n")
    if results or returncode:
        summary = (
            f"#### {runner_link} extracted {len(results)}"
            f" problem reports from {pytype_link} output"
        )
        out.write(summary + "\n")
        out.write(pd.DataFrame(results).to_markdown() + "\n")
    else:
        summary = f"#### {runner_link} reports no errors from {pytype_link} output."
        out.write(summary + "\n")
    if "PR_NUMBER" in env:
        write_summary_file(summary, anchor_text)


def write_summary_file(summary, anchor_text):
    """Write the summary file for the pytype check results.

    :param summary: The summary of the pytype check results.
    :param anchor_text: The anchor text for the link to the results.
    """
    server = env.get("GITHUB_SERVER_URL", "https://github.com")
    repo = env.get("GITHUB_REPOSITORY", "xapi-project/xen-api")
    job_url = f"{server}/{repo}/actions/runs/" + env.get("GITHUB_RUN_ID")
    anchor_url = f"{job_url}#{anchor_text.lower().replace(' ', '-')}"
    result_md = f"You can check the results of the job [here]({anchor_url})"
    with open(".git/pytype-summary.md", "w", encoding="utf-8") as step_output:
        step_output.write(summary + ".\n\n" + result_md)


def run_pytype_and_generate_summary(config: Config) -> int:
    """Load extra pytype configuration from pyproject.toml and run pytype

    :param config (dict[str, str]): The dict configuring the pytype_reporter
    :return (int): The number of unexpected warnings/errors from the pytype calls

    Load the list of files that expected to fail pytype from pyproject.toml
    Run pytype and generate markdown output table (for GitHub, else to stdout)
    """
    result_tuple = run_pytype_and_parse(config)
    returncode = result_tuple[0]

    # Write the panda table to a markdown output file:
    summary_file = env.get("GITHUB_STEP_SUMMARY", None)
    if summary_file:
        with open(summary_file, "w", encoding="utf-8") as fp:
            generate_markdown(config, fp, *result_tuple)
    else:
        generate_markdown(config, stdout, *result_tuple)
    step_output_filename = env.get("GITHUB_OUTPUT")
    if step_output_filename:
        with open(step_output_filename, "w", encoding="utf-8") as step_output:
            step_output.write(f"exit_code={returncode}")
    return returncode


def load_config(config_file: str, script_basename: str) -> Config:
    """Load extra pytype configuration from pyproject.toml

    :param script_name: The file name of the script for writing it to the output
    :return: The configuration as a dict of strings
    """
    script_name = script_basename.removesuffix(".py")
    loglevel = DEBUG if env.get("PYTYPE_REPORTER_DEBUG") else INFO
    basicConfig(format=script_name + ": %(message)s", level=loglevel)

    pyproject = toml.load(config_file)
    debug("Loaded: %s", config_file)
    config = pyproject["tool"].get(script_name) or {}
    config["script_name"] = script_name
    config.setdefault("default_branch", DEFAULT_BRANCH_NAME)
    debug(script_name)
    debug("Default branch: %s", config["default_branch"])
    try:
        repository_url = pyproject["project"]["urls"]["repository"].strip(" /")
    except IndexError:
        repository_url = "Please add: pyproject.toml: [urls]: repository=..."

    # In a GitHub action, we want to use URL of the fork with the GitHub action:
    github_server_url = env.get("GITHUB_SERVER_URL")
    github_repository = env.get("GITHUB_REPOSITORY")
    branch = config["default_branch"]
    if github_server_url and github_repository:
        repository_url = f"{github_server_url}/{github_repository}"
        branch = env.get("GITHUB_HEAD_REF") or env.get("GITHUB_REF_NAME")

    config["tree_url"] = f"{repository_url}/blob/{branch}"
    config["section"] = f"{config_file}[tool.{script_name}]"
    return config


def get_changed_xfail_files(config: Config) -> list[str]:
    """Get the list of changed files compared to the default branch.

    :param config: The configuration dictionary.
    :return: The list of changed files.
    """
    return check_output(  # nosec:B603
        args=[
            "git",
            "diff",
            "--ignore-space-change",
            "--name-only",
            "origin/" + config["default_branch"],
            *config["expected_to_fail"],
        ],
        universal_newlines=True,
    ).splitlines()


def main():
    """This function serves as the entry point of the script.

    It performs the following steps:
    - Loads the configuration from the 'pyproject.toml' file.
    - Checks for any files that have changed but are still marked as expected
      to fail in the configuration.
    - Prints an error message and exits if there are any changed files
      that are still marked as expected to fail.
    - Calls the 'run_pytype_and_generate_summary' function to run pytype
      and generate a summary of the results.
    """
    config_file = "pyproject.toml"
    config = load_config(config_file, basename(__file__))
    config.setdefault("expected_to_fail", [])
    debug("Expected to fail: %s", ", ".join(config["expected_to_fail"]))
    changed_but_in_expected_to_fail = get_changed_xfail_files(config)
    for changed_xfail in changed_but_in_expected_to_fail:
        annotate(
            kind="error",
            config=config,
            filename=config_file,
            title=f"remove {changed_xfail} from expected_to_fail",
            message=f"{changed_xfail} was changed, remove it from expected_to_fail"
            f" in {config_file} and make sure it passes pytype checks",
        )
    if changed_but_in_expected_to_fail:
        fatal(
            "Remove "
            + ", ".join(changed_but_in_expected_to_fail)
            + f" from in {config['section']}:expected_to_fail: "
            "When files are changed, all pytype errors must be fixed!"
        )
        return 5
    return run_pytype_and_generate_summary(config)


if __name__ == "__main__":
    sys_exit(main())
