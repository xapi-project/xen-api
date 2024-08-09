---
title: "Python"
---

Introduction
------------

Most Python3 scripts and plugins shall be located below the `python3` directory.
The structure of the directory is as follows:

- `python3/bin`: This contains files installed in `/opt/xensource/bin`
   and are meant to be run by users
- `python3/libexec`: This contains files installed in `/opt/xensource/libexec`
   and are meant to only be run by `xapi` and other daemons.
- `python3/packages`: Contains files to be installed in python's `site-packages`
   are meant to be modules and packages to be imported by other scripts
   or executed via `python3 -m`
- `python3/plugins`: This contains files that
   are meant to be `xapi` plugins
- `python3/tests`: Tests for testing and covering the Python scripts and plugins

Dependencies for development and testing
----------------------------------------

In GitHub CI and local testing, we can use [pre-commit] to execute the tests.
It provides a dedicated, clearly defined and always consistent Python environment.
The easiest way to run all tests and checks is to simply run [pre-commit].
The example commands below assume that you have Python3 in your PATH.
Currently, Python 3.11 is required for it:

```bash { title="Installing and running pre-commit" }
pip3 install pre-commit
pre-commit run -av
# Or, to just run the pytest hook:
pre-commit run -av pytest
```

> Note: By default, CentOS 8 provides Python 3.6, whereas some tests need Python >= 3.7

Alternatively, you can of course tests in any suitable environment,
given that you install the supported versions of all dependencies.
You can find the dependencies in the list [additional_dependencies] of the [pytest] hook
in the [pre-commit] configuration file [.pre-commit-config.yaml].
{{% expand title=
"Example `pytest` hook from `.pre-commit-config.yaml` (expand)" %}}

```yaml
    hooks:
    -   id: pytest
        files: python3/
        name: check that the Python3 test suite in passes
        entry: sh -c 'coverage run && coverage xml &&
            coverage html && coverage report &&
            diff-cover --ignore-whitespace --compare-branch=origin/master
            --show-uncovered --html-report .git/coverage-diff.html
            --fail-under 50 .git/coverage3.11.xml'
        require_serial: true
        pass_filenames: false
        language: python
        types: [python]
        additional_dependencies:
        - coverage
        - diff-cover
        - future
        - opentelemetry-api
        - opentelemetry-exporter-zipkin-json
        - opentelemetry-sdk
        - pytest-mock
        - mock
        - wrapt
        - XenAPI
```

{{% /expand %}}

Coverage
--------

Code moved to the python3 directory tree shall have good code coverage using
tests that are executed, verified and covered using [pytest] and [Coverage.py].
The `coverage` tool and [pytest] are configured in `pyproject.toml` and
`coverage run` is configured to run [pytest] by default.

`coverage run` collects coverage from the run and stores it in its database.
The most simple command line to run and report coverage to stdout is:
`coverage run && coverage report`

{{% expand title="Other commands also used in the pytest hook example above (expand)" %}}

- `coverage xml`: Generates an XML report from the coverage database to
  `.git/coverage3.11.xml`. It is needed for upload to <https://codecov.io>
- `coverage html`: Generates an HTML report from the coverage database to
  `.git/coverage_html/`
{{% /expand %}}

We configure the file paths used for the generated database and other coverage
configuration in the sections `[tool.coverage.run]` and `[tool.coverage.report]`
of `pyproject.toml`.

Pytest
------

If your Python environment has the [dependencies for the tests] installed, you
can run [pytest] in this environment without any arguments to use the defaults.

{{% expand title="For development, pytest can also only run one test (expand)" %}}

To run a specific pytest command, run pytest and pass the test case to it (example):

```bash { title="Example for running only one specific test" }
pytest python3/tests/test_perfmon.py
```

```bash { title="Running only one test and reporting the code coverage of it" }
coverage run -m pytest python3/tests/test_perfmon.py && coverage report
```

{{% /expand %}}

[coverage.py]: https://coverage.readthedocs.io
"coverage.py is the coverage collector for Python"
[dependencies for the tests]: #dependencies-for-development-and-testing
"Installation of the dependencies for development and testing"
[pytest]: https://docs.pytest.org "Pytest documentation"
[pre-commit]: https://pre-commit.com "pre-commit commit hook framework"
[.pre-commit-config.yaml]: https://pre-commit.com/#adding-pre-commit-plugins-to-your-project
"project-specific configuration file of pre-commit, found in the project's top directory"
[additional_dependencies]: https://pre-commit.com/#pre-commit-configyaml---hooks
"dependencies that will be installed in the environment where this hook gets to run"
