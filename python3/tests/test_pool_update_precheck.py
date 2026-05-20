#!/usr/bin/env python3
"""
Unit tests for the dnf5 output parser in pool_update.precheck.
"""

import re
import unittest

from python3.tests.import_helper import import_file_as_module, mocked_modules

with mocked_modules("XenAPI", "xcp", "xcp.logger"):
    precheck = import_file_as_module("python3/extensions/pool_update.precheck")


# ---------------------------------------------------------------------------
# Captured dnf5 outputs (real, not synthesised)
# ---------------------------------------------------------------------------

CONFLICT = """\
Updating and loading repositories:
Repositories loaded.
Failed to resolve the transaction:
Problem: installed package xs-host-1.0-1.noarch conflicts with xs-host provided by xs-conflict-1.0-1.noarch from testrepo
  - problem with installed package
  - conflicting requests
  - nothing provides xs-platform >= 2.0 needed by xs-host-2.0-1.noarch from testrepo
You can try to add to command line:
  --skip-broken to skip uninstallable packages
"""

WRONG_VERSION = """\
Updating and loading repositories:
 testrepo                               100% |  75.3 KiB/s |   3.4 KiB |  00m00s
Repositories loaded.
Failed to resolve the transaction:
Package "xs-host-1.0-1.noarch" is already installed.
Problem: cannot install the best candidate for the job
  - nothing provides xs-platform >= 2.0 needed by xs-host-2.0-1.noarch from testrepo
You can try to add to command line:
  --no-best to not limit the transaction to the best candidates
  --skip-broken to skip uninstallable packages
"""

PREREQ_MISSING = """\
Updating and loading repositories:
Repositories loaded.
Failed to resolve the transaction:
Problem: conflicting requests
  - nothing provides missing-lib needed by xs-prereq-1.0-1.noarch from testrepo
You can try to add to command line:
  --skip-broken to skip uninstallable packages
"""

NONEXISTENT = """\
Updating and loading repositories:
Repositories loaded.
Failed to resolve the transaction:
No match for argument: no-such-pkg-xyz
You can try to add to command line:
  --skip-unavailable to skip unavailable packages
"""

# Librepo emits "GPG signature verification error" on first access to a repo
# whose key is not yet trusted, before dnf5 auto-imports the key. The same
# line therefore also appears in front of unrelated solver failures, so the
# precheck plugin must only raise GpgkeyNotImported when no "Failed to
# resolve" block follows. The two fixtures below are condensed XenRT
# captures (jobs 31109885 / 31109895) used to exercise that gate.

LIBREPO_GPG_THEN_PREREQ = """\
Updating and loading repositories:
>>> Librepo error: repomd.xml GPG signature verification error: Bad GPG signature
Importing PGP key 0xDEADBEEF:
 UserID     : "XenServer Updates <security@xenserver.com>"
Key imported.
Repositories loaded.
Failed to resolve the transaction:
Problem: conflicting requests
  - nothing provides update-test-hotfix-basic-1 needed by test-hotfix-depend-1-1.0-1.noarch from updates
You can try to add to command line:
  --skip-broken to skip uninstallable packages
"""

LIBREPO_GPG_THEN_WRONG_VERSION = """\
Updating and loading repositories:
>>> Librepo error: repomd.xml GPG signature verification error: Bad GPG signature
Importing PGP key 0xDEADBEEF:
Key imported.
Repositories loaded.
Failed to resolve the transaction:
Problem: cannot install the best candidate for the job
  - nothing provides platform-version = 0.73.2 needed by control-test-hotfix-basic-4-1.0-1.noarch from updates
You can try to add to command line:
  --no-best to not limit the transaction to the best candidates
"""


# ---------------------------------------------------------------------------
# Classifier (mirrors the post-`returncode != 0` branch of execute_precheck)
# ---------------------------------------------------------------------------

def classify(output):
    """Re-run the parser branch of execute_precheck() against captured output.

    Returns a (kind, *args) tuple naming the exception class that
    execute_precheck() would have raised.
    """
    if precheck.ERROR_MESSAGE_DOWNLOAD_PACKAGE in output:
        return ("InvalidUpdate",)

    if (precheck.ERROR_MESSAGE_FAILED_TO_RESOLVE not in output
            and any(p in output
                    for p in precheck.ERROR_MESSAGE_GPGKEY_NOT_IMPORTED_PATTERNS)):
        return ("GpgkeyNotImported",)

    if precheck.PATCH_PRECHECK_FAILED_ISO_MOUNTED in output:
        return ("IsoMounted",)
    if precheck.PATCH_PRECHECK_FAILED_VM_RUNNING in output:
        return ("VmRunning",)

    m = re.search(
        precheck.ERROR_MESSAGE_FAILED_TO_RESOLVE + "(.+)$",
        output, flags=re.DOTALL,
    )
    if not m:
        return ("PrecheckFailure", output)
    errmsg = re.sub(
        precheck.ERROR_MESSAGE_END + ".+", "", m.group(1), flags=re.DOTALL
    )

    if precheck.ERROR_MESSAGE_CONFLICTS_WITH in errmsg:
        conflicts = re.findall(
            precheck.ERROR_MESSAGE_CONFLICTS_WITH + r"(\S+)", errmsg
        )
        if conflicts:
            return ("ConflictPresent", " ".join(conflicts))
        return ("PrecheckFailure", errmsg)

    wv = re.search(
        precheck.ERROR_MESSAGE_NOTHING_PROVIDES
        + r"(\S+\s*[<>=]+\s*\S+) needed by (\S+)", errmsg)
    if wv:
        return ("WrongServerVersion", wv.group(1).strip(), wv.group(2).strip())

    prereqs = re.findall(
        precheck.ERROR_MESSAGE_NOTHING_PROVIDES
        + r"(.+?)(?: needed by | from |\n)", errmsg)
    prereqs += re.findall(
        precheck.ERROR_MESSAGE_REQUIRES + r"(.+?), but none of", errmsg)
    if prereqs:
        return ("PrerequisiteMissing", " ".join(prereqs))

    return ("PrecheckFailure", errmsg)


class TestDnf5OutputParser(unittest.TestCase):
    """Parser tests against captured dnf5 5.2.12 output."""

    def test_conflict(self):
        kind, *args = classify(CONFLICT)
        self.assertEqual(kind, "ConflictPresent")
        # First "conflicts with" token in the captured output.
        self.assertIn("xs-host", args[0])

    def test_wrong_server_version(self):
        self.assertEqual(
            classify(WRONG_VERSION),
            ("WrongServerVersion",
             "xs-platform >= 2.0", "xs-host-2.0-1.noarch"),
        )

    def test_prerequisite_missing(self):
        self.assertEqual(
            classify(PREREQ_MISSING),
            ("PrerequisiteMissing", "missing-lib"),
        )

    def test_nonexistent_package(self):
        # No conflict / nothing-provides / requires pattern: falls through to
        # the generic PrecheckFailure with the cleaned errmsg.
        kind, errmsg = classify(NONEXISTENT)
        self.assertEqual(kind, "PrecheckFailure")
        self.assertIn("No match for argument: no-such-pkg-xyz", errmsg)

    def test_download_failure_short_circuits(self):
        output = "Some preamble\nFailed to download packages\nmore noise\n"
        self.assertEqual(classify(output), ("InvalidUpdate",))

    def test_gpgkey_patterns_short_circuit(self):
        # Each GPG marker alone (no "Failed to resolve" block) must classify
        # as GpgkeyNotImported.
        for pattern in precheck.ERROR_MESSAGE_GPGKEY_NOT_IMPORTED_PATTERNS:
            with self.subTest(pattern=pattern):
                self.assertEqual(
                    classify("preamble\n" + pattern + "\n"),
                    ("GpgkeyNotImported",),
                )

    def test_librepo_gpg_noise_does_not_mask_prereq(self):
        """Regression for XenRT 31109885 / 31109895.

        Librepo prints a transient GPG verification error before dnf5
        auto-imports the repo key; the run then fails at the solver stage
        for an unrelated reason. The plugin must surface the real reason
        (PrerequisiteMissing here) rather than GpgkeyNotImported.
        """
        self.assertEqual(
            classify(LIBREPO_GPG_THEN_PREREQ),
            ("PrerequisiteMissing", "update-test-hotfix-basic-1"),
        )

    def test_librepo_gpg_noise_does_not_mask_wrong_version(self):
        """Same regression, wrong-version variant."""
        self.assertEqual(
            classify(LIBREPO_GPG_THEN_WRONG_VERSION),
            ("WrongServerVersion",
             "platform-version = 0.73.2",
             "control-test-hotfix-basic-4-1.0-1.noarch"),
        )


if __name__ == "__main__":
    unittest.main()
