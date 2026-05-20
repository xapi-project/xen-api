#!/usr/bin/env python3
"""
Unit tests for the dnf5 output parser in pool_update.precheck.
"""

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


class TestDnf5OutputParser(unittest.TestCase):
    """Parser tests against captured dnf5 5.2.12 output."""

    def test_conflict(self):
        with self.assertRaises(precheck.ConflictPresent) as ctx:
            precheck.classify_dnf_failure(CONFLICT)
        # First "conflicts with" token in the captured output.
        self.assertIn("xs-host", ctx.exception.args[0])

    def test_wrong_server_version(self):
        with self.assertRaises(precheck.WrongServerVersion) as ctx:
            precheck.classify_dnf_failure(WRONG_VERSION)
        self.assertEqual(
            ctx.exception.args,
            ("xs-platform >= 2.0", "xs-host-2.0-1.noarch"),
        )

    def test_prerequisite_missing(self):
        with self.assertRaises(precheck.PrerequisiteMissing) as ctx:
            precheck.classify_dnf_failure(PREREQ_MISSING)
        self.assertEqual(ctx.exception.args, ("missing-lib",))

    def test_nonexistent_package_falls_through(self):
        # No conflict / nothing-provides / requires pattern: classifier
        # raises PrecheckFailure with the cleaned errmsg.
        with self.assertRaises(precheck.PrecheckFailure) as ctx:
            precheck.classify_dnf_failure(NONEXISTENT)
        self.assertIn("No match for argument: no-such-pkg-xyz",
                      ctx.exception.args[0])

    def test_download_failure_short_circuits(self):
        output = "Some preamble\nFailed to download packages\nmore noise\n"
        with self.assertRaises(precheck.InvalidUpdate):
            precheck.classify_dnf_failure(output)

    def test_gpgkey_patterns_short_circuit(self):
        # Each GPG marker alone (no "Failed to resolve" block) must classify
        # as GpgkeyNotImported.
        for pattern in precheck.ERROR_MESSAGE_GPGKEY_NOT_IMPORTED_PATTERNS:
            with self.subTest(pattern=pattern):
                with self.assertRaises(precheck.GpgkeyNotImported):
                    precheck.classify_dnf_failure("preamble\n" + pattern + "\n")

    def test_librepo_gpg_noise_does_not_mask_prereq(self):
        """Regression for XenRT 31109885 / 31109895.

        Librepo prints a transient GPG verification error before dnf5
        auto-imports the repo key; the run then fails at the solver stage
        for an unrelated reason. The plugin must surface the real reason
        (PrerequisiteMissing here) rather than GpgkeyNotImported.
        """
        with self.assertRaises(precheck.PrerequisiteMissing) as ctx:
            precheck.classify_dnf_failure(LIBREPO_GPG_THEN_PREREQ)
        self.assertEqual(
            ctx.exception.args, ("update-test-hotfix-basic-1",),
        )

    def test_librepo_gpg_noise_does_not_mask_wrong_version(self):
        """Same regression, wrong-version variant."""
        with self.assertRaises(precheck.WrongServerVersion) as ctx:
            precheck.classify_dnf_failure(LIBREPO_GPG_THEN_WRONG_VERSION)
        self.assertEqual(
            ctx.exception.args,
            ("platform-version = 0.73.2",
             "control-test-hotfix-basic-4-1.0-1.noarch"),
        )

    def test_iso_mounted(self):
        output = "noise\n" + precheck.PATCH_PRECHECK_FAILED_ISO_MOUNTED + "\n"
        with self.assertRaises(precheck.IsoMounted):
            precheck.classify_dnf_failure(output)

    def test_vm_running(self):
        output = "noise\n" + precheck.PATCH_PRECHECK_FAILED_VM_RUNNING + "\n"
        with self.assertRaises(precheck.VmRunning):
            precheck.classify_dnf_failure(output)

    def test_unrecognised_output_returns_silently(self):
        # When the output matches none of the patterns the classifier just
        # returns; execute_precheck then raises PrecheckFailure(output).
        self.assertIsNone(precheck.classify_dnf_failure("totally unrelated\n"))


if __name__ == "__main__":
    unittest.main()
