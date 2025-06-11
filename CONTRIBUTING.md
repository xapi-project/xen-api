# Issues

We welcome reports of technical issues with the components of the xen-api
toolstack. Please make sure that the description of the issue is as detailed as
possible to help anyone investigating it:

1) Mention how it was detected, if and how it could be reproduced

1) What's the desired behaviour? In what cases would it be useful?

1) Include error messages, related logs if appropriate

# Pull Requests

To contribute changes to xen-api, please fork the repository on
GitHub, and then submit a pull request.

It is required to add a `Signed-off-by:` as a
[Developers Certificate of Origin](http://developercertificate.org).
It certifies the patch's origin and is licensed under an
appropriate open-source licence to include it in Xapi:
https://git-scm.com/docs/git-commit#Documentation/git-commit.txt---signoff

The following points are intended to describe what makes a contribution "good" -
easier to review, integrate, and maintain. Please follow them in your work.

## Commit subjects and PR titles

Commit subjects should preferrably start with the name of the component the
commit is most related to, and describe what the commit achieves. If your
commit only touches the `ocaml/xenopsd` directory, it should look like this,
for example:

```
xenopsd: Fix a deadlock during VM suspend
```

Similar principle applies to Pull Request titles. If there is only a single
commit in the PR, Github will automatically copy its subject and description to
the PR's title and body. If there are several commits in the PR, describe what
the PR achieves and the components it most directly impacts.

If the commit subject includes some tracking identifier (such as `CP-1234`, for
example) referring to internal systems, please make sure to include all of the
essential information in the public descriptions - describe the symptoms of the
issue, how it was detected, investigated, how it could be reproduced, what are
the trade-offs and so on as appropriate.

## Split into commits

Following from the rules described above, if what the commit achieves is
difficult to fit into its subject, it is probably better to split it into
several commits, if possible. Note that every commit should build (`make`
should work and the CI should pass) independently, without requiring future
commits. This means some modifications can't really be split into several
commits (datamodel changes, in particular, require modifications to several
components at the same time), but makes it easier to revert part of the Pull
Request if some issues are detected in integration testing at a later point.

## Good Commit Messages

Commit messages (and the body of a Pull Request) should be as helpful and
descriptive as possible. If applicable, please include a description of current
behaviour, your changes, and the new behaviour. Justify the reasoning behind
your changes - are they sufficient on their own, or preparing for more changes?
Link any appropriate documentation, issues, or commits (avoiding internal and
publicly inaccessible sources)

## CI

Please make sure your Pull Request passes the Github CI. It will verify that
your code has been properly formatted (can be done locally with `make format`),
builds (`make` and `make check`), and passes the unit tests (`make test`).
The CI will run in the branches of your fork, so you can verify it passes
there before opening a Pull Request.

## Testing

Describe what kind of testing your contribution underwent. If the testing was
manual, please describe the commands or external clients that were used. If the
tests were automated, include at least a cursory description/name of the tests,
when they were regressed, if possible.

Please note that any contribution to the code of the project will likely
require at least some testing to be done. Depending on how central the
component touched in your PR is to the system, the more things could only be
detected in real-world usecases through integration testing.

If a commit has been determined to break integration testing at a later stage,
please note that the first and safest measure will almost always be reverting
the faulty commit. Making sure critical tests are passing remains a priority
over waiting for some commit to be reworked or refactored (which can be worked
on after a revert has been done). Though we are striving to make more tests
public (with failure then being visible to all), as long as some critical tests
remain private, this will also apply to such tests (with maintainers flagging
the breakage preferrably describing at least the gist of the test).

If you are still waiting on some testing to be done, please mark the PR as a
"draft" and make the reasoning clear.

If wider testing is needed (e.g. the change itself is believed to be correct
but may expose latent bugs in other components), lightweight feature flags can
also be used. E.g. an entry in `xapi_globs.ml` and `xapi.conf`, where the
feature/change is defaulted to `off`, to be turned on at a future time
(when e.g. more related PRs land, or it has passed some wider testing).

If your contribution doesn't intend to have any functional changes, please make
that clear as well.

## Feature work

If your contribution adds some new feature or reworks some major aspect of the
system (as opposed to one-off fixes), it can be benefitial to first describe
the plan of your work in a design proposal. Architectural issues are better
spotted early on, and taking a big-picture view can often lead to new insights.

An example of a design proposal is here:

https://github.com/xapi-project/xen-api/pull/6387

If submitting a design first is not possible, include documentation alongside
with your PR describing the work, like it was done in the last three commits
here:

https://github.com/xapi-project/xen-api/pull/6457

Note that the design will often serve as documentation as well - so take care
updating it after the implementation is done to better reflect reality.

## Review process and merge

It can often be useful to address review suggestions with a "fixup" commit
(created manually or with the help of `git commit --fixup=HASH`). This way it
is clear what the original code was and what your fix touches. Once the
fixup commit has been reviewed and the PR approved, please squash the fixup
commits with `git rebase --autosquash` before merging. Otherwise the commits in
the Pull Request should stay as independent commits - we do not require
squashing all the commits into a single one on merge.

If the commit fixes a bug in an earlier, already merged PR then it might be
useful to mention that in the commit, if known.

This can be done by adding this to your GIT configuration:

```
[pretty]
    fixes = Fixes: %h (\"%s\")
```

And then running:

```
# git log -1 --pretty=fixes <hash-of-bad-commit>
Fixes: 1c581c074 ("xenopsd: Fix a deadlock during VM suspend")
```

This will print the commit title and hash in a nice format, which can then be
added to the footer of the commit message (alongside the sign-off).

This is useful information to have if any of these commits get backported to
another release in the future, so that we also backport the bugfixes, not just
the buggy commits.
