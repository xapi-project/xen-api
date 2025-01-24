# Quick start guide:

- Visit https://xapi-project.github.io/new-docs/ to view the current documentation.

## Required software

The docs use Hugo and the [Hugo Relearn theme](https://mcshelby.github.io/hugo-theme-relearn),
an enhanced fork of the popular Hugo Learn theme.

### Compatible versions

Due to a number of gradual changes in Hugo and Relearn,
the docs are currently only compatible with specific older versions of Hugo and Relearn.

Hugo v0.121.0 to ~v0.127.0 (the current version of the Ubuntu `snap` is too recent)
- Fixes to support newer versions are forthcoming.

Hugo Relearn 5.24.0 (defined by a git tag in doc/go.mod)
- Note: Hugo Relearn >= 5.25 currently trigger additional warnings due to deprecations.
- Further updates fix this situation are forthcoming step by step.

Hugo Relearn >= 5.24.0 and < 6.x are expected to work:
- https://mcshelby.github.io/hugo-theme-relearn/introduction/releasenotes/5/index.html#5-24-0
- Breaking changes in Relearn 6.0.0:
  https://mcshelby.github.io/hugo-theme-relearn/introduction/releasenotes/6/#6-0-0

## Installation

- Install Hugo; follow the guidance on https://gohugo.io/getting-started/installing.
  You'll need to install Go as well: see https://go.dev/
  - Hugo installation is described at https://gohugo.io/installation
  - On Ubuntu 24.04, the version installed by `apt` works.
  - On Ubuntu 22.04 and older:
    - `apt-get install hugo` would install a version that is too old.
    - `sudo snap install hugo` installs a too recent version

  - To install Hugo from source, you need a recent `golang-1.2x` compiler:
    - On Ubuntu 22.04, this can be done with:
      ```bash
      sudo apt install golang-1.23-go
      # Add it to your path, assuming your .local/bin/ is early in your PATH:
      ln -s /usr/lib/go-1.23/bin/go ~/.local/bin/go
      go version
      go install github.com/gohugoio/hugo@v0.127.0
      ```

## Development

- Run a local server: `hugo server`
- Open a browser at http://127.0.0.1:1313/new-docs/
- Add content to `doc/content/`:
  - Documents are written in Markdown.
  - Please wrap lines in paragraphs to make review and diffs easier to read.
  - The menu hierarchy comes mostly from the directory structure in `content/`.
  - A file called `_index.md` is needed in a directory to define a new level in the menu.
    - To set the page title which is also used for the main menu,
      [use the front matter](https://gohugo.io/content-management/front-matter/).
  - For a page that has images or other stuff included, it is best to create a new directory. Put the contents in a `index.md` file (no `_`) and the related files next to it. See https://gohugo.io/content-management/organization/ for more information.
  - Look at https://mcshelby.github.io/hugo-theme-relearn/ for more information about what the Relearn theme offers, including some handy "shortcodes".
