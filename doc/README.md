Quick start guide:

- Install Hugo; follow the guidance on https://gohugo.io/getting-started/installing.
  You'll need Go as well: see https://go.dev/
- Run a local server: `hugo server`
- Open a browser at http://127.0.0.1:1313/
- Add some content.
  - Documents are written in Markdown.
  - Please wrap lines in paragraphs to make review and diffs easier to read.
  - The menu hierarchy comes mostly from the directory structure in `content/`.
  - A file called `_index.md` is needed in a directory to define a new level in the menu. Add a title to the frontmatter.
  - For a page that has images or other stuff included, it is best to create a new directory. Put the contents in a `index.md` file (no `_`) and the related files next to it. See https://gohugo.io/content-management/organization/ for more information.
  - Look at https://mcshelby.github.io/hugo-theme-relearn/ for more information about what the Relearn theme offers, including some handy "shortcodes".
