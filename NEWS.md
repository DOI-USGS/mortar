mortar 0.3.2
===========
- Update README template with one geared toward software release.

mortar 0.3.1
===========
- Build mortar with GitLab CI instead of manually and committing HTML to repo
- Switch logo from PNG to SVG
- Add dark mode capability to website

mortar 0.3.0
===========
- Prepare repo for provisional data release
- Add missing metadata documents
- Update existing metadata
- Update README and Getting Started vignette
- Remove incidental absolute file paths from git history. Breaks some prior git links.

mortar 0.2.3
===========
- Add MR template to inst directory
- Create `use_gitlab_mr_template()` to write the template file to the correct location to be used as the default template
- Added tests and beefed up existing assertion checks based on results of figuring out the tests.

mortar 0.2.2
===========
- Substantially increase the number of unit tests
- Add some additional assertion tests

mortar 0.2.1
===========
- Dynamically adds the repository link to the issue page in CONTRIBUTING.md (#3).

mortar 0.2.0
===========
- Fixes the layout of _targets.R.
- Updates _targets.R to use `tar_source()` for simpler code.
- If home = "." in `tar_init()`, doesn't prepend directory with unnecessary ".".
- Gives option to prepend phase numbers in files with 0 in `tar_init()`.
- Enumerate possible options on arguments where it makes sense for `use_usgs_project()`.
- Adds code.json.
- Add dontrun to the documentation example of use_usgs_project() which was inadvertently printing absolute file paths in the documentation.
- Update DESCRIPTION with me as maintainer and incremented version minor release to 0.2.0.
- Update documentation
- Remove strange merge conflict text in some of the HTML.
- Update URLs in _pkgdown.yml and DESCRIPTION and update to bootstrap 5 to remove warnings from `pkgdown::build_site()`.

