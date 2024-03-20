
<!-- README.md is generated from README.Rmd. Please edit that file -->

# mortar

<!-- badges: start -->
<!-- badges: end -->
<!-- > *Mortar: a plastic building material (such as a mixture of cement, -->
<!-- > lime, or gypsum plaster with sand and water) that hardens and is used -->
<!-- > in masonry or plastering* -->
<!-- > ([source](https://www.merriam-webster.com/dictionary/mortar)) -->

The goal of `mortar` is to standardize common workflows in the USGS
Water Mission Area’s Data Science Branch to produce more robust,
reproducible pipelines. It contains helper functions to standardize the
creation of [`targets`](https://books.ropensci.org/targets/) pipelines
using our branch’s best practices, manage package dependencies using
[`renv`](https://rstudio.github.io/renv/index.html), and other
functionality (eventually, hopefully) contributed by users who want to
standardize their workflows across DaSB projects/collaborators.

The package name has multiple meanings. It mixes water (😉) with
structural ingredients to form a foundation upon which our pipelines can
be more easily and efficiently built. It was also originally created to
provide **mor**e **tar**gets functions than those available in the
`targets` and `tarchetypes` packages.

## Installation

The easiest installation method is to clone this repository locally and
install the package yourself. For this, you only need to have read
access to this repository (which you should have if you’re reading this
README).

1.  Clone the repo the usual way like
    `git clone git@code.usgs.gov:wma/iidd/analytics/mor_tar.git` if
    using ssh or
    `git clone https://code.usgs.gov/wma/iidd/analytics/mor_tar.git` if
    using https.

2.  Open the mortar.Rproj file to open this package’s project in
    RStudio.

3.  Install the package’s dependencies by running
    `remotes::install_deps()` in the R Console (note: requires the
    [`remotes`](https://remotes.r-lib.org/) R package)

4.  Once open, press CTRL/CMD + SHIFT + B to build and install the
    package. Alternatively, navigate to the “Build” tab in the same
    toolbar as “Environment” - a quick way is to press CTRL/CMD +
    SHIFT + F2. Then press the “Install” button at the top.

5.  Wait for package to install and for the session to restart. You
    should now be able to load the waterlogged package locally.

The downside of this local install option is that you’ll need to pull
and manually re-install the package each time changes are made to this
repo. Another option is to use a Personal Access Token to install the
package using `remotes::install_gitlab()`. You’ll need to ask the author
of this package (currently Joe Zemmels, <jzemmels@usgs.gov>) for a
Personal Access Token (PAT). Once you have a PAT, you can install mortar
with the following call, replacing “pat_here” with your personal access
token.

``` r
remotes::install_gitlab( repo='wma/iidd/analytics/mortar', 
                         auth_token = "pat_here", 
                         host='code.usgs.gov', 
                         quiet=FALSE, force=TRUE )
```

Using this method, you only need the PAT to re-install the package. You
will want to somehow save this PAT since you can’t view it after it’s
created.

## TODO

Here are some ideas of potential functionality we can add to the
package.

- Initialize `renv` with some pre-loaded packages (`targets`, etc.)
  - Add an automatic `renv::status()` check in a `_targets.R` file as
    detailed
    [here](https://code.usgs.gov/wma/national-iwaas/NWAA/wu-crosswalks/-/merge_requests/25#note_608723).
- Initialize/update .gitignore with an opinionated list of files we
  often want ignored (`.Renviron`, for example)
- Function(s) for locally saving credentials (ScienceBase, Google
  Analytics, etc.) using `.Renviron` file as is done
  [here](https://code.usgs.gov/wma/national-iwaas/NWAA/wu-crosswalks/-/blob/main/00_config/src/sb_cache.R?ref_type=heads)
  or using the [`secret`](https://github.com/gaborcsardi/secret) R
  package as done
  [here](https://code.usgs.gov/wma/iidd/analytics/waterlogged/-/blob/main/R/saml2aws_login.R?ref_type=heads).
  - Option to encrypt these credentials using the user’s local SSH key
- Linting functions to check code against DaSB’s best practices using
  the [`lintr`](https://lintr.r-lib.org/) package or others
- Functions to provide more user-friendly set up and management of conda
  environments using the
  [`reticulate`](https://rstudio.github.io/reticulate/) R package.
- Function(s) for setting up a `gitlab-ci.yml` file easily and/or
  invoking GitLab runners within a project
