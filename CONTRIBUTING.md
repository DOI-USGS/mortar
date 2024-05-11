Contributing
============

Contributions are welcome from the community. Questions can be asked on the
issues page of this repo. Before creating a new issue, please take a moment to search
and make sure a similar issue does not already exist. If one does exist, you
can comment (most simply even with just a `:+1:`) to show your support for that
issue.

If you have direct contributions you would like considered for incorporation
into the project you can [fork this repository][2] and
[submit a merge request][3] for review.

## Package Development 101

This section discusses the basic workflow of writing R packages.
See [this cheat sheet](https://rstudio.github.io/cheatsheets/package-development.pdf) for more information.

1. Make an informatively named branch off of the main branch of this repo.

2. Make your desired change(s).

  - If writing a new function, make sure you thoroughly document the function using [`roxygen2`](https://roxygen2.r-lib.org/) style. If you want this function to be exported with the package, add an `@export` tab somewhere in the documentation (I prefer at the very end) 
  
  - If you're changing a function that is already documented, make sure to update any aspects of the documentation your changes make outdated.

  - Write some tests to verify the function works/breaks when it should (e.g., throws an error when incorrect arguments are passed). See the [testthat](https://testthat.r-lib.org/) package documentation for information on writing good unit tests.

3. Re-compile the documentation with [`devtools::document()`](https://devtools.r-lib.org/reference/document.html). This will print warnings/errors if there's anything wrong with the documentation. This will update/create an `.Rd` file in the `man` folder.

  - Bonus: re-build the package website with [`pkgdown::build_site()`](https://pkgdown.r-lib.org/reference/build_site.html). This will update the contents of the `docs` folder.
  
4. Re-build the package with `CTRL/CMD + SHIFT + B`. Wait for the session to restart and the package to load.

5. Run your function (in the console or wherever) and verify it works as expected.

6. Stage, commit, and push your changes to the `mortar` GitLab repo. Open a merge request and add someone appropriate (e.g., the package maintainer) as a reviewer.

[2]: https://docs.gitlab.com/ee/user/project/working_with_projects.html#fork-a-project
[3]: https://docs.gitlab.com/ee/user/project/merge_requests/
