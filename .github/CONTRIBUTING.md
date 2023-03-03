# Contributing to `eamenaR`

This outlines how to propose a change to `eamenaR`.

## Fixing typos

You can fix typos, spelling mistakes, or grammatical errors in the documentation (`.R` files upper part) directly using the GitHub web interface.  

## Bigger changes

To develop the versatility of the package, we encourage contributions directly related to:

* Multivariate analysis

* Time series

* IIIF

If you want to make a bigger change, it's a good idea to first file an issue and make sure someone from the team agrees that it's needed. If you've found a bug, please file an issue that illustrates the bug with a minimal [reprex](https://www.tidyverse.org/help/#reprex) (this will also help you write a unit test, if needed).

### Pull request process

*   Fork the package and clone onto your computer. If you haven't done this before, we recommend using `usethis::create_from_github("eamena-project/eamenaR", fork = TRUE)`.

*   Make sure the package passes R CMD check by running `devtools::check()`. If R CMD check doesn't pass cleanly, it's a good idea to ask for help before continuing. 
*   Create a Git branch for your pull request (PR). We recommend using `usethis::pr_init("brief-description-of-change")`.
*   Make your changes, commit to git, and then create a PR by running `usethis::pr_push()`, and following the prompts in your browser. The title of your PR should briefly describe the change. The body of your PR should contain `Fixes #issue-number`.

*  For user-facing changes, add a bullet to the top of [`NEWS.md`](https://github.com/eamena-project/eamenaR/blob/master/NEWS.md).


## Code of Conduct

Please note that the iconr project is released with a [Contributor Code of Conduct](CODE_OF_CONDUCT.md). By contributing to this project you agree to abide by its terms.
