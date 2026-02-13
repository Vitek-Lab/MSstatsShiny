## Code of conduct

As contributors and maintainers of MSstats project, we pledge to follow the [Carpentry Code of Conduct](https://docs.carpentries.org/topic_folders/policies/code-of-conduct.html).

Instances of abusive, harassing or other unacceptable behavior may be reported by following our [reporting guidelines](https://docs.carpentries.org/topic_folders/policies/code-of-conduct.html#reporting-guidelines).


## Typos

Typos and grammatical errors in documentation can be fixed directly via Github. 
All changes should be made in the `.R` or `.Rmd` files rather than `.Rd` files in the `/man` directory.


## Bug reports

Bug reports can be filed in two ways:

- via the official [Google group](https://groups.google.com/forum/#!forum/msstats),

- or via [GitHub issues](https://github.com/Vitek-Lab/MSstatsConvert/issues) (the preferred way). 

Before filing a bug report please make sure that:

- the problem was not solved before 
(by searching older issues and possibly other sources such as the Google group)

- you can provide a [minimal reproducible example](https://stackoverflow.com/questions/5963269/how-to-make-a-great-r-reproducible-example) so that the developers can reproduce and understand the problem. 
The [reprex](https://reprex.tidyverse.org/) package might be helpful.

To help with describing the bug, we have created an issue template which can be used after starting a new issue.


## Feature requests

GitHub issues are meant primarily for bug requests.
We suggest to make feature requests via the official [Google group](https://groups.google.com/forum/#!forum/msstats) of `MSstats`.


## Code contributions

We welcome code contributions to the `MSstats`. 
Every contribution should be made via a [pull request](https://help.github.com/en/github/collaborating-with-issues-and-pull-requests/about-pull-requests) on GitHub.

### Hard Constraints (Required)

These requirements **must** be met for your pull request to be accepted:

* **Create a separate Git branch** for each pull request
* **Pass all CI checks** - the continuous integration system must give a green light
* **Include relevant tests** using [testthat](https://cran.r-project.org/package=testthat)
* **Document new code** using [roxygen2](https://cran.r-project.org/package=roxygen2) and [R Markdown](https://cran.r-project.org/web/packages/roxygen2/vignettes/markdown.html)
* **Add reviewers** - tag at least these two people:
    - [Anthony Wu](https://github.com/tonywu1999) - MSstats developer
    - [Mateusz Staniak](https://github.com/mstaniak) - MSstats developer
* **Update NEWS.md and package version** before merging, following [semantic versioning](https://semver.org/) and [Bioconductor versioning](http://bioconductor.org/developers/how-to/version-numbering/) conventions

### Soft Constraints (Strongly Recommended)

These guidelines should be followed when possible to maintain code quality:

* **For bug fixes**: Open an issue first to describe the problem
* **Follow the tidyverse [style guide](https://style.tidyverse.org)** with these exceptions:
    - Function and class names: use `camelCase`
    - Other names: use lowercase with underscores (`snake_case`), except for existing names that must remain unchanged for backward compatibility
* **Limit dependencies** to keep the package lightweight
* **Maintain backward compatibility** when modifying existing functionality
* **Use `@inheritParams`** when functions share parameter documentation
* **Reuse existing functions** rather than duplicating code
* **Prefer `data.table`** for data manipulation when appropriate
* **Use `match.arg()`** for argument validation when applicable

## 🚀 New Developer Setup Guide

This guide is for developers who want to contribute to any VitekLab project. It will walk you through setting up a local development environment to run the app from its source code.

### 1. Prerequisites 💻

Make sure you have the following software installed:

* **[R](https://cran.r-project.org/)**
* **[RStudio](https://posit.co/download/rstudio-desktop/)**
* **[Git](https://git-scm.com/downloads)**

### 2. Step-by-Step Setup

#### Step 1: Clone the Repository (MSstatsShiny Example)

1.  Open your computer's **Terminal** (Mac/Linux) or **Git Bash** (Windows).
2.  Navigate to the folder where you want to store the project.
3.  Run the `git clone` command

    ```bash
    git clone [https://github.com/Vitek-Lab/MSstatsShiny.git](https://github.com/Vitek-Lab/MSstatsShiny.git)
    ```

#### Step 2: Set Up the RStudio Project

1.  Open the **RStudio** application.
2.  Go to the top menu: **File > New Project...**
3.  Select **Existing Directory**.
4.  Click **Browse...** and navigate to the `MSstatsShiny` folder you just cloned.
5.  Click **Create Project**.

#### Step 3: Install All Dependencies

1.  In RStudio, go to the **Console** pane.
2.  Install the `devtools` package:

    ```r
    install.packages("devtools")
    ```

3.  Run the following command to install all project dependencies:

    ```r
    devtools::install_deps(dependencies = TRUE)
    ```

> **Note:** This step can take a long time. You only need to do this once.

### 3. The Developer Workflow 🧑‍💻

This is the cycle you will follow to make and test changes.

#### Step 1: Load Your Local Code

In the RStudio **Console**, run:

```r
library(MSstatsShiny)
```

#### Step 2: Edit, Load, and Test!

This is your core workflow:

1.  **Edit:** Open any project file and make your code changes.
2.  **Save:** Save the file.
3.  **Install:** Go to the top right corner and click "Install" to reinstall the package.
4.  **Build:** Go to the top right corner and click "Build" to run all tests and build the package.

Repeat this **Edit > Save > Install > Build** cycle as you develop.
