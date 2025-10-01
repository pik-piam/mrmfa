# Add an adequate Title

R package **mrmfa**, version **0.0.2**

[![CRAN status](https://www.r-pkg.org/badges/version/mrmfa)](https://cran.r-project.org/package=mrmfa) [![R build status](https://github.com/pik-piam/mrmfa/workflows/check/badge.svg)](https://github.com/pik-piam/mrmfa/actions) [![codecov](https://codecov.io/gh/pik-piam/mrmfa/branch/master/graph/badge.svg)](https://app.codecov.io/gh/pik-piam/mrmfa) 

## Purpose and Functionality

Add an adequate Description, formulated as a complete sentence.


## Installation

For installation of the most recent package version an additional repository has to be added in R:

```r
options(repos = c(CRAN = "@CRAN@", pik = "https://rse.pik-potsdam.de/r/packages"))
```
The additional repository can be made available permanently by adding the line above to a file called `.Rprofile` stored in the home folder of your system (`Sys.glob("~")` in R returns the home directory).

After that the most recent version of the package can be installed using `install.packages`:

```r 
install.packages("mrmfa")
```

Package updates can be installed using `update.packages` (make sure that the additional repository has been added before running that command):

```r 
update.packages()
```

## Questions / Problems

In case of questions / problems please contact Jakob Dürrwächter <jakobdu@pik-potsdam.de>.

## Citation

To cite package **mrmfa** in publications use:

Dürrwächter J (2025). "mrmfa: Add an adequate Title." Version: 0.0.2, <https://github.com/pik-piam/mrmfa>.

A BibTeX entry for LaTeX users is

 ```latex
@Misc{,
  title = {mrmfa: Add an adequate Title},
  author = {Jakob Dürrwächter},
  date = {2025-10-01},
  year = {2025},
  url = {https://github.com/pik-piam/mrmfa},
  note = {Version: 0.0.2},
}
```
