# Input data generation for the REMIND MFA

R package **mrmfa**, version **1.1.0.9001**

[![CRAN status](https://www.r-pkg.org/badges/version/mrmfa)](https://cran.r-project.org/package=mrmfa) [![R build status](https://github.com/pik-piam/mrmfa/workflows/check/badge.svg)](https://github.com/pik-piam/mrmfa/actions) [![codecov](https://codecov.io/gh/pik-piam/mrmfa/branch/master/graph/badge.svg)](https://app.codecov.io/gh/pik-piam/mrmfa) 

## Purpose and Functionality

The mrmfa packages contains data preprocessing for the REMIND MFA.


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

Dürrwächter J, Weiss B, Schweiger L, Benke F, Hosak M, Zhang Q (2026). "mrmfa: Input data generation for the REMIND MFA." Version: 1.1.0.9001, <https://github.com/pik-piam/mrmfa>.

A BibTeX entry for LaTeX users is

 ```latex
@Misc{,
  title = {mrmfa: Input data generation for the REMIND MFA},
  author = {Jakob Dürrwächter and Bennet Weiss and Leonie Schweiger and Falk Benke and Merlin Jo Hosak and Qianzhi Zhang},
  date = {2026-02-12},
  year = {2026},
  url = {https://github.com/pik-piam/mrmfa},
  note = {Version: 1.1.0.9001},
}
```
