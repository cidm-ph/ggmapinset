# Resubmission

Resubmission to address feedback from v0.2.2:

  * Example for `build_sf_inset_layers()` no longer uses `\dontrun{}`.

## Test environments
- R-hub windows-x86_64-devel (r-devel)
- R-hub ubuntu-gcc-release (r-release)
- R-hub fedora-clang-devel (r-devel)

## R CMD check results
❯ On windows-x86_64-devel (r-devel), ubuntu-gcc-release (r-release)
  checking CRAN incoming feasibility ... NOTE
  Maintainer: 'Carl Suster <Carl.Suster@health.nsw.gov.au>'
  
  New submission

❯ On windows-x86_64-devel (r-devel)
  checking for detritus in the temp directory ... NOTE
  Found the following files/directories:
    'lastMiKTeXException'

❯ On fedora-clang-devel (r-devel)
  checking CRAN incoming feasibility ... [6s/25s] NOTE
  Maintainer: ‘Carl Suster <Carl.Suster@health.nsw.gov.au>’
  
  New submission

❯ On fedora-clang-devel (r-devel)
  checking HTML version of manual ... NOTE
  Skipping checking HTML validation: no command 'tidy' found

0 errors ✔ | 0 warnings ✔ | 4 notes ✖
