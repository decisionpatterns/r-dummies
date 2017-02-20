dummies
====

[![CRAN](http://www.r-pkg.org/badges/version/dummies)](https://cran.rstudio.com/web/packages/dummies/index.html) 
[![License](http://img.shields.io/badge/license-GPL%20%28%3E=%202%29-brightgreen.svg?style=flat)](http://www.gnu.org/licenses/gpl-2.0.html) 
[![Downloads](http://cranlogs.r-pkg.org/badges/dummies?color=brightgreen)](http://www.r-pkg.org/pkg/dummies)


Create dummy/indicator variables flexibly and efficiently

Contains functions to create dummy variables flexibly using *model.matrix* returning them as either matrices or data frames for further analysis.  

Also, contains methods, for manipulating dummy variables.

## Installation

### Stable 

    install.packages('dummies')
    
### Development

    devtools::install_github("decisionpatterns/r-dummies")

## Features

Create dummy variables: 

- from vectors (e.g. characters, factors) or data (e.g. data.frame, data.table, tibble, matrix)
- as data (e.g. data.frame, data.table, tibble or matrix)
- for all columns/variables 
- based on class or type (e.g. factor, character )
- encode as integers, logicals or factors

### Planned

- Allow for dummies based on another data see the `dummy` package.
- Automatically create for '(Missing)' or '(Other)' type dummy variables 
  category.
- Option to omit reference level

## Important Functions 

- `dummy` : creates dummy variables

## Usage

### Expand atomic 

    1:3 %>% dummy( sep="|" )


### Data Structures: data.table, data.frame. tibble and matrix


## References 

- (http://wiki.r-project.org/rwiki/doku.php?id=tips:data-manip:create_indicator)
- (http://tolstoy.newcastle.edu.au/R/help/00b/1199.html)
- (http://tolstoy.newcastle.edu.au/R/help/03a/6409.html)
- (http://tolstoy.newcastle.edu.au/R/help/01c/0580.html)
