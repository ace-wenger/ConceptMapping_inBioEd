# Working title: *Concept Mapping in Biology Education - A Systematic Review and Meta-Analysis.*

This work was completed as part of my dissertation for a Ph.D. in Science Education at Western Michigan University. 
My committee chair is Dr. William Cobern with Dr. Betty Adams and Dr. Ya Zhang serving as committee members.

## Purpose
Concept mapping (CM) instructional interventions have substantial positive effects, according to several meta-analyses. 
These meta-analyses found significant heterogeneity in effect sizes (ES), but did not apply best methods for investigating it. 
The current study assessed and investigated the heterogeneity of 92 ES from 44 experimental or quasi-experimental studies of CM in biology education using multilevel, meta-regression. 

## Research Questions
1. What is the mean effect size for cognitive outcomes with CM interventions in biology education and to what extent does it vary within and between studies?
2. After controlling for study-level, extrinsic and methodological characteristics, to what extent do effect sizes vary within and between studies?
3. What is the relationship between effect size and characteristics of the intervention and sample for CM interventions in biology education controlling for study-level, extrinsic and methodological characteristics?

## Ancillary Goals
The reproducibility of results (i.e., same data + same analysis = same results) is an important principle of scientific practice but is difficult to achieve.
This project aims to be computationally reproducible in that the data analysis is explicit and entirely scripted with no undocumented external dependecies or user inputs. 
This project also aims to be extensible in that analysis scripts are written as a series of functions that reliably perform one operation with no side effects (changes to the computational environment which may impact futher operations).

In order to achieve these goals, the `renv` and `targets` packages are implemented.
`renv` records the R environment and validates future environments.
`targets` defines a data analysis pipeline and monitors the pipeline for changes.

# Overview of Repository Structure

- data
- R
- tables
- figures
- writing

- _targets.R
- renv.lock


# Pipeline
### Data processing
- data_raw
- data_raw_change
- data_processed

### Evaluating influential cases
- data_vcv_matrix
- model_base
- table_model_base
- model_base_cooks
figure_cooks_base
- data_processed_out
- data_vcv_matrix_out
- model_base_out
- table_model_base_out
- model_base_cooks_out
figure_cooks_base_out

### Exploratory univariate meta-regression models
- model_univariate
- table_model_univariate
- model_univariate_out
- table_model_univariate_out

### Multiple meta-regression models
- model_exmeth
- table_model_exmeth
- model_exmeth_out
- table_model_exmeth_out

- model_sampint
- table_model_sampint
- model_sampint_out
- table_model_sampint_out

- model_allsampint
- table_model_allsampint
- model_allsampint_out
- table_model_allsampint_out

- model_all
- table_model_all
- model_all_out
- table_model_all_out