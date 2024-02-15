# The Retrospective Analysis of Multicenter Clinical Trial Data on Graft vs Host Disease in Patients Following Bone Marrow Transplantation
This project is a retrospective analysis of data from a multi-centre clinical trial designed to investigate graft versus host response (GvHD) in patients following bone marrow transplantation. The study aims to identify potential associations with glucocorticoid resistance, analyse the survival of patients with acute and chronic GvHD, and establish a predictive model for glucocorticoid resistance. Based on data from 343 patients, the project includes biostatistical analysis and the application of machine learning methods to assess the impact of various factors on transplant outcomes and to develop personalised treatment strategies to improve bone marrow transplant outcomes and prevent GvHD.

## Table of content

[Structure of repository](https://github.com/Asklepiad/GvHD/blob/main/README.md#structure-of-repository)

[Introduction](https://github.com/Asklepiad/GvHD/blob/main/README.md#introduction)

[Aims and objectives](https://github.com/Asklepiad/GvHD/blob/main/README.md#aims-and-objectives)

[Data](https://github.com/Asklepiad/GvHD/blob/main/README.md#data)

[Workflow](https://github.com/Asklepiad/GvHD/blob/main/README.md#workflow)

[Results](https://github.com/Asklepiad/GvHD/blob/main/README.md#results)

[Discussions](https://github.com/Asklepiad/GvHD/blob/main/README.md#discussions)

[Literature](https://github.com/Asklepiad/GvHD/blob/main/README.md#literature)

## Structure of repository

There are four directories in the repo.

1. `raw_data`: the folder contains the data we had at the start point of analysis. You can reproduce our way of analysis (or do your own instead), using these datasets.

2. Placeholder: checking and adding data from Ivan `survival_analysis`: the folder contains one script that performed survival analysis (and, especially landmark and concurrent risks analysis).

3. `ml`: different approaches and experiments for finding predictive models for steroid resistance occurrence. There are two boosting realisations (one is R-way and another is python-way), random forest, boruta (for feature selection) and logistic regression (just for fun).

4. `cooked_data`: transformed datasets for use in further analysis

5. In the main folder we can find a docx file with specifications for source datasets and df_creator.rmd file that created the main dataset and cooked datasets, and does EDA.

## Introduction

Different blood and bone marrow disease treatment is a bone marrow transplantation. One of the main adverse effects of such treatment is graft vs host disease (GvHD). This adverse effect appears in 30-70% of patients with allogeneic transplantations. It may be acute (if the disease appears in the first 100 days after transplantation) or chronic (if it appears later). The mortality rate of GvHD is nearly 40% for the acute form and nearly 10% for the chronic one.

One of the main strategies for GvHD treatment is using of glucocorticoids. It is the cheapest treatment with fewer adverse effects than more complicated therapy. Sometimes patients demonstrate refractory to glucocorticoids. It may lead to different consequences: more aggressive therapy, 


## Aims and objectives

1. Searching the potential associations of glucocorticoid refractory.
2. Survival analysis.
3. Creating a predictive model for glucocorticoid refractory occurrence. 

## Data

Data was provided by the Research Institute of Children Oncology, Hematology and Transplantology named after R.M. Gorbacheva.

There were 18 ADAM-like xlsx files (10 source data files and 8 derivatives) which contained data about demography, preventive care, treatment, resistance to glucocorticoids, and other helpful information.

More detailed information about the input data can be found in the [`GVHD_Specification.docx`](https://github.com/Asklepiad/GvHD/blob/main/GVHD_Specification.docx) file in this repository.

## Workflow

The principal scheme of workflow is illustrated in Fig.1

![Fig. 1 Principal scheme of workflow](https://github.com/Asklepiad/GvHD/blob/main/workflow_GvHD.jpg)

1. Creating datasets from ADAM-like files. We created twelve long-form datasets for three levels: four for the treatment level (unit of observation -- one drug prescribtion), four for the disease level (unit of observation -- one diagnosis), four for the patient level (unit of observation -- one person), and ~~one dataset to rull them all~~. Four datasets for every level were common dataset, dataset with acute GVHD cases only, dataset with chronuc GVHD cases only and dataset with cross-syndrome (transitional stage between acute and chronic GVHD).

2. Performing EDA, descriptive statistics and NA analysis.

3. Survival analysis (rate mortality).

4. ML (steroid resistance)

## EDA

No new insights were found after EDA, but we make sure our data have similar properties with other investigations in this field.
As an example -- patients with a more severe gastrointestinal lesion at the time of diagnosis had a worse mortality rate. People with non-relative haplo-matched donors also have worse prognoses by survival and steroid resistance.

More interesting details and figures you can found [after paying for annual subscription](https://boosty.to/bioinf) or in the `df_creation.html` file in the "EDA" section. But subscription is a preferred option.

### Survival analysis


### ML

> Boruta

> Lasso

> Random forest

> Catboost

We tried to implement categorial boosting to predict the fact of resistance. We used the MICE algorithm for imputing missing values, optuna tool for choosing the best hyperparameters, and the catboost itself for evaluating the analysis. The metric we will maximize in the process of training is sensitivity (also widely known by the alias "recall"). Our sensitivity on the test dataset was 0.22. More interesting details can be found in [`the appropriate file`](https://github.com/Asklepiad/GvHD/blob/main/ml/Forest_Gump.ipynb).

Soft
+ Python 3.9.13
+ matplotlib==3.8.0
+ miceforest==5.7.0
+ nafig==1.0.1
+ optuna==3.5.0
+ pandas==1.5.3
+ sklearn==1.3.2
+ seaborn==0.12.2
+ numpy==1.24.2

> HGBM

## Results

## Literature

Placeholder: Uniformity of links

> [1] Martin PJ, Schoch G, Fisher L, Byers V, Anasetti C, Appelbaum FR, Beatty PG, Doney K, McDonald GB, Sanders JE, et al. A retrospective analysis of therapy for acute graft-versus-host disease: initial treatment. Blood. 1990 Oct 15;76(8):1464-72. PMID: 2207321.

> [2] Van Lint, M.T. Early treatment of acute graft-versus-host disease with high- or low-dose 6-methylprednisolone: a multicenter randomized trial from the Italian Group for Bone Marrow Transplantation / M.T. van Lint, C. Uderzo, A. Locasciulli, I. Majolino, R. Scimé et al. // Blood. — 1998 Oct 1. — Vol. 92, № 7. — P. 2288–2293.

> [3] Zhang MY, Zhao P, Zhang Y, Wang JS. Efficacy and safety of ruxolitinib for steroid-refractory graft-versus-host disease: Systematic review and meta-analysis of randomised and non-randomised studies. PLoS One. 2022 Jul 29;17(7):e0271979. doi: 10.1371/journal.pone.0271979. PMID: 35905125; PMCID: PMC9337651.

> [4] Park JH, Lee HJ, Kim SR, Song GW, Lee SK, Park SY, Kim KC, Hwang SH, Park JS. Etanercept for steroid-refractory acute graft versus host disease following allogeneic hematopoietic stem cell transplantation. Korean J Intern Med. 2014 Sep;29(5):630-6. doi: 10.3904/kjim.2014.29.5.630. Epub 2014 Aug 28. PMID: 25228839; PMCID: PMC4164727.

> [5] Saidu NEB, Bonini C, Dickinson A, Grce M, Inngjerdingen M, Koehl U, Toubert A, Zeiser R, Galimberti S. New Approaches for the Treatment of Chronic Graft-Versus-Host Disease: Current Status and Future Directions. Front Immunol. 2020 Oct 9;11:578314. doi: 10.3389/fimmu.2020.578314. PMID: 33162993; PMCID: PMC7583636.

> [6] D’Souza, A. Current Uses and Outcomes of Hematopoietic Cell Transplantation (HCT): CIBMTR Summary Slides / A. D’Souza, C. Fretham — 2017. Available at: www.cibmtr.org, as of 28/07/18

> [7] Carreras E, Dufour C, Mohty M, Kröger N, editors. The EBMT Handbook: Hematopoietic Stem Cell Transplantation and Cellular Therapies [Internet]. 7th ed. Cham (CH): Springer; 2019. PMID: 32091673.

## Authors

Students:
- Ivan Lebedev
- Elizaveta Melnichuk
- Bogdan Sotnikov
- Iuliia Trifonova

Curators:
- Oleg Arnaut
- Nikita Volkov
