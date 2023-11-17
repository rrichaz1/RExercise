####+####+####+####+####+####+####+####+####+####+####+####+####+####+####+####+

# Installs all packages used in the DSU curriculum
# Marc d. Paradis
# R 3.5.1 for Windows
# RStudio 1.1.456

# WHEN UPDATING R, ALWAYS UNINSTALL PREVIOUS VERSIONS OF R BEFORE INSTALLING 
# NEW ONES

# When updating RStudio, install over existing RStudio in order to preserve 
# settings 

# Note that you may have to specify a CRAN Mirror for package installation and
# updates. RStudio -> Tools -> Global Options -> Packages -> 
# CRAN Mirror : Change -> Any mirror in the U.S.

# Upgrading R often erases all previously loaded packages. You many find that
# you have to run this script after every R upgrade.

# Note that after succesfful RStudio upgrade you should update all base
# packages

# For info on individual packages, click the Packages tab in the lower-right
# container to see a short description. Click the package name to open the
# online docs for that package.

# Following each install.packages command, the desired result will be shown
# in the form of comments

# DATA VISUALIZATION

install.packages('ggplot2')

install.packages('googleVis')

# DATA TRANSFORMATION

install.packages('dplyr')  #already installed

install.packages('data.table')

# DATA IMPUTATION

install.packages('missForest')

install.packages('missMDA')

# OUTLIER DETECTION

install.packages('outliers')

# https://cran.r-project.org/web/packages/outliers/outliers.pdf

install.packages('evir')

# FEATURE SELECTION

install.packages('features')

install.packages('RRF')

# DIMENSION REDUCTION

install.packages('CCP')

# CONTINUOUS REGRESSION

install.packages('car') #already installed

install.packages('randomForest') #already installed

# ORDINAL REGRESSION

install.packages('rminer')

install.packages('CORElearn')

# CLASSIFICATION

install.packages('caret')

# CLUSTERING

install.packages('cba')

install.packages('Rankcluster')

# package 'Rankcluster' successfully unpacked and MD5 sums checked

# TIME SERIES

install.packages('forecast')

install.packages('ltsa')

# SURVIVAL

install.packages('survival')

install.packages('BaSTA')

# GENERAL MODEL VALIDATION

install.packages('lsmeans')

install.packages('comparison')

# REGRESSION VALIDATION

install.packages('regtest')

install.packages('ACD')

# CLASSIFICATION VALIDATION

install.packages('Daim')

# CLUSTERING VALIDATION

install.packages('clusteval')

install.packages('sigclust')

# ROC ANALYSIS

install.packages('pROC')

install.packages('timeROC')

# IMPROVE PERFORMANCE

install.packages('Rcpp') #already installed

install.packages('parallel') #already installed

# WORK WITH WEB

install.packages('XML') 

install.packages('jsonlite') #already installed

install.packages('httr')

# REPORT RESULTS

install.packages('shiny')

install.packages('rmarkdown')

# TEXT MINING

install.packages('tm')

install.packages('twitteR')

# DATABASE

install.packages('sqldf')

install.packages('RODBC')

install.packages('RMongo')

# MISCELLANEOUS

install.packages('swirl')

# install.packages('reshape2')

install.packages('qcc')

# KDNUGGETS RECOMMENDS

install.packages('manipulate')

# RStudio Recommends

install.packages('RMySQL')

install.packages('RSQLite')

install.packages('RPostgreSQL')

install.packages('XLConnect')

install.packages('xlsx')

install.packages('ggvis')

install.packages('rgl')

install.packages('vcd')

install.packages('sp')

install.packages('maptools')

install.packages('maps')

install.packages('ggmap')

install.packages('devtools')

install.packages('roxygen2')

# REVOLUTION ANALYTICS RECOMMENDS

install.packages('Hmisc')

# MACHINE LEARNING METHODS

install.packages('psych') #already installed as part of 1.2.1 script

install.packages('MatchIt')

install.packages('lsr')

install.packages('moments')

install.packages('vioplot')

install.packages('usdm')

install.packages('rmutil')

install.packages('RANN')

install.packages('philentropy')

install.packages('fGarch')

install.packages('mclust')

install.packages("np", dependencies=T)

install.packages("earth", dependencies=T)

install.packages("effects")

install.packages('MASS')

install.packages('factoextra')

install.packages('jmv')

install.packages('rpart.plot')

install.packages('inTrees')

install.packages('Rborist')

install.packages('Boruta')

install.packages('arulesViz')

install.packages('faraway')

install.packages('ggfortify')

install.packages('gvlma')

install.packages('clv')

install.packages('clValid')

install.packages('dbscan')

install.packages('recommenderlab')

install.packages('klaR')

install.packages('depmixS4')

install.packages('bnlearn')

install.packages('R.utils')

install.packages('mlogit')

install.packages('DescTools')

# UPDATE ALL
# Remember to check for package updates at least once/month and to update all,
# unless you are in production, in which case, manage updates carefully

# END