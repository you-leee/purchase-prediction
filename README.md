# purchase-prediction

This is a simple project for predicting the purchases of customers from historical (2 years) data. It also contains documentations and scripts of data exploration and on different approaches to handle this problem.

### Structure
 - /data: raw and preprocessed data, final model
 - /docs: documentation and R sources of the whole workflow (data exploration and different approaches, models used)
 - /R: R sources
 - RevenuePredictor.R: main file

### Usage
Go to the root directory of the project and set it as your "Working Directory". Source the `RevenuePredictor.R` file (this may take a while). This will install and load the required packages, load the preprocessed data and trained model and source 2 provided function, one for testing predicting.
For more information of these function look at the source of the `RevenuePredictor.R` file, where the documentation of these can be found.

    setwd(rootFolder)
    source("RevenuePredictor.R")
    
    # Test
    testRevenue(c("18759585", "8139020", "41680290", "13147861", "48751655"))
    
    # Predict
    predictRevenue(c("26762879", "42993805", "8578048"))
