# Machine-learning-knn

## Problem to Address
 Identifying spam emails is one important classification task. Install kernlab library in R and use the spam data in the library. You can find the description of the data by ?spam.
• Randomly select 1) 100 spam emails and 100 nonspam emails as the training set and 2) 50 spam emails and 50 nonspam emails as the test set. Make sure that the emails in the training set and the test set are distinct, i.e. no repeated email instances.
• Use 1NN, 9NN and 25NN to classify the test set.
• Comment on the results you obtained.
• Make sure that your results are reproducible.

## Directory Structure
``` bash
machine-learning-knn
├── LICENSE
├── README.md
└── scripts
    ├── 00_analysis_code.R
    ├── analysis_document
    │   └── _00_analysis_doc.pdf
    └── viz_accuracy
        ├── scaled_features_acc.pdf
        └── unscaled_features_acc.pdf. 
```
## File Description

### scripts
* ``` 00_analysis_code.R ``` -> Data preperation and processing and analysis
### analysis_document
* ``` _00_analysis_doc.pdf ``` -> Companion document for the obtained result
### viz_accuracy
* ``` scaled_features_acc.pdf ``` -> Accuracy plot for scaled data, comparing test and train data set
* ``` unscaled_features_acc.pdf ``` -> Accuracy plot for unscaled data, comparing test and train data set
