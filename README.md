# MitigationHubs-shiny
The Shiny App of the MitigationHubs project.
Visit ![mitigationhubs.shinyapps.io/mitigationhubs-shiny/](https://mitigationhubs.shinyapps.io/mitigationhubs-shiny/) to discover our web app. Take a look at our website ![mitigationhubs.github.io/](https://mitigationhubs.github.io/) for more information on the MitigationHubs project.

## Structure of this repository
`data_\*.R` scripts query the database for current case numbers, request the current version of the reported mitigation measures, preprocess the data and upload the data so that it can be accessed by the online app. Intermediate data will be stored in the `data` directory which contains additional data as well. Authentication is required for uploading the processed data but the processing scripts can be run locally. For running the scripts refer to `data_prepare.R`, the upper-level script.

The folder `MitigationHubs-shiny` contains the actual app which queries the preprocessed data and visualises the results.
