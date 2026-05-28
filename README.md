# adhd-trends
Repository for Masters project. 
Project title: "Temporal trends in coded diagnoses and medications for Attention-Deficit Hyperactivity Disorder across English care pathways from 2011 to 2025".

# Project description

Note that author names and usernames have been redacted for blind review. As a result, code depending on functional codelist links will not work. Author names and usernames will be restored in late June 2026 after examination. 

This project aims to comprehensively review publicly available ADHD data to: 

1. Describe and evaluate publicly available data sources across ADHD care settings, assess their suitability and limitations for research, and contribute to open-source tools that make these data more accessible and easier to work with for future researchers;
2. Describe temporal trends and variation in ADHD diagnosis and medication coding across ADHD care pathways (including NHS primary, secondary, and private care);
3. Identify significant periods of change in ADHD medication coding. 


# Data Setup

The open source tools `opencodecounts`, openprescribing.net, and hospitals.openprescribing.net are developed and maintained by the Bennett Institute of Applied Data Science at the Department of Primary Care Health Sciences, University of Oxford, where this project was conducted.

## SNOMED CT code usage in primary care

You can obtain yearly aggregated clinical code usage (SNOMED CT) in primary care through `opencodecounts` (https://bennettoxford.github.io/opencodecounts/reference/index.html), or run `adhd_dx_coding_results.qmd`.   

## ICD-10 code usage and breakdowns in hospital admitted patient care

You can obtain yearly aggregated diagnosed code usage (ICD-10) in hospital admitted patient care through `opencodecounts`. Version 0.5.0 is required for age, sex, and diagnosis position breakdowns for ICD-10 coding. Running `adhd_dx_coding_results.qmd` will give you the same information for ADHD coding. 

## English Prescribing Data (GP prescribing data)

Run `opr.R` to download GP prescribing data from OpenPrescribing (?)
You can download raw English Prescribing Data from the (NHSBSA website)[https://opendata.nhsbsa.net/dataset/english-prescribing-dataset-epd-with-snomed-code], or access GP prescribing data for the last five years through (OpenPrescribing)[https://openprescribing.net/]. 

## FOI Prescribing Data

Run `get_foi_data.R` to download NHS Business Services Authority responses to FOI requests for private controlled drug ADHD prescriptions from the FOI disclosure log.

## Secondary Care Medicines Data (SCMD data)

To download Secondary Care Medicines Data (SCMD) data from the OpenPrescribing Hospitals website, you will need to manually download from https://hospitals.openprescribing.net/analyse/?vtms=776752004,774531008,775504000,774690000,785373000,776162002&quantity=ddd. We describe our process in more detail in `openprescribing/get-web-oph-adhd-meds.qmd`, here is a short summary: 

1. Scroll down to the percentile chart and click on the hamburger menu icon.
2. Click "Download Raw Data"
3. Place the downloaded .zip file in `openprescribing/oph_web_data/`.

See `openprescribing/get-web-oph-adhd-meds.qmd` for a more detailed explanation and run the code to unzip the file. 
