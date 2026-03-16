# adhd-trends
Repository for MSc project. 
Project title: 'Temporal trends in coding of diagnosis and medication prescribing for Attention-Deficit Hyperactivity Disorder in English primary care and hospital admitted care from 2012 to 2024'

# Project description

This project aims to comprehensively review publicly available ADHD data to: 

1. Describe temporal trends and variation in ADHD coding and prescribing activity across ADHD care pathways (including NHS primary care, secondary care, and private care);
2. Identify periods of change in ADHD prescribing activity;
3. Inform future ADHD research using Electronic Health Records by reviewing publicly available data across care settings. 


# Data Setup

To download Secondary Care Medicines Data (SCMD) data from the OpenPrescribing Hospitals website, you will need to manually download from https://hospitals.openprescribing.net/analyse/?vtms=776752004,774531008,775504000,774690000,785373000,776162002&quantity=ddd as follows: 

1. Scroll down to the percentile chart and click on the hamburger menu icon.
2. Click "Download Raw Data"
3. Place the downloaded .zip file in `openprescribing/oph_web_data/` and manually rename it to "oph_web_adhd_meds.zip".

See [openprescribing/get-web-oph-adhd-meds.qmd](openprescribing/get-web-oph-adhd-meds.qmd) for a more detailed explanation and run the code to unzip the file. 
