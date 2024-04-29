--Apache has been removed as a new severity score will be created 

-- gender -> male: 1 ó 0; female: 1 ó 0
-- hosp-mortality -> alive or dead, 1 ó 0
-- icu_los_hours ???

SELECT  
        patientunitstayid, 
        age,
        gender AS sex,
        hosp_mortality
FROM `physionet-data.eicu_crd_derived.basic_demographics`
