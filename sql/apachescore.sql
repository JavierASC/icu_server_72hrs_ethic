--df apachescore, Severity Level

SELECT patientunitstayid,MAX(apachescore) AS apachescoreIV
FROM `physionet-data.eicu_crd.apachepatientresult` 
GROUP BY patientunitstayid
