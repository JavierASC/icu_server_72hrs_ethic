-- Calculates ICU mortality between 24 and 72hrs

-- actualiculos is the time between the hospitalization and death, in days, so we are interested in 0 - 3.
-- patientunitstayid could be repeated by error, so we have to choose the maximun actualiculos (time hospitalization to death) and group by patientunitstayid.
-- we are interested only in knowing if the patient is alive or not during the first 3 days of hospitalization, so we can use a column with 0 and 1 to show that. This is possible with CASE WHEN.
-- We decide:  0 = ALIVE and 1 = EXPIRED

SELECT
  patientunitstayid, actualiculos,
  MAX(CASE WHEN actualICULOS BETWEEN 0 AND 3 AND actualICUMortality = 'EXPIRED' THEN 1
    ELSE 0 END) AS icu_mortality_72hrs
FROM
  `physionet-data.eicu_crd.apachepatientresult`
GROUP BY
  patientunitstayid, actualiculos
