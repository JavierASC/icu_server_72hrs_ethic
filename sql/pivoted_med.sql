-- Amount of drugs recieved during the first 24h (1440 min)

-- We are not interested in "drugorderoffset", "drugstopoffset" and "chartoffset" (but we will use it to filter).

-- Patients could have recieved different amount of drug (different lines for the same ID), so we are interested in the minimum, the maximum and the mean of all drugs recieved.

-- It has been filtered to obtain the information of the first 24 hours. "chartoffset" is the time after patient hospitalization in minutes. We are interested in 0 - 1440 min.

SELECT
  patientunitstayid,
  MAX(norepinephrine) AS norepinephrine,
  MAX(epinephrine) AS epinephrine,
  MAX(dopamine) AS dopamine,
  MAX(dobutamine) AS dobutamine,
  MAX(phenylephrine) AS phenylephrine,
  MAX(vasopressin) AS vasopressin,
  MAX(milrinone) AS milrinone,
  MAX(heparin) AS heparin,
  MAX(warfarin) AS warfarin
FROM
  `physionet-data.eicu_crd_derived.pivoted_med`
WHERE
  chartoffset >= 0
  AND chartoffset <= 2880
GROUP BY
  patientunitstayid;
