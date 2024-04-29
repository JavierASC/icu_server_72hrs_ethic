-- Vital signs within the first 24h (1440 min)

-- This query calculates the mean, max, and min values of vital signs per patient within the first 24 hours

-- We are not interested in "chartoffset" (but we use it to filter time), "entryoffset", "temperaturelocation", "ibp_systolic", "ibp_diastolic" and "ibp_mean".

SELECT
  patientunitstayid,
  AVG(heartrate) AS heartrate_mean,
  MAX(heartrate) AS heartrate_max,
  MIN(heartrate) AS heartrate_min,
  AVG(respiratoryrate) AS respiratoryrate_mean,
  MAX(respiratoryrate) AS respiratoryrate_max,
  MIN(respiratoryrate) AS respiratoryrate_min,
  AVG(spo2) AS spo2_mean,
  MAX(spo2) AS spo2_max,
  MIN(spo2) AS spo2_min,
  AVG(nibp_systolic) AS nibp_systolic_mean,
  MAX(nibp_systolic) AS nibp_systolic_max,
  MIN(nibp_systolic) AS nibp_systolic_min,
  AVG(nibp_diastolic) AS nibp_diastolic_mean,
  MAX(nibp_diastolic) AS nibp_diastolic_max,
  MIN(nibp_diastolic) AS nibp_diastolic_min,
  AVG(nibp_mean) AS nibp_mean_mean,
  MAX(nibp_mean) AS nibp_mean_max,
  MIN(nibp_mean) AS nibp_mean_min,
  AVG(temperature) AS temperature_mean,
  MAX(temperature) AS temperature_max,
  MIN(temperature) AS temperature_min
FROM `physionet-data.eicu_crd_derived.pivoted_vital`
WHERE chartoffset >= 0 AND chartoffset <= 2880
GROUP BY patientunitstayid;
