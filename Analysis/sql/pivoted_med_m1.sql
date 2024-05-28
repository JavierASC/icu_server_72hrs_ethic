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
  chartoffset >= -1440
  AND chartoffset < 0
GROUP BY
  patientunitstayid;
