SELECT
  p.patientunitstayid, 
  p.hospitalid AS patient_hospitalid,
  h.hospitalid AS hospital_hospitalid,
  h.*
FROM 
  `physionet-data.eicu_crd.patient` AS p
LEFT JOIN
  `physionet-data.eicu_crd.hospital` AS h
ON 
  p.hospitalid = h.hospitalid

UNION ALL

SELECT
  p.patientunitstayid, 
  p.hospitalid AS patient_hospitalid,
  h.hospitalid AS hospital_hospitalid,
  h.*
FROM 
  `physionet-data.eicu_crd.patient` AS p
RIGHT JOIN
  `physionet-data.eicu_crd.hospital` AS h
ON 
  p.hospitalid = h.hospitalid
WHERE 
  p.hospitalid IS NULL;