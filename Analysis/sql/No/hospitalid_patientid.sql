-- Query 1
SELECT
  *
FROM 
  `physionet-data.eicu_crd.hospital`;

-- Query 2
SELECT  
  p.patientunitstayid, 
  p.hospitalid,
  h.*
FROM 
  `physionet-data.eicu_crd.patient` AS p
FULL OUTER JOIN
  `physionet-data.eicu_crd.hospital` AS h
ON 
  p.hospitalid = h.hospitalid;