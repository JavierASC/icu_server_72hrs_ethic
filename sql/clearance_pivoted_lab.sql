WITH ChartoffsetStatsAlbumin AS (
  SELECT
    patientunitstayid,
    MIN(chartoffset) AS albumin_first_chartoffset,
    MAX(chartoffset) AS albumin_last_chartoffset
  FROM `physionet-data.eicu_crd_derived.pivoted_lab`
  WHERE chartoffset >= 0 AND chartoffset <= 2880 AND albumin IS NOT NULL
  GROUP BY patientunitstayid
),
ChartoffsetStatsBilirubin AS (
  SELECT
    patientunitstayid,
    MIN(chartoffset) AS bilirubin_first_chartoffset,
    MAX(chartoffset) AS bilirubin_last_chartoffset
  FROM `physionet-data.eicu_crd_derived.pivoted_lab`
  WHERE chartoffset >= 0 AND chartoffset <= 2880 AND bilirubin IS NOT NULL
  GROUP BY patientunitstayid
),
ChartoffsetStatsBUN AS (
  SELECT
    patientunitstayid,
    MIN(chartoffset) AS BUN_first_chartoffset,
    MAX(chartoffset) AS BUN_last_chartoffset
  FROM `physionet-data.eicu_crd_derived.pivoted_lab`
  WHERE chartoffset >= 0 AND chartoffset <= 2880 AND BUN IS NOT NULL
  GROUP BY patientunitstayid
),
ChartoffsetStatsCalcium AS (
  SELECT
    patientunitstayid,
    MIN(chartoffset) AS calcium_first_chartoffset,
    MAX(chartoffset) AS calcium_last_chartoffset
  FROM `physionet-data.eicu_crd_derived.pivoted_lab`
  WHERE chartoffset >= 0 AND chartoffset <= 2880 AND calcium IS NOT NULL
  GROUP BY patientunitstayid
),
ChartoffsetStatsCreatinine AS (
  SELECT
    patientunitstayid,
    MIN(chartoffset) AS creatinine_first_chartoffset,
    MAX(chartoffset) AS creatinine_last_chartoffset
  FROM `physionet-data.eicu_crd_derived.pivoted_lab`
  WHERE chartoffset >= 0 AND chartoffset <= 2880 AND creatinine IS NOT NULL
  GROUP BY patientunitstayid
),
ChartoffsetStatsGlucose AS (
  SELECT
    patientunitstayid,
    MIN(chartoffset) AS glucose_first_chartoffset,
    MAX(chartoffset) AS glucose_last_chartoffset
  FROM `physionet-data.eicu_crd_derived.pivoted_lab`
  WHERE chartoffset >= 0 AND chartoffset <= 2880 AND glucose IS NOT NULL
  GROUP BY patientunitstayid
),
ChartoffsetStatsBicarbonate AS (
  SELECT
    patientunitstayid,
    MIN(chartoffset) AS bicarbonate_first_chartoffset,
    MAX(chartoffset) AS bicarbonate_last_chartoffset
  FROM `physionet-data.eicu_crd_derived.pivoted_lab`
  WHERE chartoffset >= 0 AND chartoffset <= 2880 AND bicarbonate IS NOT NULL
  GROUP BY patientunitstayid
),
ChartoffsetStatsTotalCO2 AS (
  SELECT
    patientunitstayid,
    MIN(chartoffset) AS TotalCO2_first_chartoffset,
    MAX(chartoffset) AS TotalCO2_last_chartoffset
  FROM `physionet-data.eicu_crd_derived.pivoted_lab`
  WHERE chartoffset >= 0 AND chartoffset <= 2880 AND TotalCO2 IS NOT NULL
  GROUP BY patientunitstayid
),
ChartoffsetStatsHematocrit AS (
  SELECT
    patientunitstayid,
    MIN(chartoffset) AS hematocrit_first_chartoffset,
    MAX(chartoffset) AS hematocrit_last_chartoffset
  FROM `physionet-data.eicu_crd_derived.pivoted_lab`
  WHERE chartoffset >= 0 AND chartoffset <= 2880 AND hematocrit IS NOT NULL
  GROUP BY patientunitstayid
),
ChartoffsetStatsHemoglobin AS (
  SELECT
    patientunitstayid,
    MIN(chartoffset) AS hemoglobin_first_chartoffset,
    MAX(chartoffset) AS hemoglobin_last_chartoffset
  FROM `physionet-data.eicu_crd_derived.pivoted_lab`
  WHERE chartoffset >= 0 AND chartoffset <= 2880 AND hemoglobin IS NOT NULL
  GROUP BY patientunitstayid
),
ChartoffsetStatsINR AS (
  SELECT
    patientunitstayid,
    MIN(chartoffset) AS INR_first_chartoffset,
    MAX(chartoffset) AS INR_last_chartoffset
  FROM `physionet-data.eicu_crd_derived.pivoted_lab`
  WHERE chartoffset >= 0 AND chartoffset <= 2880 AND INR IS NOT NULL
  GROUP BY patientunitstayid
),
ChartoffsetStatsLactate AS (
  SELECT
    patientunitstayid,
    MIN(chartoffset) AS lactate_first_chartoffset,
    MAX(chartoffset) AS lactate_last_chartoffset
  FROM `physionet-data.eicu_crd_derived.pivoted_lab`
  WHERE chartoffset >= 0 AND chartoffset <= 2880 AND lactate IS NOT NULL
  GROUP BY patientunitstayid
),
ChartoffsetStatsPlatelets AS (
  SELECT
    patientunitstayid,
    MIN(chartoffset) AS platelets_first_chartoffset,
    MAX(chartoffset) AS platelets_last_chartoffset
  FROM `physionet-data.eicu_crd_derived.pivoted_lab`
  WHERE chartoffset >= 0 AND chartoffset <= 2880 AND platelets IS NOT NULL
  GROUP BY patientunitstayid
),
ChartoffsetStatsPotassium AS (
  SELECT
    patientunitstayid,
    MIN(chartoffset) AS potassium_first_chartoffset,
    MAX(chartoffset) AS potassium_last_chartoffset
  FROM `physionet-data.eicu_crd_derived.pivoted_lab`
  WHERE chartoffset >= 0 AND chartoffset <= 2880 AND potassium IS NOT NULL
  GROUP BY patientunitstayid
),
ChartoffsetStatsPtt AS (
  SELECT
    patientunitstayid,
    MIN(chartoffset) AS ptt_first_chartoffset,
    MAX(chartoffset) AS ptt_last_chartoffset
  FROM `physionet-data.eicu_crd_derived.pivoted_lab`
  WHERE chartoffset >= 0 AND chartoffset <= 2880 AND ptt IS NOT NULL
  GROUP BY patientunitstayid
),
ChartoffsetStatsSodium AS (
  SELECT
    patientunitstayid,
    MIN(chartoffset) AS sodium_first_chartoffset,
    MAX(chartoffset) AS sodium_last_chartoffset
  FROM `physionet-data.eicu_crd_derived.pivoted_lab`
  WHERE chartoffset >= 0 AND chartoffset <= 2880 AND sodium IS NOT NULL
  GROUP BY patientunitstayid
),
ChartoffsetStatsWbc AS (
  SELECT
    patientunitstayid,
    MIN(chartoffset) AS wbc_first_chartoffset,
    MAX(chartoffset) AS wbc_last_chartoffset
  FROM `physionet-data.eicu_crd_derived.pivoted_lab`
  WHERE chartoffset >= 0 AND chartoffset <= 2880 AND wbc IS NOT NULL
  GROUP BY patientunitstayid
),
ChartoffsetStatsBands AS (
  SELECT
    patientunitstayid,
    MIN(chartoffset) AS bands_first_chartoffset,
    MAX(chartoffset) AS bands_last_chartoffset
  FROM `physionet-data.eicu_crd_derived.pivoted_lab`
  WHERE chartoffset >= 0 AND chartoffset <= 2880 AND bands IS NOT NULL
  GROUP BY patientunitstayid
),
ChartoffsetStatsAlt AS (
  SELECT
    patientunitstayid,
    MIN(chartoffset) AS alt_first_chartoffset,
    MAX(chartoffset) AS alt_last_chartoffset
  FROM `physionet-data.eicu_crd_derived.pivoted_lab`
  WHERE chartoffset >= 0 AND chartoffset <= 2880 AND alt IS NOT NULL
  GROUP BY patientunitstayid
),
ChartoffsetStatsAst AS (
  SELECT
    patientunitstayid,
    MIN(chartoffset) AS ast_first_chartoffset,
    MAX(chartoffset) AS ast_last_chartoffset
  FROM `physionet-data.eicu_crd_derived.pivoted_lab`
  WHERE chartoffset >= 0 AND chartoffset <= 2880 AND ast IS NOT NULL
  GROUP BY patientunitstayid
),
ChartoffsetStatsAlp AS (
  SELECT
    patientunitstayid,
    MIN(chartoffset) AS alp_first_chartoffset,
    MAX(chartoffset) AS alp_last_chartoffset
  FROM `physionet-data.eicu_crd_derived.pivoted_lab`
  WHERE chartoffset >= 0 AND chartoffset <= 2880 AND alp IS NOT NULL
  GROUP BY patientunitstayid
),
patientdistinct AS (
  SELECT DISTINCT patientunitstayid
  FROM `physionet-data.eicu_crd.patient`
)
SELECT
  patientdistinct.patientunitstayid,
  a.albumin_first_chartoffset,
  (
    SELECT ARRAY_AGG(albumin)
    FROM `physionet-data.eicu_crd_derived.pivoted_lab` p1
    WHERE p1.patientunitstayid = a.patientunitstayid AND p1.chartoffset = a.albumin_first_chartoffset
  ) [OFFSET(0)] AS albumin_first,
  a.albumin_last_chartoffset,
   CASE
   WHEN a.albumin_first_chartoffset = a.albumin_last_chartoffset THEN NULL
  ELSE(
    SELECT ARRAY_AGG(albumin)
    FROM `physionet-data.eicu_crd_derived.pivoted_lab` p2
    WHERE p2.patientunitstayid = a.patientunitstayid AND p2.chartoffset = a.albumin_last_chartoffset
  ) [OFFSET(0)] END AS albumin_last,
    (
    SELECT ARRAY_AGG(albumin)
    FROM `physionet-data.eicu_crd_derived.pivoted_lab` p3
    WHERE p3.patientunitstayid = a.patientunitstayid AND p3.chartoffset = a.albumin_last_chartoffset
  ) [OFFSET(0)] - (
    SELECT ARRAY_AGG(albumin)
    FROM `physionet-data.eicu_crd_derived.pivoted_lab` p4
    WHERE p4.patientunitstayid = a.patientunitstayid AND p4.chartoffset = a.albumin_first_chartoffset
  ) [OFFSET(0)] AS albumin_clear,
  b.bilirubin_first_chartoffset,
  (
    SELECT ARRAY_AGG(bilirubin)
    FROM `physionet-data.eicu_crd_derived.pivoted_lab` p5
    WHERE p5.patientunitstayid = b.patientunitstayid AND p5.chartoffset = b.bilirubin_first_chartoffset
  ) [OFFSET(0)] AS bilirubin_first,
  b.bilirubin_last_chartoffset,
   CASE
   WHEN b.bilirubin_first_chartoffset =  b.bilirubin_last_chartoffset THEN NULL
  ELSE(
    SELECT ARRAY_AGG(bilirubin)
    FROM `physionet-data.eicu_crd_derived.pivoted_lab` p6
    WHERE p6.patientunitstayid = b.patientunitstayid AND p6.chartoffset = b.bilirubin_last_chartoffset
  ) [OFFSET(0)] END AS bilirubin_last,
    (
    SELECT ARRAY_AGG(bilirubin)
    FROM `physionet-data.eicu_crd_derived.pivoted_lab` p7
    WHERE p7.patientunitstayid = b.patientunitstayid AND p7.chartoffset = b.bilirubin_last_chartoffset
  ) [OFFSET(0)] - (
    SELECT ARRAY_AGG(bilirubin)
    FROM `physionet-data.eicu_crd_derived.pivoted_lab` p8
    WHERE p8.patientunitstayid = b.patientunitstayid AND p8.chartoffset = b.bilirubin_first_chartoffset
  ) [OFFSET(0)] AS bilirubin_clear,
  c.BUN_first_chartoffset,
  (
    SELECT ARRAY_AGG(BUN)
    FROM `physionet-data.eicu_crd_derived.pivoted_lab` p9
    WHERE p9.patientunitstayid = c.patientunitstayid AND p9.chartoffset = c.BUN_first_chartoffset
  ) [OFFSET(0)] AS BUN_first,
  c.BUN_last_chartoffset,
   CASE
   WHEN c.BUN_first_chartoffset =  c.BUN_last_chartoffset THEN NULL
  ELSE(
    SELECT ARRAY_AGG(BUN)
    FROM `physionet-data.eicu_crd_derived.pivoted_lab` p10
    WHERE p10.patientunitstayid = c.patientunitstayid AND p10.chartoffset = c.BUN_last_chartoffset
  ) [OFFSET(0)] END AS BUN_last,
    (
    SELECT ARRAY_AGG(BUN)
    FROM `physionet-data.eicu_crd_derived.pivoted_lab` p11
    WHERE p11.patientunitstayid = c.patientunitstayid AND p11.chartoffset = c.BUN_last_chartoffset
  ) [OFFSET(0)] - (
    SELECT ARRAY_AGG(BUN)
    FROM `physionet-data.eicu_crd_derived.pivoted_lab` p12
    WHERE p12.patientunitstayid = c.patientunitstayid AND p12.chartoffset = c.BUN_first_chartoffset
  ) [OFFSET(0)] AS BUN_clear,
  d.calcium_first_chartoffset,
  (
    SELECT ARRAY_AGG(calcium)
    FROM `physionet-data.eicu_crd_derived.pivoted_lab` p13
    WHERE p13.patientunitstayid = d.patientunitstayid AND p13.chartoffset = d.calcium_first_chartoffset
  ) [OFFSET(0)] AS calcium_first,
  d.calcium_last_chartoffset,
  CASE
   WHEN d.calcium_first_chartoffset =  d.calcium_last_chartoffset THEN NULL
  ELSE(
    SELECT ARRAY_AGG(calcium)
    FROM `physionet-data.eicu_crd_derived.pivoted_lab` p14
    WHERE p14.patientunitstayid = d.patientunitstayid AND p14.chartoffset = d.calcium_last_chartoffset
  ) [OFFSET(0)] END AS calcium_last,
    (
    SELECT ARRAY_AGG(calcium)
    FROM `physionet-data.eicu_crd_derived.pivoted_lab` p15
    WHERE p15.patientunitstayid = d.patientunitstayid AND p15.chartoffset = d.calcium_last_chartoffset
  ) [OFFSET(0)] - (
    SELECT ARRAY_AGG(calcium)
    FROM `physionet-data.eicu_crd_derived.pivoted_lab` p16
    WHERE p16.patientunitstayid = d.patientunitstayid AND p16.chartoffset = d.calcium_first_chartoffset
  ) [OFFSET(0)] AS calcium_clear,
  e.creatinine_first_chartoffset,
  (
     SELECT ARRAY_AGG(creatinine)
    FROM `physionet-data.eicu_crd_derived.pivoted_lab` p17
    WHERE p17.patientunitstayid = e.patientunitstayid AND p17.chartoffset = e.creatinine_first_chartoffset
  ) [OFFSET(0)] AS creatinine_first,
  e.creatinine_last_chartoffset,
  CASE
   WHEN  e.creatinine_first_chartoffset = e.creatinine_last_chartoffset THEN NULL
  ELSE(
    SELECT ARRAY_AGG(creatinine)
    FROM `physionet-data.eicu_crd_derived.pivoted_lab` p18
    WHERE p18.patientunitstayid = e.patientunitstayid AND p18.chartoffset = e.creatinine_last_chartoffset
  ) [OFFSET(0)] END AS creatinine_last,
    (
    SELECT ARRAY_AGG(creatinine)
    FROM `physionet-data.eicu_crd_derived.pivoted_lab` p19
    WHERE p19.patientunitstayid = e.patientunitstayid AND p19.chartoffset = e.creatinine_last_chartoffset
  ) [OFFSET(0)] - (
    SELECT ARRAY_AGG(creatinine)
    FROM `physionet-data.eicu_crd_derived.pivoted_lab` p20
    WHERE p20.patientunitstayid = e.patientunitstayid AND p20.chartoffset = e.creatinine_first_chartoffset
  ) [OFFSET(0)] AS creatinine_clear,
  f.glucose_first_chartoffset,
  (
    SELECT ARRAY_AGG(glucose)
    FROM `physionet-data.eicu_crd_derived.pivoted_lab` p21
    WHERE p21.patientunitstayid = f.patientunitstayid AND p21.chartoffset = f.glucose_first_chartoffset
  ) [OFFSET(0)] AS glucose_first,
  f.glucose_last_chartoffset,
   CASE
   WHEN  f.glucose_first_chartoffset =  f.glucose_last_chartoffset THEN NULL
  ELSE(
    SELECT ARRAY_AGG(glucose)
    FROM `physionet-data.eicu_crd_derived.pivoted_lab` p22
    WHERE p22.patientunitstayid = f.patientunitstayid AND p22.chartoffset = f.glucose_last_chartoffset
  ) [OFFSET(0)] END AS glucose_last,
    (
    SELECT ARRAY_AGG(glucose)
    FROM `physionet-data.eicu_crd_derived.pivoted_lab` p23
    WHERE p23.patientunitstayid = f.patientunitstayid AND p23.chartoffset = f.glucose_last_chartoffset
  ) [OFFSET(0)] - (
    SELECT ARRAY_AGG(glucose)
    FROM `physionet-data.eicu_crd_derived.pivoted_lab` p24
    WHERE p24.patientunitstayid = f.patientunitstayid AND p24.chartoffset = f.glucose_first_chartoffset
  ) [OFFSET(0)] AS glucose_clear,
  g.bicarbonate_first_chartoffset,
  (
    SELECT ARRAY_AGG(bicarbonate)
    FROM `physionet-data.eicu_crd_derived.pivoted_lab` p25
    WHERE p25.patientunitstayid = g.patientunitstayid AND p25.chartoffset = g.bicarbonate_first_chartoffset
  ) [OFFSET(0)] AS bicarbonate_first,
  g.bicarbonate_last_chartoffset,
  CASE
   WHEN  g.bicarbonate_first_chartoffset = g.bicarbonate_last_chartoffset THEN NULL
  ELSE(
    SELECT ARRAY_AGG(bicarbonate)
    FROM `physionet-data.eicu_crd_derived.pivoted_lab` p26
    WHERE p26.patientunitstayid = g.patientunitstayid AND p26.chartoffset = g.bicarbonate_last_chartoffset
  )[OFFSET(0)] END AS bicarbonate_last,
    (
    SELECT ARRAY_AGG(bicarbonate)
    FROM `physionet-data.eicu_crd_derived.pivoted_lab` p27
    WHERE p27.patientunitstayid = g.patientunitstayid AND p27.chartoffset = g.bicarbonate_last_chartoffset
  ) [OFFSET(0)] - (
    SELECT ARRAY_AGG(bicarbonate)
    FROM `physionet-data.eicu_crd_derived.pivoted_lab` p28
    WHERE p28.patientunitstayid = g.patientunitstayid AND p28.chartoffset = g.bicarbonate_first_chartoffset
  ) [OFFSET(0)] AS bicarbonate_clear,
  h.TotalCO2_first_chartoffset,
  (
    SELECT ARRAY_AGG(TotalCO2)
    FROM `physionet-data.eicu_crd_derived.pivoted_lab` p29
    WHERE p29.patientunitstayid = h.patientunitstayid AND p29.chartoffset = h.TotalCO2_first_chartoffset
  )[OFFSET(0)] AS TotalCO2_first,
  h.TotalCO2_last_chartoffset,
  CASE
   WHEN h.TotalCO2_first_chartoffset = h.TotalCO2_last_chartoffset THEN NULL
  ELSE(
    SELECT ARRAY_AGG(TotalCO2)
    FROM `physionet-data.eicu_crd_derived.pivoted_lab` p30
    WHERE p30.patientunitstayid = h.patientunitstayid AND p30.chartoffset = h.TotalCO2_last_chartoffset
  )[OFFSET(0)] END AS TotalCO2_last,
    (
    SELECT ARRAY_AGG(TotalCO2)
    FROM `physionet-data.eicu_crd_derived.pivoted_lab` p31
    WHERE p31.patientunitstayid = h.patientunitstayid AND p31.chartoffset = h.TotalCO2_last_chartoffset
  ) [OFFSET(0)] - (
    SELECT ARRAY_AGG(TotalCO2)
    FROM `physionet-data.eicu_crd_derived.pivoted_lab` p32
    WHERE p32.patientunitstayid = h.patientunitstayid AND p32.chartoffset = h.TotalCO2_first_chartoffset
  ) [OFFSET(0)] AS TotalCO2_clear,
  i.hematocrit_first_chartoffset,
  (
    SELECT ARRAY_AGG(hematocrit)
    FROM `physionet-data.eicu_crd_derived.pivoted_lab` p33
    WHERE p33.patientunitstayid = i.patientunitstayid AND p33.chartoffset = i.hematocrit_first_chartoffset
  )[OFFSET(0)] AS hematocrit_first,
  i.hematocrit_last_chartoffset,
   CASE
   WHEN  i.hematocrit_first_chartoffset =  i.hematocrit_last_chartoffset THEN NULL
  ELSE(
    SELECT ARRAY_AGG(hematocrit)
    FROM `physionet-data.eicu_crd_derived.pivoted_lab` p34
    WHERE p34.patientunitstayid = i.patientunitstayid AND p34.chartoffset = i.hematocrit_last_chartoffset
  )[OFFSET(0)] END AS hematocrit_last,
    (
    SELECT ARRAY_AGG(hematocrit)
    FROM `physionet-data.eicu_crd_derived.pivoted_lab` p35
    WHERE p35.patientunitstayid = i.patientunitstayid AND p35.chartoffset = i.hematocrit_last_chartoffset
  ) [OFFSET(0)] - (
    SELECT ARRAY_AGG(hematocrit)
    FROM `physionet-data.eicu_crd_derived.pivoted_lab` p36
    WHERE p36.patientunitstayid = i.patientunitstayid AND p36.chartoffset = i.hematocrit_first_chartoffset
  ) [OFFSET(0)] AS hematocrit_clear,
  j.hemoglobin_first_chartoffset,
  (
    SELECT ARRAY_AGG(hemoglobin)
    FROM `physionet-data.eicu_crd_derived.pivoted_lab` p37
    WHERE p37.patientunitstayid = j.patientunitstayid AND p37.chartoffset = j.hemoglobin_first_chartoffset
  )[OFFSET(0)] AS hemoglobin_first,
  j.hemoglobin_last_chartoffset,
  CASE
   WHEN  j.hemoglobin_first_chartoffset = j.hemoglobin_last_chartoffset THEN NULL
  ELSE(
    SELECT ARRAY_AGG(hemoglobin)
    FROM `physionet-data.eicu_crd_derived.pivoted_lab` p38
    WHERE p38.patientunitstayid = j.patientunitstayid AND p38.chartoffset = j.hemoglobin_last_chartoffset
  )[OFFSET(0)] END AS hemoglobin_last,
    (
    SELECT ARRAY_AGG(hemoglobin)
    FROM `physionet-data.eicu_crd_derived.pivoted_lab` p39
    WHERE p39.patientunitstayid = j.patientunitstayid AND p39.chartoffset = j.hemoglobin_last_chartoffset
  ) [OFFSET(0)] - (
    SELECT ARRAY_AGG(hemoglobin)
    FROM `physionet-data.eicu_crd_derived.pivoted_lab` p40
    WHERE p40.patientunitstayid = j.patientunitstayid AND p40.chartoffset = j.hemoglobin_first_chartoffset
  ) [OFFSET(0)] AS hemoglobin_clear,
  k.INR_first_chartoffset,
  (
    SELECT ARRAY_AGG(INR)
    FROM `physionet-data.eicu_crd_derived.pivoted_lab` p41
    WHERE p41.patientunitstayid = k.patientunitstayid AND p41.chartoffset = k.INR_first_chartoffset
  )[OFFSET(0)] AS INR_first,
  k.INR_last_chartoffset,
  CASE
   WHEN  k.INR_first_chartoffset =  k.INR_last_chartoffset THEN NULL
  ELSE(
    SELECT ARRAY_AGG(INR)
    FROM `physionet-data.eicu_crd_derived.pivoted_lab` p42
    WHERE p42.patientunitstayid = k.patientunitstayid AND p42.chartoffset = k.INR_last_chartoffset    
  )[OFFSET(0)] END AS INR_last,
    (
    SELECT ARRAY_AGG(INR)
    FROM `physionet-data.eicu_crd_derived.pivoted_lab` p43
    WHERE p43.patientunitstayid = k.patientunitstayid AND p43.chartoffset = k.INR_last_chartoffset
  ) [OFFSET(0)] - (
    SELECT ARRAY_AGG(INR)
    FROM `physionet-data.eicu_crd_derived.pivoted_lab` p44
    WHERE p44.patientunitstayid = k.patientunitstayid AND p44.chartoffset = k.INR_first_chartoffset
  ) [OFFSET(0)] AS INR_clear,
  l.lactate_first_chartoffset,
  (
    SELECT ARRAY_AGG(lactate)
    FROM `physionet-data.eicu_crd_derived.pivoted_lab` p45
    WHERE p45.patientunitstayid = l.patientunitstayid AND p45.chartoffset = l.lactate_first_chartoffset
  )[OFFSET(0)] AS lactate_first,
  l.lactate_last_chartoffset,
  CASE
   WHEN  l.lactate_first_chartoffset =   l.lactate_last_chartoffset THEN NULL
  ELSE(
    SELECT ARRAY_AGG(lactate)
    FROM `physionet-data.eicu_crd_derived.pivoted_lab` p46
    WHERE p46.patientunitstayid = l.patientunitstayid AND p46.chartoffset = l.lactate_last_chartoffset
  )[OFFSET(0)] END AS lactate_last,
    (
    SELECT ARRAY_AGG(lactate)
    FROM `physionet-data.eicu_crd_derived.pivoted_lab` p47
    WHERE p47.patientunitstayid = l.patientunitstayid AND p47.chartoffset = l.lactate_last_chartoffset
  ) [OFFSET(0)] - (
    SELECT ARRAY_AGG(lactate)
    FROM `physionet-data.eicu_crd_derived.pivoted_lab` p48
    WHERE p48.patientunitstayid = l.patientunitstayid AND p48.chartoffset = l.lactate_first_chartoffset
  ) [OFFSET(0)] AS lactate_clear,
  m.platelets_first_chartoffset,
  (
    SELECT ARRAY_AGG(platelets)
    FROM `physionet-data.eicu_crd_derived.pivoted_lab` p49
    WHERE p49.patientunitstayid = m.patientunitstayid  AND p49.chartoffset = m.platelets_first_chartoffset
  )[OFFSET(0)] AS platelets_first,
  m.platelets_last_chartoffset,
  CASE
   WHEN  m.platelets_first_chartoffset =   m.platelets_last_chartoffset THEN NULL
  ELSE(
    SELECT ARRAY_AGG(platelets)
    FROM `physionet-data.eicu_crd_derived.pivoted_lab` p50
    WHERE p50.patientunitstayid = m.patientunitstayid  AND p50.chartoffset = m.platelets_last_chartoffset
  )[OFFSET(0)] END AS platelets_last,
    (
    SELECT ARRAY_AGG(platelets)
    FROM `physionet-data.eicu_crd_derived.pivoted_lab` p51
    WHERE p51.patientunitstayid = m.patientunitstayid AND p51.chartoffset = m.platelets_last_chartoffset
  ) [OFFSET(0)] - (
    SELECT ARRAY_AGG(platelets)
    FROM `physionet-data.eicu_crd_derived.pivoted_lab` p52
    WHERE p52.patientunitstayid = m.patientunitstayid AND p52.chartoffset = m.platelets_first_chartoffset
  ) [OFFSET(0)] AS platelets_clear,
  n.potassium_first_chartoffset,
  (
    SELECT ARRAY_AGG(potassium)
    FROM `physionet-data.eicu_crd_derived.pivoted_lab` p53
    WHERE p53.patientunitstayid = m.patientunitstayid  AND p53.chartoffset = n.potassium_first_chartoffset
  )[OFFSET(0)] AS potassium_first,
  n.potassium_last_chartoffset,
  CASE
   WHEN  n.potassium_first_chartoffset =  n.potassium_last_chartoffset THEN NULL
  ELSE(
    SELECT ARRAY_AGG(potassium)
    FROM `physionet-data.eicu_crd_derived.pivoted_lab` p54
    WHERE p54.patientunitstayid = m.patientunitstayid  AND p54.chartoffset = n.potassium_last_chartoffset
  )[OFFSET(0)] END AS potassium_last,
    (
    SELECT ARRAY_AGG(potassium)
    FROM `physionet-data.eicu_crd_derived.pivoted_lab` p55
    WHERE p55.patientunitstayid = n.patientunitstayid AND p55.chartoffset = n.potassium_last_chartoffset
  ) [OFFSET(0)] - (
    SELECT ARRAY_AGG(potassium)
    FROM `physionet-data.eicu_crd_derived.pivoted_lab` p56
    WHERE p56.patientunitstayid = n.patientunitstayid AND p56.chartoffset = n.potassium_first_chartoffset
  ) [OFFSET(0)] AS potassium_clear,
  o.ptt_first_chartoffset,
  (
    SELECT ARRAY_AGG(ptt)
    FROM `physionet-data.eicu_crd_derived.pivoted_lab` p57
    WHERE p57.patientunitstayid = m.patientunitstayid  AND p57.chartoffset = o.ptt_first_chartoffset
  )[OFFSET(0)] AS ptt_first, 
  o.ptt_last_chartoffset,
  CASE
   WHEN  o.ptt_first_chartoffset =  o.ptt_last_chartoffset THEN NULL
  ELSE(
    SELECT ARRAY_AGG(ptt)
    FROM `physionet-data.eicu_crd_derived.pivoted_lab` p58
    WHERE p58.patientunitstayid = o.patientunitstayid AND p58.chartoffset = o.ptt_last_chartoffset
  )[OFFSET(0)] END AS ptt_last,
    (
    SELECT ARRAY_AGG(ptt)
    FROM `physionet-data.eicu_crd_derived.pivoted_lab` p59
    WHERE p59.patientunitstayid = o.patientunitstayid AND p59.chartoffset = o.ptt_last_chartoffset
  ) [OFFSET(0)] - (
    SELECT ARRAY_AGG(ptt)
    FROM `physionet-data.eicu_crd_derived.pivoted_lab` p60
    WHERE p60.patientunitstayid = o.patientunitstayid AND p60.chartoffset = o.ptt_first_chartoffset
  ) [OFFSET(0)] AS ptt_clear,
  p.sodium_first_chartoffset,
  (
    SELECT ARRAY_AGG(sodium)
    FROM `physionet-data.eicu_crd_derived.pivoted_lab` p61
    WHERE p61.patientunitstayid = p.patientunitstayid AND p61.chartoffset = p.sodium_first_chartoffset
  )[OFFSET(0)] AS sodium_first,
  p.sodium_last_chartoffset,
  CASE
   WHEN  p.sodium_first_chartoffset =  p.sodium_last_chartoffset THEN NULL
  ELSE(
    SELECT ARRAY_AGG(sodium)
    FROM `physionet-data.eicu_crd_derived.pivoted_lab` p62
    WHERE p62.patientunitstayid = p.patientunitstayid AND p62.chartoffset = p.sodium_last_chartoffset
  )[OFFSET(0)] END AS sodium_last,
    (
    SELECT ARRAY_AGG(sodium)
    FROM `physionet-data.eicu_crd_derived.pivoted_lab` p63
    WHERE p63.patientunitstayid = p.patientunitstayid AND p63.chartoffset = p.sodium_last_chartoffset
  ) [OFFSET(0)] - (
    SELECT ARRAY_AGG(sodium)
    FROM `physionet-data.eicu_crd_derived.pivoted_lab` p64
    WHERE p64.patientunitstayid = p.patientunitstayid AND p64.chartoffset = p.sodium_first_chartoffset
  ) [OFFSET(0)] AS sodium_clear,
  q.wbc_first_chartoffset,
  (
    SELECT ARRAY_AGG(wbc)
    FROM `physionet-data.eicu_crd_derived.pivoted_lab` p65
    WHERE p65.patientunitstayid = q.patientunitstayid AND p65.chartoffset = q.wbc_first_chartoffset
  )[OFFSET(0)] AS wbc_first,
  q.wbc_last_chartoffset,
  CASE
   WHEN  q.wbc_first_chartoffset = q.wbc_last_chartoffset THEN NULL
  ELSE(
    SELECT ARRAY_AGG(wbc)
    FROM `physionet-data.eicu_crd_derived.pivoted_lab` p66
    WHERE p66.patientunitstayid = q.patientunitstayid AND p66.chartoffset = q.wbc_last_chartoffset
  )[OFFSET(0)] END AS wbc_last,
    (
    SELECT ARRAY_AGG(wbc)
    FROM `physionet-data.eicu_crd_derived.pivoted_lab` p67
    WHERE p67.patientunitstayid = q.patientunitstayid AND p67.chartoffset = q.wbc_last_chartoffset
  ) [OFFSET(0)] - (
    SELECT ARRAY_AGG(wbc)
    FROM `physionet-data.eicu_crd_derived.pivoted_lab` p68
    WHERE p68.patientunitstayid = q.patientunitstayid AND p68.chartoffset = q.wbc_first_chartoffset
  ) [OFFSET(0)] AS wbc_clear,
  r.bands_first_chartoffset,
  (
    SELECT ARRAY_AGG(bands)
    FROM `physionet-data.eicu_crd_derived.pivoted_lab` p69
    WHERE p69.patientunitstayid = r.patientunitstayid AND p69.chartoffset = r.bands_first_chartoffset
  )[OFFSET(0)] AS bands_first,
  r.bands_last_chartoffset,
  CASE
   WHEN  r.bands_first_chartoffset = r.bands_last_chartoffset THEN NULL
  ELSE(
    SELECT ARRAY_AGG(bands)
    FROM `physionet-data.eicu_crd_derived.pivoted_lab` p70
    WHERE p70.patientunitstayid = r.patientunitstayid AND p70.chartoffset = r.bands_last_chartoffset
  )[OFFSET(0)] END AS bands_last,
    (
    SELECT ARRAY_AGG(bands)
    FROM `physionet-data.eicu_crd_derived.pivoted_lab` p71
    WHERE p71.patientunitstayid = r.patientunitstayid AND p71.chartoffset = r.bands_last_chartoffset
  ) [OFFSET(0)] - (
    SELECT ARRAY_AGG(bands)
    FROM `physionet-data.eicu_crd_derived.pivoted_lab` p72
    WHERE p72.patientunitstayid = r.patientunitstayid AND p72.chartoffset = r.bands_first_chartoffset
  ) [OFFSET(0)] AS bands_clear,
  s.alt_first_chartoffset,
  (
    SELECT ARRAY_AGG(alt)
    FROM `physionet-data.eicu_crd_derived.pivoted_lab` p73
    WHERE p73.patientunitstayid = s.patientunitstayid AND p73.chartoffset = s.alt_first_chartoffset
  )[OFFSET(0)] AS alt_first,
  s.alt_last_chartoffset,
   CASE
   WHEN s.alt_first_chartoffset = s.alt_last_chartoffset THEN NULL
  ELSE(
    SELECT ARRAY_AGG(alt)
    FROM `physionet-data.eicu_crd_derived.pivoted_lab` p74
    WHERE p74.patientunitstayid = s.patientunitstayid AND p74.chartoffset = s.alt_last_chartoffset
  )[OFFSET(0)] END AS alt_last,
    (
    SELECT ARRAY_AGG(alt)
    FROM `physionet-data.eicu_crd_derived.pivoted_lab` p75
    WHERE p75.patientunitstayid = s.patientunitstayid AND p75.chartoffset = s.alt_last_chartoffset
  ) [OFFSET(0)] - (
    SELECT ARRAY_AGG(alt)
    FROM `physionet-data.eicu_crd_derived.pivoted_lab` p76
    WHERE p76.patientunitstayid = s.patientunitstayid AND p76.chartoffset = s.alt_first_chartoffset
  ) [OFFSET(0)] AS alt_clear,
  t.ast_first_chartoffset,
  (
    SELECT ARRAY_AGG(ast)
    FROM `physionet-data.eicu_crd_derived.pivoted_lab` p77
    WHERE p77.patientunitstayid = t.patientunitstayid AND p77.chartoffset = t.ast_first_chartoffset
  )[OFFSET(0)] AS ast_first,
  t.ast_last_chartoffset,
  CASE
   WHEN t.ast_first_chartoffset = t.ast_last_chartoffset THEN NULL
  ELSE(
    SELECT ARRAY_AGG(ast)
    FROM `physionet-data.eicu_crd_derived.pivoted_lab` p78
    WHERE p78.patientunitstayid = t.patientunitstayid AND p78.chartoffset = t.ast_last_chartoffset
  )[OFFSET(0)] END AS ast_last,
    (
    SELECT ARRAY_AGG(ast)
    FROM `physionet-data.eicu_crd_derived.pivoted_lab` p79
    WHERE p79.patientunitstayid = t.patientunitstayid AND p79.chartoffset = t.ast_last_chartoffset
  ) [OFFSET(0)] - (
    SELECT ARRAY_AGG(ast)
    FROM `physionet-data.eicu_crd_derived.pivoted_lab` p80
    WHERE p80.patientunitstayid = t.patientunitstayid AND p80.chartoffset = t.ast_first_chartoffset
  ) [OFFSET(0)] AS ast_clear,
  u.alp_first_chartoffset,
  (
    SELECT ARRAY_AGG(alp)
    FROM `physionet-data.eicu_crd_derived.pivoted_lab` p81
    WHERE p81.patientunitstayid = u.patientunitstayid AND p81.chartoffset = u.alp_first_chartoffset
  )[OFFSET(0)] AS alp_first,
  u.alp_last_chartoffset,
  CASE
   WHEN u.alp_first_chartoffset = u.alp_last_chartoffset THEN NULL
  ELSE(
    SELECT ARRAY_AGG(alp)
    FROM `physionet-data.eicu_crd_derived.pivoted_lab` p82
    WHERE p82.patientunitstayid = u.patientunitstayid AND p82.chartoffset = u.alp_last_chartoffset
  )[OFFSET(0)] END AS alp_last,
    (
    SELECT ARRAY_AGG(alp)
    FROM `physionet-data.eicu_crd_derived.pivoted_lab` p83
    WHERE p83.patientunitstayid = u.patientunitstayid AND p83.chartoffset = u.alp_last_chartoffset
  ) [OFFSET(0)] - (
    SELECT ARRAY_AGG(alp)
    FROM `physionet-data.eicu_crd_derived.pivoted_lab` p84
    WHERE p84.patientunitstayid = u.patientunitstayid AND p84.chartoffset = u.alp_first_chartoffset
  ) [OFFSET(0)] AS alp_clear
FROM patientdistinct
LEFT JOIN ChartoffsetStatsAlbumin a ON patientdistinct.patientunitstayid = a.patientunitstayid
LEFT JOIN ChartoffsetStatsBilirubin b ON patientdistinct.patientunitstayid = b.patientunitstayid
LEFT JOIN ChartoffsetStatsBUN c ON patientdistinct.patientunitstayid = c.patientunitstayid
LEFT JOIN ChartoffsetStatsCalcium d ON patientdistinct.patientunitstayid = d.patientunitstayid
LEFT JOIN ChartoffsetStatsCreatinine e ON patientdistinct.patientunitstayid = e.patientunitstayid
LEFT JOIN ChartoffsetStatsGlucose f ON patientdistinct.patientunitstayid = f.patientunitstayid
LEFT JOIN ChartoffsetStatsBicarbonate g ON patientdistinct.patientunitstayid = g.patientunitstayid
LEFT JOIN ChartoffsetStatsTotalCO2 h ON patientdistinct.patientunitstayid = h.patientunitstayid
LEFT JOIN ChartoffsetStatsHematocrit i ON patientdistinct.patientunitstayid = i.patientunitstayid
LEFT JOIN ChartoffsetStatsHemoglobin j ON patientdistinct.patientunitstayid = j.patientunitstayid
LEFT JOIN ChartoffsetStatsINR k ON patientdistinct.patientunitstayid = k.patientunitstayid
LEFT JOIN ChartoffsetStatsLactate l ON patientdistinct.patientunitstayid = l.patientunitstayid
LEFT JOIN ChartoffsetStatsPlatelets m ON patientdistinct.patientunitstayid = m.patientunitstayid
LEFT JOIN ChartoffsetStatsPotassium n ON patientdistinct.patientunitstayid = n.patientunitstayid
LEFT JOIN ChartoffsetStatsPtt o ON patientdistinct.patientunitstayid = o.patientunitstayid
LEFT JOIN ChartoffsetStatsSodium p ON patientdistinct.patientunitstayid = p.patientunitstayid
LEFT JOIN ChartoffsetStatsWbc q ON patientdistinct.patientunitstayid = q.patientunitstayid
LEFT JOIN ChartoffsetStatsBands r ON patientdistinct.patientunitstayid = r.patientunitstayid
LEFT JOIN ChartoffsetStatsAlt s ON patientdistinct.patientunitstayid = s.patientunitstayid
LEFT JOIN ChartoffsetStatsAst t ON patientdistinct.patientunitstayid = t.patientunitstayid
LEFT JOIN ChartoffsetStatsAlp u ON patientdistinct.patientunitstayid = u.patientunitstayid
ORDER BY patientdistinct.patientunitstayid ASC
