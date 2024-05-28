WITH ChartoffsetStatsHeartrate AS (
  SELECT
    patientunitstayid,
    MIN(chartoffset) AS heartrate_first_chartoffset,
    MAX(chartoffset) AS heartrate_last_chartoffset
  FROM `physionet-data.eicu_crd_derived.pivoted_vital`
  WHERE chartoffset >= -1440 AND chartoffset < 0 AND heartrate IS NOT NULL
  GROUP BY patientunitstayid
),
ChartoffsetStatsRespiratoryrate AS (
  SELECT
    patientunitstayid,
    MIN(chartoffset) AS respiratoryrate_first_chartoffset,
    MAX(chartoffset) AS respiratoryrate_last_chartoffset
  FROM `physionet-data.eicu_crd_derived.pivoted_vital`
  WHERE chartoffset >= -1440 AND chartoffset < 0 AND respiratoryrate IS NOT NULL
  GROUP BY patientunitstayid
),
ChartoffsetStatsSpO2 AS (
  SELECT
    patientunitstayid,
    MIN(chartoffset) AS spo2_first_chartoffset,
    MAX(chartoffset) AS spo2_last_chartoffset
  FROM `physionet-data.eicu_crd_derived.pivoted_vital`
  WHERE chartoffset >= -1440 AND chartoffset < 0 AND spo2 IS NOT NULL
  GROUP BY patientunitstayid
),
ChartoffsetStatsNIBPSystolic AS (
  SELECT
    patientunitstayid,
    MIN(chartoffset) AS nibp_systolic_first_chartoffset,
    MAX(chartoffset) AS nibp_systolic_last_chartoffset
  FROM `physionet-data.eicu_crd_derived.pivoted_vital`
  WHERE chartoffset >= -1440 AND chartoffset < 0 AND nibp_systolic IS NOT NULL
  GROUP BY patientunitstayid
),
ChartoffsetStatsNIBPDiastolic AS (
  SELECT
    patientunitstayid,
    MIN(chartoffset) AS nibp_diastolic_first_chartoffset,
    MAX(chartoffset) AS nibp_diastolic_last_chartoffset
  FROM `physionet-data.eicu_crd_derived.pivoted_vital`
  WHERE chartoffset >= -1440 AND chartoffset < 0 AND nibp_diastolic IS NOT NULL
  GROUP BY patientunitstayid
),
ChartoffsetStatsNIBPMean AS (
  SELECT
    patientunitstayid,
    MIN(chartoffset) AS nibp_mean_first_chartoffset,
    MAX(chartoffset) AS nibp_mean_last_chartoffset
  FROM `physionet-data.eicu_crd_derived.pivoted_vital`
  WHERE chartoffset >= -1440 AND chartoffset < 0 AND nibp_mean IS NOT NULL
  GROUP BY patientunitstayid
),
ChartoffsetStatsTemperature AS (
  SELECT
    patientunitstayid,
    MIN(chartoffset) AS temperature_first_chartoffset,
    MAX(chartoffset) AS temperature_last_chartoffset
  FROM `physionet-data.eicu_crd_derived.pivoted_vital`
  WHERE chartoffset >= -1440 AND chartoffset < 0 AND temperature IS NOT NULL
  GROUP BY patientunitstayid
),
ChartoffsetStatsIBPSystolic AS (
  SELECT
    patientunitstayid,
    MIN(chartoffset) AS ibp_systolic_first_chartoffset,
    MAX(chartoffset) AS ibp_systolic_last_chartoffset
  FROM `physionet-data.eicu_crd_derived.pivoted_vital`
  WHERE chartoffset >= -1440 AND chartoffset < 0 AND ibp_systolic IS NOT NULL
  GROUP BY patientunitstayid
),
ChartoffsetStatsIBPDiastolic AS (
  SELECT
    patientunitstayid,
    MIN(chartoffset) AS ibp_diastolic_first_chartoffset,
    MAX(chartoffset) AS ibp_diastolic_last_chartoffset
  FROM `physionet-data.eicu_crd_derived.pivoted_vital`
  WHERE chartoffset >= -1440 AND chartoffset < 0 AND ibp_diastolic IS NOT NULL
  GROUP BY patientunitstayid
),
ChartoffsetStatsIBPMean AS (
  SELECT
    patientunitstayid,
    MIN(chartoffset) AS ibp_mean_first_chartoffset,
    MAX(chartoffset) AS ibp_mean_last_chartoffset
  FROM `physionet-data.eicu_crd_derived.pivoted_vital`
  WHERE chartoffset >= -1440 AND chartoffset < 0 AND ibp_mean IS NOT NULL
  GROUP BY patientunitstayid
),
patientdistinct AS (
 SELECT DISTINCT patientunitstayid
 FROM `physionet-data.eicu_crd.patient`
)
SELECT
  patientdistinct.patientunitstayid, 
  a.heartrate_first_chartoffset,
  (
    SELECT ARRAY_AGG(heartrate)
    FROM `physionet-data.eicu_crd_derived.pivoted_vital` p1
    WHERE p1.patientunitstayid = a.patientunitstayid AND p1.chartoffset = a.heartrate_first_chartoffset
  ) [OFFSET(0)] AS heartrate_first,
  a.heartrate_last_chartoffset,
   CASE
   WHEN a.heartrate_first_chartoffset = a.heartrate_last_chartoffset THEN NULL
  ELSE(
    SELECT ARRAY_AGG(heartrate)
    FROM `physionet-data.eicu_crd_derived.pivoted_vital` p2
    WHERE p2.patientunitstayid = a.patientunitstayid AND p2.chartoffset = a.heartrate_last_chartoffset
  ) [OFFSET(0)] END AS heartrate_last,
    (
    SELECT ARRAY_AGG(heartrate)
    FROM `physionet-data.eicu_crd_derived.pivoted_vital` p3
    WHERE p3.patientunitstayid = a.patientunitstayid AND p3.chartoffset = a.heartrate_last_chartoffset
  ) [OFFSET(0)] - (
    SELECT ARRAY_AGG(heartrate)
    FROM `physionet-data.eicu_crd_derived.pivoted_vital` p4
    WHERE p4.patientunitstayid = a.patientunitstayid AND p4.chartoffset = a.heartrate_first_chartoffset
  ) [OFFSET(0)] AS heartrate_clear,
  b.respiratoryrate_first_chartoffset,
  (
    SELECT ARRAY_AGG(respiratoryrate)
    FROM `physionet-data.eicu_crd_derived.pivoted_vital` p5
    WHERE p5.patientunitstayid = b.patientunitstayid AND p5.chartoffset = b.respiratoryrate_first_chartoffset
  ) [OFFSET(0)] AS respiratoryrate_first,
  b.respiratoryrate_last_chartoffset,
   CASE
   WHEN b.respiratoryrate_first_chartoffset =  b.respiratoryrate_last_chartoffset THEN NULL
  ELSE(
    SELECT ARRAY_AGG(respiratoryrate)
    FROM `physionet-data.eicu_crd_derived.pivoted_vital` p6
    WHERE p6.patientunitstayid = b.patientunitstayid AND p6.chartoffset = b.respiratoryrate_last_chartoffset
  ) [OFFSET(0)] END AS respiratoryrate_last,
    (
    SELECT ARRAY_AGG(respiratoryrate)
    FROM `physionet-data.eicu_crd_derived.pivoted_vital` p7
    WHERE p7.patientunitstayid = b.patientunitstayid AND p7.chartoffset = b.respiratoryrate_last_chartoffset
  ) [OFFSET(0)] - (
    SELECT ARRAY_AGG(respiratoryrate)
    FROM `physionet-data.eicu_crd_derived.pivoted_vital` p8
    WHERE p8.patientunitstayid = b.patientunitstayid AND p8.chartoffset = b.respiratoryrate_first_chartoffset
  ) [OFFSET(0)] AS respiratoryrate_clear,
  c.spo2_first_chartoffset,
  (
    SELECT ARRAY_AGG(spo2)
    FROM `physionet-data.eicu_crd_derived.pivoted_vital` p9
    WHERE p9.patientunitstayid = c.patientunitstayid AND p9.chartoffset = c.spo2_first_chartoffset
  ) [OFFSET(0)] AS spo2_first,
  c.spo2_last_chartoffset,
   CASE
   WHEN c.spo2_first_chartoffset =  c.spo2_last_chartoffset THEN NULL
  ELSE(
    SELECT ARRAY_AGG(spo2)
    FROM `physionet-data.eicu_crd_derived.pivoted_vital` p10
    WHERE p10.patientunitstayid = c.patientunitstayid AND p10.chartoffset = c.spo2_last_chartoffset
  ) [OFFSET(0)] END AS spo2_last,
    (
    SELECT ARRAY_AGG(spo2)
    FROM `physionet-data.eicu_crd_derived.pivoted_vital` p11
    WHERE p11.patientunitstayid = c.patientunitstayid AND p11.chartoffset = c.spo2_last_chartoffset
  ) [OFFSET(0)] - (
    SELECT ARRAY_AGG(spo2)
    FROM `physionet-data.eicu_crd_derived.pivoted_vital` p12
    WHERE p12.patientunitstayid = c.patientunitstayid AND p12.chartoffset = c.spo2_first_chartoffset
  ) [OFFSET(0)] AS spo2_clear,
  d.nibp_systolic_first_chartoffset,
  (
    SELECT ARRAY_AGG(nibp_systolic)
    FROM `physionet-data.eicu_crd_derived.pivoted_vital` p13
    WHERE p13.patientunitstayid = d.patientunitstayid AND p13.chartoffset = d.nibp_systolic_first_chartoffset
  ) [OFFSET(0)] AS nibp_systolic_first,
  d.nibp_systolic_last_chartoffset,
  CASE
   WHEN d.nibp_systolic_first_chartoffset =  d.nibp_systolic_last_chartoffset THEN NULL
  ELSE(
    SELECT ARRAY_AGG(nibp_systolic)
    FROM `physionet-data.eicu_crd_derived.pivoted_vital` p14
    WHERE p14.patientunitstayid = d.patientunitstayid AND p14.chartoffset = d.nibp_systolic_last_chartoffset
  ) [OFFSET(0)] END AS nibp_systolic_last,
    (
    SELECT ARRAY_AGG(nibp_systolic)
    FROM `physionet-data.eicu_crd_derived.pivoted_vital` p15
    WHERE p15.patientunitstayid = d.patientunitstayid AND p15.chartoffset = d.nibp_systolic_last_chartoffset
  ) [OFFSET(0)] - (
    SELECT ARRAY_AGG(nibp_systolic)
    FROM `physionet-data.eicu_crd_derived.pivoted_vital` p16
    WHERE p16.patientunitstayid = d.patientunitstayid AND p16.chartoffset = d.nibp_systolic_first_chartoffset
  ) [OFFSET(0)] AS nibp_systolic_clear,
  e.nibp_diastolic_first_chartoffset,
  (
     SELECT ARRAY_AGG(nibp_diastolic)
    FROM `physionet-data.eicu_crd_derived.pivoted_vital` p17
    WHERE p17.patientunitstayid = e.patientunitstayid AND p17.chartoffset = e.nibp_diastolic_first_chartoffset
  ) [OFFSET(0)] AS nibp_diastolic_first,
  e.nibp_diastolic_last_chartoffset,
  CASE
   WHEN  e.nibp_diastolic_first_chartoffset = e.nibp_diastolic_last_chartoffset THEN NULL
  ELSE(
    SELECT ARRAY_AGG(nibp_diastolic)
    FROM `physionet-data.eicu_crd_derived.pivoted_vital` p18
    WHERE p18.patientunitstayid = e.patientunitstayid AND p18.chartoffset = e.nibp_diastolic_last_chartoffset
  ) [OFFSET(0)] END AS nibp_diastolic_last,
    (
    SELECT ARRAY_AGG(nibp_diastolic)
    FROM `physionet-data.eicu_crd_derived.pivoted_vital` p19
    WHERE p19.patientunitstayid = e.patientunitstayid AND p19.chartoffset = e.nibp_diastolic_last_chartoffset
  ) [OFFSET(0)] - (
    SELECT ARRAY_AGG(nibp_diastolic)
    FROM `physionet-data.eicu_crd_derived.pivoted_vital` p20
    WHERE p20.patientunitstayid = e.patientunitstayid AND p20.chartoffset = e.nibp_diastolic_first_chartoffset
  ) [OFFSET(0)] AS nibp_diastolic_clear,
  f.nibp_mean_first_chartoffset,
  (
    SELECT ARRAY_AGG(nibp_mean)
    FROM `physionet-data.eicu_crd_derived.pivoted_vital` p21
    WHERE p21.patientunitstayid = f.patientunitstayid AND p21.chartoffset = f.nibp_mean_first_chartoffset
  ) [OFFSET(0)] AS nibp_mean_first,
  f.nibp_mean_last_chartoffset,
   CASE
   WHEN f.nibp_mean_first_chartoffset = f.nibp_mean_last_chartoffset THEN NULL
  ELSE(
    SELECT ARRAY_AGG(nibp_mean)
    FROM `physionet-data.eicu_crd_derived.pivoted_vital` p22
    WHERE p22.patientunitstayid = f.patientunitstayid AND p22.chartoffset = f.nibp_mean_last_chartoffset
  ) [OFFSET(0)] END AS nibp_mean_last,
    (
    SELECT ARRAY_AGG(nibp_mean)
    FROM `physionet-data.eicu_crd_derived.pivoted_vital` p23
    WHERE p23.patientunitstayid = f.patientunitstayid AND p23.chartoffset = f.nibp_mean_last_chartoffset
  ) [OFFSET(0)] - (
    SELECT ARRAY_AGG(nibp_mean)
    FROM `physionet-data.eicu_crd_derived.pivoted_vital` p24
    WHERE p24.patientunitstayid = f.patientunitstayid AND p24.chartoffset = f.nibp_mean_first_chartoffset
  ) [OFFSET(0)] AS nibp_mean_clear,
  g.temperature_first_chartoffset,
  (
    SELECT ARRAY_AGG(temperature)
    FROM `physionet-data.eicu_crd_derived.pivoted_vital` p25
    WHERE p25.patientunitstayid = g.patientunitstayid AND p25.chartoffset = g.temperature_first_chartoffset
  ) [OFFSET(0)] AS temperature_first,
  g.temperature_last_chartoffset,
   CASE
   WHEN g.temperature_first_chartoffset = g.temperature_last_chartoffset THEN NULL
  ELSE(
    SELECT ARRAY_AGG(temperature)
    FROM `physionet-data.eicu_crd_derived.pivoted_vital` p26
    WHERE p26.patientunitstayid = g.patientunitstayid AND p26.chartoffset = g.temperature_last_chartoffset
  ) [OFFSET(0)] END AS temperature_last,
    (
    SELECT ARRAY_AGG(temperature)
    FROM `physionet-data.eicu_crd_derived.pivoted_vital` p27
    WHERE p27.patientunitstayid = g.patientunitstayid AND p27.chartoffset = g.temperature_last_chartoffset
  ) [OFFSET(0)] - (
    SELECT ARRAY_AGG(temperature)
    FROM `physionet-data.eicu_crd_derived.pivoted_vital` p28
    WHERE p28.patientunitstayid = g.patientunitstayid AND p28.chartoffset = g.temperature_first_chartoffset
  ) [OFFSET(0)] AS temperature_clear,
  h.ibp_systolic_first_chartoffset,
  (
    SELECT ARRAY_AGG(ibp_systolic)
    FROM `physionet-data.eicu_crd_derived.pivoted_vital` p29
    WHERE p29.patientunitstayid = h.patientunitstayid AND p29.chartoffset = h.ibp_systolic_first_chartoffset
  ) [OFFSET(0)] AS ibp_systolic_first,
  h.ibp_systolic_last_chartoffset,
  CASE
   WHEN h.ibp_systolic_first_chartoffset = h.ibp_systolic_last_chartoffset THEN NULL
  ELSE(
    SELECT ARRAY_AGG(ibp_systolic)
    FROM `physionet-data.eicu_crd_derived.pivoted_vital` p30
    WHERE p30.patientunitstayid = h.patientunitstayid AND p30.chartoffset = h.ibp_systolic_last_chartoffset
  ) [OFFSET(0)] END AS ibp_systolic_last,
    (
    SELECT ARRAY_AGG(ibp_systolic)
    FROM `physionet-data.eicu_crd_derived.pivoted_vital` p31
    WHERE p31.patientunitstayid = h.patientunitstayid AND p31.chartoffset = h.ibp_systolic_last_chartoffset
  ) [OFFSET(0)] - (
    SELECT ARRAY_AGG(ibp_systolic)
    FROM `physionet-data.eicu_crd_derived.pivoted_vital` p32
    WHERE p32.patientunitstayid = h.patientunitstayid AND p32.chartoffset = h.ibp_systolic_first_chartoffset
  ) [OFFSET(0)] AS ibp_systolic_clear,
  i.ibp_diastolic_first_chartoffset,
  (
    SELECT ARRAY_AGG(ibp_diastolic)
    FROM `physionet-data.eicu_crd_derived.pivoted_vital` p33
    WHERE p33.patientunitstayid = i.patientunitstayid AND p33.chartoffset = i.ibp_diastolic_first_chartoffset
  ) [OFFSET(0)] AS ibp_diastolic_first,
  i.ibp_diastolic_last_chartoffset,
  CASE
   WHEN i.ibp_diastolic_first_chartoffset = i.ibp_diastolic_last_chartoffset THEN NULL
  ELSE(
    SELECT ARRAY_AGG(ibp_diastolic)
    FROM `physionet-data.eicu_crd_derived.pivoted_vital` p34
    WHERE p34.patientunitstayid = i.patientunitstayid AND p34.chartoffset = i.ibp_diastolic_last_chartoffset
  ) [OFFSET(0)] END AS ibp_diastolic_last,
    (
    SELECT ARRAY_AGG(ibp_diastolic)
    FROM `physionet-data.eicu_crd_derived.pivoted_vital` p35
    WHERE p35.patientunitstayid = h.patientunitstayid AND p35.chartoffset = i.ibp_diastolic_last_chartoffset
  ) [OFFSET(0)] - (
    SELECT ARRAY_AGG(ibp_diastolic)
    FROM `physionet-data.eicu_crd_derived.pivoted_vital` p36
    WHERE p36.patientunitstayid = h.patientunitstayid AND p36.chartoffset = i.ibp_diastolic_first_chartoffset
  ) [OFFSET(0)] AS ibp_diastolic_clear,
  j.ibp_mean_first_chartoffset,
  (
    SELECT ARRAY_AGG(ibp_mean)
    FROM `physionet-data.eicu_crd_derived.pivoted_vital` p37
    WHERE p37.patientunitstayid = i.patientunitstayid AND p37.chartoffset = j.ibp_mean_first_chartoffset
  ) [OFFSET(0)] AS ibp_mean_first,
  j.ibp_mean_last_chartoffset,
  CASE
   WHEN j.ibp_mean_first_chartoffset = j.ibp_mean_last_chartoffset THEN NULL
  ELSE(
    SELECT ARRAY_AGG(ibp_mean)
    FROM `physionet-data.eicu_crd_derived.pivoted_vital` p38
    WHERE p38.patientunitstayid = i.patientunitstayid AND p38.chartoffset = j.ibp_mean_last_chartoffset
  ) [OFFSET(0)] END AS ibp_mean_last,
    (
    SELECT ARRAY_AGG(ibp_mean)
    FROM `physionet-data.eicu_crd_derived.pivoted_vital` p39
    WHERE p39.patientunitstayid = h.patientunitstayid AND p39.chartoffset = j.ibp_mean_last_chartoffset
  ) [OFFSET(0)] - (
    SELECT ARRAY_AGG(ibp_mean)
    FROM `physionet-data.eicu_crd_derived.pivoted_vital` p40
    WHERE p40.patientunitstayid = h.patientunitstayid AND p40.chartoffset = j.ibp_mean_first_chartoffset
  ) [OFFSET(0)] AS ibp_mean_clear
FROM patientdistinct
LEFT JOIN ChartoffsetStatsHeartrate a ON patientdistinct.patientunitstayid = a.patientunitstayid
LEFT JOIN ChartoffsetStatsRespiratoryrate b ON patientdistinct.patientunitstayid = b.patientunitstayid
LEFT JOIN ChartoffsetStatsSpO2 c ON patientdistinct.patientunitstayid = c.patientunitstayid
LEFT JOIN ChartoffsetStatsNIBPSystolic d ON patientdistinct.patientunitstayid = d.patientunitstayid
LEFT JOIN ChartoffsetStatsNIBPDiastolic e ON patientdistinct.patientunitstayid = e.patientunitstayid
LEFT JOIN ChartoffsetStatsNIBPMean f ON patientdistinct.patientunitstayid = f.patientunitstayid
LEFT JOIN ChartoffsetStatsTemperature g ON patientdistinct.patientunitstayid = g.patientunitstayid
LEFT JOIN ChartoffsetStatsIBPSystolic h ON patientdistinct.patientunitstayid = h.patientunitstayid
LEFT JOIN ChartoffsetStatsIBPDiastolic i ON patientdistinct.patientunitstayid = i.patientunitstayid
LEFT JOIN ChartoffsetStatsIBPMean j ON patientdistinct.patientunitstayid = j.patientunitstayid
ORDER BY patientdistinct.patientunitstayid ASC
