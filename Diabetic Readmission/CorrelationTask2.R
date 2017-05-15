tbl <- table(data$time_in_hospital, data$race) # not related. p = 0.3524839
tbl <- table(data$time_in_hospital, data$gender) # related. p = 0.04060866
tbl <- table(data$time_in_hospital, data$age) # related. p = 1.254906e-14
tbl <- table(data$time_in_hospital, data$admission_type_id) # related. p = 1.09843e-17
tbl <- table(data$time_in_hospital, data$discharge_disposition_id) # related. p = 4.70098e-07
tbl <- table(data$time_in_hospital, data$admission_source_id) # related. p = 1.731813e-19
tbl <- table(data$time_in_hospital, data$medical_specialty) # related. p = 6.883952e-74
tbl <- table(data$time_in_hospital, data$num_lab_procedures) # related. p = 7.255753e-153
tbl <- table(data$time_in_hospital, data$num_procedures) # related. p = 1.386192e-88
tbl <- table(data$time_in_hospital, data$num_medications) # related. p = 0
tbl <- table(data$time_in_hospital, data$number_outpatient) # not related. p = 0.999998
tbl <- table(data$time_in_hospital, data$number_emergency) # not related. p = 0.9997326
tbl <- table(data$time_in_hospital, data$number_inpatient) # related. p = 1.218837e-09
tbl <- table(data$time_in_hospital, data$diag_1) # related. p = 1.237587e-137
tbl <- table(data$time_in_hospital, data$diag_2) # related. p = 2.234591e-29
tbl <- table(data$time_in_hospital, data$diag_3) # realted. p = 4.48846e-25
tbl <- table(data$time_in_hospital, data$number_diagnoses) # related. p = 2.092154e-116
tbl <- table(data$time_in_hospital, data$max_glu_serum) # related. p = 0.002911867
tbl <- table(data$time_in_hospital, data$A1Cresult) # related. p = 1.337538e-07
tbl <- table(data$time_in_hospital, data$metformin) # related. p = 0.00438493
tbl <- table(data$time_in_hospital, data$repaglinide) # not related. p = 0.2012759
tbl <- table(data$time_in_hospital, data$nateglinide) # related. p = 2.631503e-09
tbl <- table(data$time_in_hospital, data$chlorpropamide) # not related. p = 0.7421269
tbl <- table(data$time_in_hospital, data$glimepiride) # not related. p = 0.2027341
tbl <- table(data$time_in_hospital, data$acetohexamide) # not related. Only one variable.
tbl <- table(data$time_in_hospital, data$glipizide) # related. p = 3.466365e-06
tbl <- table(data$time_in_hospital, data$glyburide) # related. p = 0.0003744403
tbl <- table(data$time_in_hospital, data$tolbutamide) # not related. p = 0.1769626
tbl <- table(data$time_in_hospital, data$pioglitazone) # related. p = 0.02355543
tbl <- table(data$time_in_hospital, data$rosiglitazone) # related. p = 0.0005838871
tbl <- table(data$time_in_hospital, data$acarbose) # not related. p = 0.7771105
tbl <- table(data$time_in_hospital, data$miglitol) # not related. p = 0.9825007
tbl <- table(data$time_in_hospital, data$troglitazone) # not related. Only one variable.
tbl <- table(data$time_in_hospital, data$tolazamide) # not related. p = 0.953488
tbl <- table(data$time_in_hospital, data$examide) # not related. Only one variable.
tbl <- table(data$time_in_hospital, data$citoglipton) # not related. Only one variable.
tbl <- table(data$time_in_hospital, data$insulin) # related. p = 5.29941e-26
tbl <- table(data$time_in_hospital, data$glyburide.metformin) # not related. p = 0.7199752
tbl <- table(data$time_in_hospital, data$glipizide.metformin) # not related. p = 0.4765996
tbl <- table(data$time_in_hospital, data$glimepiride.pioglitazone) # not related. Only one variable.
tbl <- table(data$time_in_hospital, data$metformin.rosiglitazone) # not related. Only one variable.
tbl <- table(data$time_in_hospital, data$metformin.pioglitazone) # not related. Only one variable.
tbl <- table(data$time_in_hospital, data$change) # related. p = 5.996495e-23
tbl <- table(data$time_in_hospital, data$diabetesMed) # related. p = 1.56838e-11
tbl <- table(data$time_in_hospital, data$readmitted) # related. p = 1.362781e-08


CrossTable(tbl, expected = T, chisq = T, sresid = T, format = "SPSS")
value <- rcorr(data$time_in_hospital, data$number_acetohexamide, type = "pearson")