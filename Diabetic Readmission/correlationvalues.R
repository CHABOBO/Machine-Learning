tbl1 <- table(data$readmitted, data$race) # related. p = 2.989455e-06
tbl1 <- table(data$readmitted, data$gender) # not related. p = 0.2587079
tbl1 <- table(data$readmitted, data$age) # related. p = 1.320659e-11
tbl1 <- table(data$readmitted, data$admission_type_id) # related. p = 0.004282654
tbl1 <- table(data$readmitted, data$discharge_disposition_id) # related. p = 7.526001e-34 
tbl1 <- table(data$readmitted, data$admission_source_id) # related. p = 3.400355e-21
tbl1 <- table(data$readmitted, data$time_in_hospital) # related. p = 1.362781e-08
tbl1 <- table(data$readmitted, data$medical_specialty) # related. p = 2.212868e-13
value <- rcorr(as.numeric(data$readmitted), data$num_lab_procedures, type = "pearson") # related. p = 3.996803e-15
tbl1 <- table(data$readmitted, data$medical_specialty) # related. p = 0.01228472
value <- rcorr(as.numeric(data$readmitted), data$num_medications, type = "pearson") # related. p = 0.0001344885
tbl1 <- table(data$readmitted, data$number_outpatient) # related. p = 9.381091e-21
tbl1 <- table(data$readmitted, data$number_emergency) # related. p = 9.555591e-16
tbl1 <- table(data$readmitted, data$number_inpatient) # related. p = 4.531272e-81
tbl1 <- table(data$readmitted, data$diag_1) # related. p = 7.870051e-10
tbl1 <- table(data$readmitted, data$diag_2) # related. p = 0.000182084
tbl1 <- table(data$readmitted, data$diag_3) # related. p = 1.502689e-06 
tbl1 <- table(data$readmitted, data$number_diagnoses) # related. p = 5.323331e-49
tbl1 <- table(data$readmitted, data$max_glu_serum) # not related. p = 0.2211165
tbl1 <- table(data$readmitted, data$A1Cresult) # not related. p = 0.194942
tbl1 <- table(data$readmitted, data$metformin) # not related. p = 0.06932096
tbl1 <- table(data$readmitted, data$repaglinide) # related. p = 0.002673941
tbl1 <- table(data$readmitted, data$nateglinide) # not related. p = 0.2000512
tbl1 <- table(data$readmitted, data$chlorpropamide) # not related. p = 0.469718
tbl1 <- table(data$readmitted, data$glimepiride) # not related. p = 0.264377
tbl1 <- table(data$readmitted, data$acetohexamide) # not related. Only one level (No)
tbl1 <- table(data$readmitted, data$glipizide) # not related. p = 0.2068714
tbl1 <- table(data$readmitted, data$glyburide) # not related. p = 0.4101949
tbl1 <- table(data$readmitted, data$tolbutamide) # not related. p = 0.8126524
tbl1 <- table(data$readmitted, data$pioglitazone) # related. p = 3.999649e-07
tbl1 <- table(data$readmitted, data$rosiglitazone) # not related. p = 0.5575477
tbl1 <- table(data$readmitted, data$acarbose) # not related. p = 0.7079662
tbl1 <- table(data$readmitted, data$miglitol) # not related. p = 0.2743488
tbl1 <- table(data$readmitted, data$troglitazone) # not related. Only one level (No)
tbl1 <- table(data$readmitted, data$tolazamide) # not related. p = 0.1569169
tbl1 <- table(data$readmitted, data$examide) # not related. Only one level (No)
tbl1 <- table(data$readmitted, data$citoglipton) # not related. Only one level (No)
tbl1 <- table(data$readmitted, data$insulin) # related. p = 3.120518e-06
tbl1 <- table(data$readmitted, data$glyburide.metformin) # not related. p = 0.4666693
tbl1 <- table(data$readmitted, data$glipizide.metformin) # not related. p = 0.7736557
tbl1 <- table(data$readmitted, data$glimepiride.pioglitazone) # not related. Only one level (No)
tbl1 <- table(data$readmitted, data$metformin.rosiglitazone) # not related. Only one level (No)
tbl1 <- table(data$readmitted, data$metformin.pioglitazone) # not related. Only one level (No)
tbl1 <- table(data$readmitted, data$change) # related. p = 3.157653e-06
tbl1 <- table(data$readmitted, data$diabetesMed) # related. p = 2.521074e-07


CrossTable(tbl1, expected = T, chisq = T, sresid = T, format = "SPSS")