tbl1 <- table(data$diag_2, data$race) # related. p = 1.644179e-1
tbl1 <- table(data$diag_2, data$gender) #related p =  3.395482e-11 
tbl1 <- table(data$diag_2, data$age)    #related  p =  3.200035e-261 
tbl1 <- table(data$diag_2, data$admission_type_id) #related p =  1.889503e-34
tbl1 <- table(data$diag_2, data$discharge_disposition_id) #related p=5.004381e-08 
tbl1 <- table(data$diag_2, data$admission_source_id) #related p =  9.709363e-23
tbl1 <- table(data$diag_2, data$time_in_hospital)  # related   p =  2.234591e-29 
tbl1 <- table(data$diag_2, data$medical_specialty) # related p=0;
value <- rcorr(as.numeric(data$diag_2), data$num_lab_procedures, type = "pearson") #not related value$P=0.90
value <- rcorr(as.numeric(data$diag_2), data$num_procedures, type = "pearson") # related value$P=8.824571e-07
value <- rcorr(as.numeric(data$diag_2), data$num_medications, type = "pearson") # related 1.779688e-12
value <- rcorr(as.numeric(data$diag_2), data$number_outpatient, type = "pearson") #not related value$P=0.259
value <- rcorr(as.numeric(data$diag_2), data$number_emergency, type = "pearson") #not related p=0.19
value <- rcorr(as.numeric(data$diag_2), data$number_inpatient, type = "pearson") #not related p=0.37
tbl1 <- table(data$diag_2, data$diag_1) # related p=0;
tbl1 <- table(data$diag_2, data$number_diagnoses) # related p =  4.352575e-103 
tbl1 <- table(data$diag_2, data$max_glu_serum) # related p =  0.0001897857
tbl1 <- table(data$diag_2, data$A1Cresult) # related p =  3.552891e-19
tbl1 <- table(data$diag_2, data$metformin) #  related p =  2.128159e-05 
tbl1 <- table(data$diag_2, data$repaglinide) #notrelated p= 0.7546902
tbl1 <- table(data$diag_2, data$nateglinide) #notrelated p= 0.99988
tbl1 <- table(data$diag_2, data$chlorpropamide) #notrelated p= 0.99988
tbl1 <- table(data$diag_2, data$glimepiride) # not related. p = 0.209377
tbl1 <- table(data$diag_2, data$acetohexamide) # not related. Only one level (No)
tbl1 <- table(data$diag_2, data$glipizide) # not related. p = 0.2098714
tbl1 <- table(data$diag_2, data$glyburide) # not related. p =  1.006914e-0
tbl1 <- table(data$diag_2, data$tolbutamide) #  related. p=1.006914e-06 
tbl1 <- table(data$diag_2, data$pioglitazone) # not related. p = 30.4230652
tbl1 <- table(data$diag_2, data$rosiglitazone) #  related. p = 5.715846e-14
tbl1 <- table(data$diag_2, data$acarbose) # not related. p = 0.999
tbl1 <- table(data$diag_2, data$miglitol) # not related. p = 0.999
tbl1 <- table(data$diag_2, data$troglitazone) # not related. Only one level (No)
tbl1 <- table(data$diag_2, data$tolazamide) # not related. p = 0.42
tbl1 <- table(data$diag_2, data$examide) # not related. Only one level (No)
tbl1 <- table(data$diag_2, data$citoglipton) # not related. Only one level (No)
tbl1 <- table(data$diag_2, data$insulin) # related. p = 5.853485e-09
tbl1 <- table(data$diag_2, data$glyburide.metformin) # not related. p = 0.5581
tbl1 <- table(data$diag_2, data$glipizide.metformin) #  related. p = 4.901798e-06 
tbl1 <- table(data$diag_2, data$glimepiride.pioglitazone) # not related. Only one level (No)
tbl1 <- table(data$diag_2, data$metformin.rosiglitazone) # not related. Only one level (No)
tbl1 <- table(data$diag_2, data$metformin.pioglitazone) # not related. Only one level (No)

tbl1 <- table(data$diag_2, data$change) # related. p = 4.407204e-07 
tbl1 <- table(data$diag_2, data$diabetesMed) # related. p = 1.720928e-06














