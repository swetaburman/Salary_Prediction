Bank = read.csv(file.choose())
head(Bank)
View(Bank)
Bank$EducLev = as.factor(Bank$EducLev)
Bank$JobGrade = as.factor(Bank$JobGrade)


RegBank = lm(Salary ~ EducLev + JobGrade + Gender + YrsPrior, data=Bank)
summary(RegBank)

RegAll = lm(Salary ~ .-Employee, data = Bank)
summary(RegAll)

Bank$Experience = Bank$YrsPrior + (95 - Bank$YrHired)
Bank$Age = 95 - Bank$YrBorn

RegEx = lm(Salary ~ Experience + Age + Gender, data = Bank)
summary(RegEx)

Bank1 = Bank[-c(208),]

RegAll = lm(Salary ~ .-Employee - Experience - Age, data = Bank)
summary(RegAll)

Bank$Experience = Bank$YrsPrior + (95 - Bank$YrHired)
Bank$Age = 95 - Bank$YrBorn

RegEx = lm(Salary ~ Experience + Age + Gender, data = Bank)
summary(RegEx)

View(Bank)
Bank1$NullValue = as.character(Bank1$NullValue)

Bank1$GenderTech = as.character(Bank1$GenderTech)

Bank1$GenderTech <- with(Bank1, ifelse(as.character(Gender)=="Male", "Male" , "Male"))
                         
Bank1$GenderTech <- with(Bank1, ifelse(as.character(Gender)=="Female" & as.character(PCJob)=="Yes","Female PC", "Female No PC"))

Bank1$GenderTech <- with(Bank1, ifelse(as.character(Gender)=="Male", "Male" , ifelse(as.character(Gender)=="Female" & as.character(PCJob)=="Yes","Female PC", "Female No PC")))


RegGT = lm(Salary ~ JobGrade + GenderTech + Experience + Age, data = Bank1)

Bank1$Experience = Bank1$YrsPrior + (95 - Bank1$YrHired)
Bank1$Age = 95 - Bank1$YrBorn

RegGT = lm(Salary ~ JobGrade + GenderTech + Experience + Age, data = Bank1)
summary(RegGT)
      
                         
