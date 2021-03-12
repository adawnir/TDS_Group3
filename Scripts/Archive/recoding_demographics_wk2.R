library(data.table)
library(openxlsx)

mydata=data.frame(fread("ukb26390.csv"))
mycoding=read.csv("Codings_Showcase.csv")

withdrawn=as.character(read.csv("w19266_20200204.csv")[,1])
withdrawn <- as.list(as.integer(withdrawn))

mydata2 <- mydata
# make a new dataset without the patients who withdrew consent
mydata2$eid <- as.character(mydata2$eid)
mydata2 <- mydata[!mydata$eid %in% withdrawn, ]


## EDUCATION : 6138

########
coding_id="100305"
print(head(mycoding))
mycoding_field=mycoding[which(mycoding[,1]==coding_id),]
mycoding_field=mycoding_field[,-1]
rownames(mycoding_field)=mycoding_field[,1]
print(mycoding_field)

# As it is in raw data:
print(mydata2$X6138.0.0)

# Recoded categories:
mydata2$X6138.0.0 <- as.character(mycoding_field[as.character(mydata2$X6138.0.0),"Meaning"])
summary(mydata2$X6138.0.0)

# Recode as factors:
mydata2$X6138.0.0 <- as.factor(mydata2$X6138.0.0)
summary(mydata2$X6138.0.0)
print(mydata2$X6138.0.0, row =5)

########

## CURRENT EMPLOYMENT : 6142

########
coding_id_employment="100295"
mycoding_field_employment=mycoding[which(mycoding[,1]==coding_id_employment),]
mycoding_field_employment=mycoding_field_employment[,-1]
rownames(mycoding_field_employment)=mycoding_field_employment[,1]
print(mycoding_field_employment)

# As it is in raw data:
print(mydata2$X6142.0.0)

# Recoded categories:
mydata2$X6142.0.0 <- as.character(mycoding_field_employment[as.character(mydata2$X6142.0.0),"Meaning"])
summary(mydata2$X6142.0.0)

# Recode as factors:
mydata2$X6142.0.0 <- as.factor(mydata2$X6142.0.0)
summary(mydata2$X6142.0.0)


########

## AVG TOTAL HOUSEHOLD INCOME : 738

########
coding_id_income="100294"
mycoding_field_income=mycoding[which(mycoding[,1]==coding_id_income),]
mycoding_field_income=mycoding_field_income[,-1]
rownames(mycoding_field_income)=mycoding_field_income[,1]
print(mycoding_field_income)

# As it is in raw data:
print(mydata2$X738.0.0)

# Recoded categories:
mydata2$X738.0.0 <- as.character(mycoding_field_income[as.character(mydata2$X738.0.0),"Meaning"])
summary(mydata2$X738.0.0)

# Recode as factors:
mydata2$X738.0.0 <- as.factor(mydata2$X738.0.0)
summary(mydata2$X738.0.0)


########

## ETHNIC BACKGROUND : 21000

########
coding_id_ethnic="1001"
mycoding_field_ethnic=mycoding[which(mycoding[,1]==coding_id_ethnic),]
mycoding_field_ethnic=mycoding_field_ethnic[,-1]
rownames(mycoding_field_ethnic)=mycoding_field_ethnic[,1]
print(mycoding_field_ethnic)

# As it is in raw data:
print(mydata2$X21000.0.0)

# Recoded categories:
mydata2$X21000.0.0 <- as.character(mycoding_field_ethnic[as.character(mydata2$X21000.0.0),"Meaning"])
summary(mydata2$X21000.0.0)

# Recode as factors:
mydata2$X21000.0.0 <- as.factor(mydata2$X21000.0.0)
summary(mydata2$X21000.0.0)


########

## DEPRIVATION SUMMARY
summary(mydata2$X189.0.0)
#######

# all the summaries because i'm lazy 
summary(mydata2$X6138.0.0) # education
summary(mydata2$X6142.0.0) # employment
summary(mydata2$X738.0.0) # avg total household income
summary(mydata2$X21000.0.0) # ethnic background
summary(mydata2$X189.0.0) # deprivation

# plots
plot(mydata2$X6138.0.0) # education
ggplot(data = mydata2) + 
  geom_bar(mapping = aes(x = X6138.0.0, fill = X6138.0.0))
plot(mydata2$X6142.0.0) # employment
plot(mydata2$X738.0.0) # avg total household income
plot(mydata2$X21000.0.0) # ethnic background
plot(mydata2$X189.0.0)