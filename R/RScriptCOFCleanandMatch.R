head (cofdata)
str (cofdata)
cofbasic <- cofdata%>%
select(DataItem, Age, County, Date, Owner_LastName, Owner_FirstName, Freed_LastName,
Freed_FirstName, Alias, Sex, PriorStatus, Height)
str (cofbasic)
cofbasic$DataItemC <- as.numeric (cofbasic$DataItem)
rm (evals)
rm (m_bty)
rm (m_bty_gen)
rm(m_elim1)
rm(m_elim2)
rm(m_rull)
rm (single)
rm (res)
rm (_4)
rm (_5)
rm (_6)
str (cofbasic)
cofbasic$cofyear <- extractyr(cofbasic$Date)
#Make the cofyear a numeric field.
cofbasic$cofyear <- as.numeric (cofbasic$cofyear)
cofbasic %<>%
mutate(cofAge = Age)%>%
cofAge<- as.numeric (cofAge)%>%
mutate (cofDate = Date) %>%
cofDate <- as.Date.numeric(cofDate)
str (cofbasic)
cofbasic <- cofbasic %>% replace(.=="NULL", NA) # replace with NA
str (cofbasic)
cofbasic$Age <- as.numeric (cofbasic$Age)
cofbasic$County <- as.factor (cofbasic$County)
cofbasic$Sex <- as.factor (cofbasic$Sex)
cofbasic$PriorStatus <- as.factor(cofbasic$PriorStatus)
str (cofbasic)
AAcof <- cofbasic %>%
subset(County == AA)
AAcof <- cofbasic %>%
subset(County == "AA")
head (AAcof)
dim (AAcof)
AAmanu <- basic %>%
subset(County == "AA")
dim (AAbasic)
head (AAbasic)
subset(County == "AA")
dim (AAmanu)
head (AAmanu)
AAmanu$Owner_LastName
names (AAcof)
rm (AAcof$DataItemC)
colnames(AAcof)[colnames(AAcof)=="DataItem"] <- "cofDataItem"
colnames(AAcof)[colnames(AAcof)=="Age"] <- "cofAge"
colnames(AAcof)[colnames(AAcof)=="Date"] <- "cofDate"
levels (AAcof)
levels (AAcof$PriorStatus)
AAcof$PriorStatus <- if (AAcof$PriorStatus == "?")
"NA"
levels (AAcof$PriorStatus)
head (AAcof$PriorStatus)
str (AAcof)
AAcof <- cofbasic %>%
subset(County == "AA")
dim (AAcof)
head (AAcof)
colnames(AAcof)[colnames(AAcof)=="DataItem"] <- "cofDataItem"
colnames(AAcof)[colnames(AAcof)=="Age"] <- "cofAge"
colnames(AAcof)[colnames(AAcof)=="Date"] <- "cofDate"
str (AAcof)
levels (PriorStatus)
AAcof$PriorStatus[AAcof$PriorStatus == "Slave"]
AAcof$PriorStatus
test <- AAmanu %>% inner_join(AAcof, by = "Owner_Name")
names (AAmanu)
names (AAcof)
test <- AAmanu %>% inner_join(AAcof, by = "Owner_LastName")
dim (test)
head (test)
view (test)
test <- inner_join(AAcof, AAmanu, by = c("Owner_LastName", "Owner_FirstName", "Freed_LastName"= "Slave_LastName"))
view (test)
AAmanu %<>%
subset(Owner_LastName, Owner_FirstName, Slave_LastName, Slave_FirstName)
AAmanu %<>%
subset(., Owner_LastName, Owner_FirstName, Slave_LastName, Slave_FirstName)
AAmanu %<>%
subset(Owner_LastName, Owner_FirstName, Slave_LastName, Slave_FirstName)
red_AAmanu <- AAmanu %>
red_AAmanu <- AAmanu %>%
select(Owner_LastName, Owner_FirstName, Slave_LastName, Slave_FirstName)
head (redAAmanu)
head (red_AAmanu)
red_AAcof <- AAcof %>%
select(Owner_LastName, Owner_Firstname, Freed_LastName, Freed_FirstName)
red_AAcof <- AAcof %>%
select(Owner_LastName, Owner_FirstName, Freed_LastName, Freed_FirstName)
head (red_AAcof)
test <- red_AAcof %>%
inner_join(red_AAmanu, by = c("Owner_LastName", "Owner_FirstName", "Slave_LastName" =
"Freed_LastName", "Slave_FirstName" = "Freed_FirstName"))
test <- red_AAcof %>%
inner_join(red_AAmanu, by = c("Owner_LastName", "Owner_FirstName"))
view (test)
test2 < - test %>%
select ("Freed_FirstName" = "Slave_FirstName")
test2 <- test %>%
select ("Freed_FirstName" = "Slave_FirstName")
view (test2)
test2 <- test %>%
select (test2["Freed_FirstName" = "Slave_FirstName", ])
test2 <- test[test$Freed_FirstName==test$Slave_FirstName,]
view (test2)
test2 <- test[test$Freed_FirstName==test$Slave_FirstName, ]
tail (test2)
tail (AAcof)
test3 <- test2[test2$Freed_FirstName !== "NA" | test2$Slave_FirstName !== "NA"]
test3 <- test2[test2$Freed_FirstName != "NA" | test2$Slave_FirstName != "NA"]
test3 <- test2[complete.cases(test2)]
view (test2)
test3 <- omit.na (test2)
test3 <- na.omit (test2)
view (test3)
test3 <- test2 %>%
select (test2[Owner_FirstName != OwnerLastName])
test3 <- test2 %>%
select (test2[Owner_FirstName != Owner_LastName])
test3 <- test2 %>%
select (test2[Owner_FirstName != Owner_LastName, ])
names (test2)
tail (test2)
test2[Owner_FirstName = Owner_LastName]
test2[test2$Owner_FirstName = test2$Owner_LastName]
test2[test2$Owner_FirstName == test2$Owner_LastName]
test3 <- test2 %>%
test2[test2$Owner_FirstName != test2$Owner_LastName, ]
dim (test2)
red_AAmanu <- AAmanu %>%
select(Owner_LastName, Owner_FirstName, Slave_LastName, Slave_FirstName)
red_AAcof <- AAcof %>%
select(Owner_LastName, Owner_FirstName, Freed_LastName, Freed_FirstName)
head (red_AAmanu)
head (red_AAcof)
test <- red_AAcof %>%
inner_join(red_AAmanu, by = c("Owner_LastName", "Owner_FirstName"))
view (test)
test2 < - test %>%
select ("Freed_FirstName" = "Slave_FirstName", rm.na=TRUE)
test2 <- test[test$Freed_FirstName==test$Slave_FirstName, ]
view (test2)
write.csv(cofbasic, file="cofbasic.csv")
write.csv(AAcof, file="AAcof.csv")
write.csv(AAmanu, file = "AAmanu.csv")
write.csv(cofdata, file = "cofdata.csv")
write.csv(red_AAcof, file = "red_AAcof.csv")
write.csv(red_AAmanu, file = "red_AAmanu.csv")
