### setup ####

#function defined to read sheets in excel files
read_excel_allsheets <- function(filename, tibble = FALSE) {
  sheets <- readxl::excel_sheets(filename)
  x <- lapply(sheets, function(X) readxl::read_excel(filename, sheet = X))
  if(!tibble) x <- lapply(x, as.data.frame)
  names(x) <- sheets
  x
}


#would be nice if you could just input the folder path and then it could search for a file named "ES.xlsx" and a file named "UI.xlsx" in that folder and run this script

### eversource data ####

ES.file <- paste(folder,"ES.xlsx",sep="")

data <- read_excel_allsheets(ES.file) #input file location
data <- data[3:5]
names(data) <- c("ES.small","ES.large","ES.HES")



#wrangling data
ES.small <- data$ES.small %>% 
  select(`Census Tract`,Town,`Distressed Tract`, #keeping only relevant columns in new data frame
         `CLM $ Collected`,`Incentive Disbursements`,
         `Residential CLM $ Collected`,`Residential Incentive Disbursements`,
         `C&I CLM $ Collected`,`C&I Incentive Disbursements`)
ES.small <-pivot_longer(ES.small,cols=c(`CLM $ Collected`,`Incentive Disbursements`,
                                        `Residential CLM $ Collected`,`Residential Incentive Disbursements`, #making long instead of wide
                                        `C&I CLM $ Collected`,`C&I Incentive Disbursements`),
                        names_to="Type")
ES.small <- ES.small %>% 
  mutate("Size"="Small") %>%
  mutate("Cat"=case_when(Type=="CLM $ Collected" ~ "total", #creating category for customer type so that it will be distinguishable when merged
                         Type=="Incentive Disbursements" ~ "total",
                         Type=="Residential CLM $ Collected" ~ "residential",
                         Type=="Residential Incentive Disbursements" ~ "residential",
                         Type=="C&I CLM $ Collected" ~ "C&I",
                         Type=="C&I Incentive Disbursements" ~ "C&I")) %>% 
  mutate("Col"=case_when(Type=="CLM $ Collected" ~ "collections", #creating category for collect/disburse for later column defining
                         Type=="Incentive Disbursements" ~ "disbursements",
                         Type=="Residential CLM $ Collected" ~ "collections",
                         Type=="Residential Incentive Disbursements" ~ "disbursements",
                         Type=="C&I CLM $ Collected" ~ "collections",
                         Type=="C&I Incentive Disbursements" ~ "disbursements")) %>% 
  select(-Type)
ES.small <- ES.small %>% 
  pivot_wider(names_from="Col",values_from="value") #creating collections and disbursements columns

#repeating above but for large customers
ES.large <- data$ES.large %>% 
  select(`Census Tract`,Town,`Distressed Tract`,
         `CLM $ Collected`,`Incentive Disbursements`,
         `Residential CLM $ Collected`,`Residential Incentive Disbursements`,
         `C&I CLM $ Collected`,`C&I Incentive Disbursements`)
ES.large <-pivot_longer(ES.large,cols=c(`CLM $ Collected`,`Incentive Disbursements`,
                                        `Residential CLM $ Collected`,`Residential Incentive Disbursements`,
                                        `C&I CLM $ Collected`,`C&I Incentive Disbursements`),
                        names_to="Type")
ES.large <- ES.large %>% 
  mutate("Size"="Large") %>% 
  mutate("Cat"=case_when(Type=="CLM $ Collected" ~ "total",
                         Type=="Incentive Disbursements" ~ "total",
                         Type=="Residential CLM $ Collected" ~ "residential",
                         Type=="Residential Incentive Disbursements" ~ "residential",
                         Type=="C&I CLM $ Collected" ~ "C&I",
                         Type=="C&I Incentive Disbursements" ~ "C&I")) %>% 
  mutate("Col"=case_when(Type=="CLM $ Collected" ~ "collections",
                         Type=="Incentive Disbursements" ~ "disbursements",
                         Type=="Residential CLM $ Collected" ~ "collections",
                         Type=="Residential Incentive Disbursements" ~ "disbursements",
                         Type=="C&I CLM $ Collected" ~ "collections",
                         Type=="C&I Incentive Disbursements" ~ "disbursements")) %>% 
  select(-Type)
ES.large <- ES.large %>% 
  pivot_wider(names_from="Col",values_from="value")

#HES doesn't have as many categories, so just changing column names to match
ES.HES <- data$ES.HES %>%
  mutate("Size"="HES") %>% 
  select(`Census Tract`,Town,`Distressed Tract`,`CLM $ Collected`,`HES Incentives`,`HES-IE Incentives`,Size) %>%
  rename(collections="CLM $ Collected",
         HES=`HES Incentives`,
         HES_IE=`HES-IE Incentives`) %>% 
  pivot_longer(cols=c(HES,HES_IE),
               names_to="Cat",
               values_to="disbursements")

ES.HES.participation <- data$ES.HES %>% 
  select(`Census Tract`,Town,`Distressed Tract`,`HES Single`,`HES 2-4`,`HES 4+`,`HES-IE Single`,`HES-IE 2-4`,`HES-IE 4+`) %>% 
  pivot_longer( cols=starts_with("HES"),
    names_to = c(".value", "units"),
    names_sep = " ") %>% 
  pivot_longer(cols=c(`HES`,`HES-IE`),
               names_to="program",
               values_to="count") %>% 
  mutate("Utility"="ES")
  
  
ES <- rbind(ES.small,ES.large,ES.HES) %>%  #merge
  mutate("Utility"="ES") 

### UI data ####

UI.file <- paste(folder,"UI.xlsx",sep="")


#reading in
data <- read_excel_allsheets(UI.file) #input file location
data <- data[3:5]
names(data) <- c("UI.small","UI.large","UI.HES")

#same as above
UI.small <- data$UI.small %>% 
  select(`Census Tract`,Town,`Distressed Tract`, #keeping only relevant columns in new data frame
         `CLM $ Collected`,`Incentive Disbursements`,
         `Residential CLM $ Collected`,`Residential Incentive Disbursements`,
         `C&I CLM $ Collected`,`C&I Incentive Disbursements`)
UI.small <-pivot_longer(UI.small,cols=c(`CLM $ Collected`,`Incentive Disbursements`,
                                        `Residential CLM $ Collected`,`Residential Incentive Disbursements`, #making long instead of wide
                                        `C&I CLM $ Collected`,`C&I Incentive Disbursements`),
                        names_to="Type")
UI.small <- UI.small %>% 
  mutate("Size"="Small") %>%
  mutate("Cat"=case_when(Type=="CLM $ Collected" ~ "total", #creating category for customer type so that it will be distinguishable when merged
                         Type=="Incentive Disbursements" ~ "total",
                         Type=="Residential CLM $ Collected" ~ "residential",
                         Type=="Residential Incentive Disbursements" ~ "residential",
                         Type=="C&I CLM $ Collected" ~ "C&I",
                         Type=="C&I Incentive Disbursements" ~ "C&I")) %>% 
  mutate("Col"=case_when(Type=="CLM $ Collected" ~ "collections", #creating category for collect/disburse for later column defining
                         Type=="Incentive Disbursements" ~ "disbursements",
                         Type=="Residential CLM $ Collected" ~ "collections",
                         Type=="Residential Incentive Disbursements" ~ "disbursements",
                         Type=="C&I CLM $ Collected" ~ "collections",
                         Type=="C&I Incentive Disbursements" ~ "disbursements")) %>% 
  select(-Type)
UI.small <- UI.small %>% 
  pivot_wider(names_from="Col",values_from="value") #creating collections and disbursements columns

#repeating above but for large customers
UI.large <- data$UI.large %>% 
  select(`Census Tract`,Town,`Distressed Tract`,
         `CLM $ Collected`,`Incentive Disbursements`,
         `Residential CLM $ Collected`,`Residential Incentive Disbursements`,
         `C&I CLM $ Collected`,`C&I Incentive Disbursements`)
UI.large <-pivot_longer(UI.large,cols=c(`CLM $ Collected`,`Incentive Disbursements`,
                                        `Residential CLM $ Collected`,`Residential Incentive Disbursements`,
                                        `C&I CLM $ Collected`,`C&I Incentive Disbursements`),
                        names_to="Type")
UI.large <- UI.large %>% 
  mutate("Size"="Large") %>%
  mutate("Cat"=case_when(Type=="CLM $ Collected" ~ "total", #creating category for customer type so that it will be distinguishable when merged
                         Type=="Incentive Disbursements" ~ "total",
                         Type=="Residential CLM $ Collected" ~ "residential",
                         Type=="Residential Incentive Disbursements" ~ "residential",
                         Type=="C&I CLM $ Collected" ~ "C&I",
                         Type=="C&I Incentive Disbursements" ~ "C&I")) %>% 
  mutate("Col"=case_when(Type=="CLM $ Collected" ~ "collections", #creating category for collect/disburse for later column defining
                         Type=="Incentive Disbursements" ~ "disbursements",
                         Type=="Residential CLM $ Collected" ~ "collections",
                         Type=="Residential Incentive Disbursements" ~ "disbursements",
                         Type=="C&I CLM $ Collected" ~ "collections",
                         Type=="C&I Incentive Disbursements" ~ "disbursements")) %>% 
  select(-Type)
UI.large <- UI.large %>% 
  pivot_wider(names_from="Col",values_from="value")

#HES doesn't have as many categories, so just changing column names to match
UI.HES <- data$UI.HES %>%
  mutate("Size"="HES") %>% 
  select(`Census Tract`,Town,`Distressed Tract`,`CLM $ Collected`,`HES Incentives`,`HES-IE Incentives`,Size) %>%
  rename(collections="CLM $ Collected",
         HES=`HES Incentives`,
         HES_IE=`HES-IE Incentives`) %>% 
  pivot_longer(cols=c(HES,HES_IE),
               names_to="Cat",
               values_to="disbursements")

UI.HES.participation <- data$UI.HES %>% 
  select(`Census Tract`,Town,`Distressed Tract`,`HES Single`,`HES 2-4`,`HES 4+`,`HES-IE Single`,`HES-IE 2-4`,`HES-IE 4+`) %>% 
  pivot_longer( cols=starts_with("HES"),
                names_to = c(".value", "units"),
                names_sep = " ") %>% 
  pivot_longer(cols=c(`HES`,`HES-IE`),
               names_to="program",
               values_to="count") %>% 
  mutate("Utility"="UI")

UI <- rbind(UI.small,UI.large,UI.HES) %>%  
  mutate("Utility"="UI")#merge

### altogether ####
data <- rbind(UI,ES)%>% 
  filter(`Distressed Tract`!="Totals",
         `Distressed Tract`!="Total") %>% 
  mutate(collections=as.numeric(collections),
         disbursements=as.numeric(disbursements),
         `Census Tract`=as.numeric(`Census Tract`),
         Town=toupper(Town),
         `Distressed Tract`=toupper(`Distressed Tract`),
         Size=toupper(Size),
         Cat=toupper(Cat))

HES <- rbind(ES.HES.participation,UI.HES.participation)%>% 
  filter(`Distressed Tract`!="Totals",
         `Distressed Tract`!="Total",
         `Distressed Tract`!="(blank)") %>% 
  mutate(count=as.numeric(count),
         `Census Tract`=as.numeric(`Census Tract`),
         Town=toupper(Town),
         `Distressed Tract`=toupper(`Distressed Tract`),
         units=toupper(units))

