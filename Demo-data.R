Energy_burden <- read.csv("Energy_burden.csv") %>% 
  select(Census.tract,Energy.burden) %>% 
  rename(`Census Tract`=Census.tract) %>% 
  mutate(Energy.burden=toupper(Energy.burden))

data <- left_join(data,Energy_burden,by="Census Tract")

Mod_distress <- read.csv("Mod_distress.csv") %>% 
  rename(`Census Tract`=Census.Tract) %>% 
  mutate(Moderate.distress=toupper(Moderate.distress))

data <- left_join(data,Mod_distress,by="Census Tract")

Racial_diversity <- read.csv("Racial_diversity.csv")%>% 
  select(tract,percent) %>% 
  rename(`Census Tract`=tract,
         Racial.diversity=percent)

data <- left_join(data,Racial_diversity,by="Census Tract")

J40 <- read.csv("Justice40.csv") 

Justice40 <- J40 %>% 
  select(Census.tract.2010.ID,Identified.as.disadvantaged) %>% 
  rename(`Census Tract`=Census.tract.2010.ID,
         Justice40.disadvantaged=Identified.as.disadvantaged)

data <- left_join(data,Justice40,by="Census Tract")

