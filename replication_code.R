
library(ggplot2)

COVID_PSU <- read_excel("~/COVID_PSU.xlsx")

p4<-ggplot(COVID_PSU, aes(Date,Positivity)) +
  geom_point(aes(color = Type, alpha = Positivity), size = 4) +
  scale_alpha_continuous(range = c(0.3, 0.8), guide = F) +
  geom_smooth(aes(color = Type), se = TRUE, size = 2) +
  labs(title = "D. Positivity Rate", y = "D. Positivity", x = "Week", caption = "Source: University Reports") +
  theme(text=element_text(size = 12, family="Times New Roman"),
        plot.subtitle = element_text(size = 12),
        legend.title=element_blank(),
        axis.text = element_text(size = 10),
        panel.background = element_rect(fill="white"),
        panel.grid.minor = element_line(color="grey90"),
        panel.grid.major = element_line(color="grey90"),
        plot.margin = unit(c(0.5,0.5,0.5,0.5), "cm"),
        legend.position = "bottom")

p1<-ggplot(COVID_PSU, aes(Date,Total,color=Type)) +
  geom_line(size=1.5)+
  geom_point(size=4)+  labs(title = "A. Number of tests", subtitle = "", y = "Number of Tests per system", x = "Week") +
  theme(text=element_text(size = 12, family="Times New Roman"),
        plot.subtitle = element_text(size = 12),
        legend.title=element_blank(),
        axis.text = element_text(size = 10),
        panel.background = element_rect(fill="white"),
        panel.grid.minor = element_line(color="grey90"),
        panel.grid.major = element_line(color="grey90"),
        plot.margin = unit(c(0.5,0.5,0.5,0.5), "cm"),
        legend.position = "bottom")

p2<-ggplot(COVID_PSU, aes(Date,Positives,color=Type)) +
  geom_line(size=1.5)+
  geom_point(size=4)+  labs(title = "B. Positives per week", subtitle = "", y = "Positive Tests per system", x = "Week") +
  theme(text=element_text(size = 12, family="Times New Roman"),
        plot.subtitle = element_text(size = 10),
        legend.title=element_blank(),
        axis.text = element_text(size = 10),
        panel.background = element_rect(fill="white"),
        panel.grid.minor = element_line(color="grey90"),
        panel.grid.major = element_line(color="grey90"),
        plot.margin = unit(c(0.5,0.5,0.5,0.5), "cm"),
        legend.position = "bottom")

p3<-ggplot(COVID_PSU, aes(Date,Cumulative,color=Type)) +
  geom_line(size=1.5)+
  geom_point(size=4)+  labs(title = "C. Epidemic Curve", subtitle = "", y = "Cumulative positives per system", x = "Week") +
  theme(text=element_text(size = 12, family="Times New Roman"),
        plot.subtitle = element_text(size = 10),
        legend.title=element_blank(),
        axis.text = element_text(size = 10),
        panel.background = element_rect(fill="white"),
        panel.grid.minor = element_line(color="grey90"),
        panel.grid.major = element_line(color="grey90"),
        plot.margin = unit(c(0.5,0.5,0.5,0.5), "cm"),
        legend.position = "bottom")


library(ggpubr)
ggarrange(p1,p2,p3,p4,ncol=2,nrow=2,common.legend = TRUE)

library(tidycensus)
options(tigris_class="sf") #Data from shapefiles will come as dataframes in R
options(tigris_use_cache=TRUE) #Stores files used in the analysis 

paper_variables<-c("B03002_004E","B01001_002E","B01001_026E" ,"B03002_012E","B03002_001E","B01001_003E",
                   "B01001_004E","B01001_005E","B01001_006E","B01001_027E",
                   "B01001_028E","B01001_029E","B01001_030E","B01001_001E",
                   "B01001_020E","B01001_021E","B01001_022E","B01001_023E",
                   "B01001_024E","B01001_025E","B01001_044E","B01001_045E",
                   "B01001_046E","B01001_047E","B01001_048E","B01001_049E",
                   "B15002_002E","B15002_003E","B15002_004E","B15002_005E",
                   "B15002_006E","B15002_007E","B15002_008E","B15002_009E",
                   "B15002_010E","B15002_011E","B15002_012E","B15002_013E",
                   "B15002_014E","B15002_015E","B15002_016E","B15002_001E",
                   "C24050_004E","C24050_002E","C24050_029E","C24050_001E",
                   "C24050_014E","B12006_006E","B12006_011E","B12006_017E",
                   "B12006_022E","B12006_029E","B12006_033E","B12006_039E",
                   "B12006_044E","B12006_050E","B12006_055E","B12006_004E",
                   "B12006_009E","B12006_015E","B12006_020E","B12006_026E",
                   "B12006_031E","B12006_037E","B12006_042E","B12006_048E",
                   "B12006_053E","B12006_010E","B12006_008E","B25011_013E",
                   "B25011_037E","B25011_001E","B07003_004E","B07003_007E",
                   "B07003_001E","B17001_002E","B17001_001E","B17005_006E",
                   "B17005_011E","B17005_017E","B17005_022E","B17005_001E",
                   "B12006_021E","B12006_032E","B12006_043E","B12006_054E",
                   "B12006_019E","B12006_030E","B12006_041E","B12006_052E",
                   "B05001_006E","B05001_001E","B26001_001E","B01002_001E","B06011_001E")

paper_data<-get_acs(geography="zcta",variables=paper_variables,year=2018, output="wide")

zip_codes<-c("16801","16802","16803")

paper_data<-subset(paper_data,GEOID %in% zip_codes)

attach(paper_data) #Will make the data part of R, and then will send it back to the original database. 
#This does not requires us to write "paper_data" everytime in front of each variable. 
#Note that we are creating the variables inside "paper_data"

paper_data$pov_percent<-B17001_002E/B17001_001E*100 #Calculates the percent of the population below the poverty level

paper_data$pct_black<-B03002_004E/B03002_001E*100 #Calculates the percent non-Hispanic black

paper_data$pct_hispanic<-B03002_012E/B03002_001E*100

paper_data$pct_under18years<-(B01001_003E+B01001_004E+B01001_005E+B01001_006E+B01001_027E+B01001_028E+B01001_029E+B01001_030E)/B01001_001E*100 #Calculates the percent of the population under 18 years (0-17 years old)

paper_data$pct_65andolder<-(B01001_020E+B01001_021E+B01001_022E+B01001_023E+B01001_024E+B01001_025E+B01001_044E+B01001_045E+B01001_046E+
                              B01001_047E+B01001_048E+B01001_049E)/B01001_001E*100
#Calculates the percent of the population that is 65 years and older

paper_data$pct_immobile_1yr<-(B07003_004E+B07003_007E)/B07003_001E*100 #Calculates percent immbole 1 year ago

paper_data$percent_lessthanHS<-(B15002_002E+B15002_003E+B15002_004E+B15002_005E+B15002_006E+B15002_007E+B15002_008E+B15002_009E+B15002_010E+
                                  B15002_011E+B15002_012E+B15002_013E+B15002_014E+B15002_015E+B15002_016E)/B15002_001E*100
#Calculates the percent of the population over 25 years without a high school education 

#The following code classified counties as southern if they are in a state in the Southern Census Region. It assigns a one to southern counties and a zero to non-southern counties.
paper_data$pct_manufacturing<-C24050_004E/C24050_001E*100 #Population working in Manufacturing Sector 

paper_data$pct_extractive<-C24050_002E/C24050_001E*100    #Population working in Agriculture, Forestry, Fishing, and Mining

paper_data$pct_service<-C24050_029E/C24050_001E*100       #Population woking in the Service Sector

paper_data$pct_government<-C24050_014E/C24050_001E*100    #Population working in Public Administration (Government)

paper_data$pct_female_headed_hh<-(B25011_013E+B25011_037E)/B25011_001E*100 #Female Headed Households

paper_data$pct_fem_employed<-(B12006_010E+B12006_021E+B12006_032E+B12006_043E+B12006_054E)/(B12006_008E+B12006_019E+B12006_030E+B12006_041E+B12006_052E)*100 #Percent of females employed

paper_data$pct_unemployed<-(B17005_006E+B17005_011E+B17005_017E+B17005_022E)/B17005_001E*100 #Percent unemployed

paper_data$pov<-(paper_data$pov_percent/(1-paper_data$pov_percent)) #The author's make this transformation in the original paper.

paper_data$pov_log<-log(paper_data$pov,base = exp(1)) #The author's make this transformation in the original paper. 

paper_data$noncit<-(B05001_006E/B05001_001E)*100

paper_data$gq<-B26001_001E

table(paper_data$GEOID,paper_data$B01001_001E)
table(paper_data$GEOID,paper_data$B01002_001E)
table(paper_data$GEOID,paper_data$gq)
table(paper_data$GEOID,paper_data$pct_65andolder)

paper_data$male<-(paper_data$B01001_002E/paper_data$B01001_001E)*100
table(paper_data$GEOID,paper_data$male)
table(paper_data$GEOID,paper_data$noncit)
table(paper_data$GEOID,paper_data$pct_black)
table(paper_data$GEOID,paper_data$pct_hispanic)
table(paper_data$GEOID,paper_data$B06011_001E)
table(paper_data$GEOID,paper_data$pov_percent)
table(paper_data$GEOID,paper_data$percent_lessthanHS)