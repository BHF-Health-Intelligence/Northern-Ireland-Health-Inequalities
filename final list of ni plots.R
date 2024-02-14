##Figure 2: SMOKING
smoking_data <- data.frame(
  "Year" = c("2010/11", "2011/12", "2012/13", "2013/14", "2014/15", "2015/16", "2016/17", "2017/18", "2018/19", "2019/20", "2020/21", "2021/22","2022/23"),
  "Quintile 1" = c(40, 39, 37, 34, 37, 36, 31, 31, 29, 27, 22, 29,24),
  "Quintile 2" = c(26, 26, 28, 26, 26, 28, 23, 24, 21, 20, 16, 23,16),
  "Quintile 3" = c(22, 23, 22, 20, 22, 21, 19, 16, 17, 17, 8, 14,12),
  "Quintile 4" = c(20 ,19 ,20 ,17 ,17 ,15 ,16 ,13 ,15 ,15 ,10 ,13,11),
  "Quintile 5" = c(14 ,18 ,12 ,12 ,12 ,14 ,11 ,12 ,12 ,10 ,7 ,10,7)
)

smoking_long <- smoking_data %>%
  pivot_longer(!Year, names_to = "Deprivation_Quintile", values_to = "Percentage")
smoking_long$Deprivation_Quintile <- gsub("Quintile.3", "3", smoking_long$Deprivation_Quintile)
smoking_long$Deprivation_Quintile <- gsub("Quintile.1", "1 (most deprived)", smoking_long$Deprivation_Quintile)
smoking_long$Deprivation_Quintile <- gsub("Quintile.2", "2", smoking_long$Deprivation_Quintile)
smoking_long$Deprivation_Quintile <- gsub("Quintile.4", "4", smoking_long$Deprivation_Quintile)
smoking_long$Deprivation_Quintile <- gsub("Quintile.5", "5 (least deprived)", smoking_long$Deprivation_Quintile)
smoking_plot <- ggplot(smoking_long, aes(x = Year, y = Percentage, group = Deprivation_Quintile, color = Deprivation_Quintile)) +
  geom_point(size = 3) +
  geom_line(size = 2.6) +
  bhf_style() +
  scale_color_bhf(palette = "red and light blue", name = "Deprivation Quintile", labels = c("1 = most deprived", "2", "3", "4", "5"="Least Deprived")) +
  scale_alpha_manual(values=c(1,0.4,0.4,0.4, 0.4,0.4,0.4,0.4, 0.4, 1), guide = "none") +
  labs(title = "Percentage of those in Northern Ireland who currently smoke cigarettes, by deprivation quintile from 2010 to 2023", x = "Year",
       y = "Percentage (%)", caption=str_wrap("Data Source: Health Survey Northern Ireland. Data for 2020/21 should be treated with caution due to the significantly smaller sample size of the Health Survey Northern Ireland in the first year of the pandemic.",100)) +
  scale_y_continuous(limits = c(0,75)) +
  theme(plot.caption.position = "plot")+
  theme(axis.text = element_text(size = 14),
        title = element_text(size = 16),
        axis.title.x = element_text(size = 20),
        axis.title.y= element_text(size = 20),
        plot.title= element_text(size = 20),
        legend.text=element_text(size=15))
smoking_plot

##Obesity
obesity_data <- read_excel("~/Projects/obesity_data.xlsx")
obesity_long <- obesity_data %>%
  pivot_longer(!Deprivation_Quintile, names_to = "Year", values_to ="Percentage")
obesity_plot <- ggplot(obesity_long, aes(x = Year, y = Percentage, group = Deprivation_Quintile, color = Deprivation_Quintile,alpha=Deprivation_Quintile)) +
  geom_point(size = 3) +
  geom_line(size = 2.3) +
  bhf_style() +
  scale_color_bhf(palette = "red and light blue", name = "Deprivation Quintile", guide = guide_legend(override.aes = list(size = 8))) +
  scale_alpha_manual(values=c(1,0.4,0.4,0.4, 1), guide = "none") +
  scale_y_continuous(labels = scales::percent, limits=c(0,0.35))+
  labs(title = "Percent of adults in Northern Ireland who are classified as obese by deprivation quintile", y="Percentage (%)", caption="Data Source: Health Survey Northern Ireland 22/23")+
  theme(plot.caption.position = "plot")+
  theme(axis.text = element_text(size = 14),
        title = element_text(size = 17),
        axis.title.x = element_text(size = 20),
        axis.title.y= element_text(size = 20),
        plot.title= element_text(size = 20),
        legend.text=element_text(size=15))
obesity_plot

p1_obesity <- read_excel("~/Projects/p1_obesity.xlsx")
p1_longer <- p1_obesity%>%
  pivot_longer(!"Proportion of Pupils (%)", names_to = "Year", values_to = "Percentage")
p1_longer <- p1_longer %>%
  rename(Deprivation_Quintile="Proportion of Pupils (%)")
p1_plot <- ggplot(p1_longer, aes(x = Year, y = Percentage, group = Deprivation_Quintile, fill = Deprivation_Quintile)) +
  #geom_bar(position = "dodge", stat = "identity") +
  geom_bar(stat = "identity", position = position_dodge(width = 1)) +
  scale_fill_bhf(palette = 'red and light blue') +
  bhf_style() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  geom_text(aes(label = sprintf("%.1f", Percentage)), position = position_dodge(width = 1), hjust = 0.5, vjust=-0.4) +
  labs(title = "Percentage of N. Ireland children in primary 1 who are classifed as obese from 2015-2022", y="Percentage (%)",fill="Deprivation Quintile", caption="Data source: Health Inequalities Annual Report 2023")+
  scale_y_continuous(breaks = seq(0, 10))+
  theme(plot.caption.position = "plot")+
  theme(axis.text = element_text(size = 14),
        title = element_text(size = 17),
        axis.title.x = element_text(size = 20),
        axis.title.y= element_text(size = 20),
        plot.title= element_text(size = 20),
        legend.text=element_text(size=15))
p1_plot

## figure 5 5 a day
five_data <- read_excel("~/Projects/five_data.xlsx")
five_long <- five_data %>%
  pivot_longer(!`Deprivation Quintile`, names_to = "Year", values_to = "Proportion")
five_plot <- ggplot(five_long, aes(x = Year, y = Proportion, group = `Deprivation Quintile`, color = `Deprivation Quintile`, alpha=`Deprivation Quintile`)) +
  geom_point(size = 2.6) +
  geom_line(size = 2) +
  bhf_style() +
  scale_color_bhf(palette = "red and light blue", name = "Deprivation Quintile", labels = c("1 = most deprived", "2", "3", "4", "5"="Least Deprived")) +
  scale_alpha_manual(values=c(1,0.4,0.4,0.4,1), guide = "none") +
  labs(title = "Proportion of adults in Northern Ireland who report meeting 5-a-day requirements by deprivation quintile", y="Percentage", caption="Data Source: Health Survey Northern Ireland 22/23")+
  scale_y_continuous(labels = scales::percent,limits = c(0,0.75)) +
  theme(plot.caption.position = "plot")+
  theme(axis.text = element_text(size = 14),
        title = element_text(size = 17),
        axis.title.x = element_text(size = 20),
        axis.title.y= element_text(size = 20),
        plot.title= element_text(size = 20),
        legend.text=element_text(size=15))
five_plot

#figure 6 alcohol
alc2 <- read_excel("Projects/alc2.xlsx")
alc2 <- alc2 %>% 
  pivot_longer(!`Deprivation quintile`,names_to="Year", values_to="Percentage")
alc2$Year <-as.factor(alc2$Year)
alc2_plot <- ggplot(alc2, aes(x=Year, y=Percentage, group=`Deprivation quintile`, color=`Deprivation quintile`, alpha=`Deprivation quintile`))+
  geom_point(size = 2.6) +
  geom_line(size = 2)+
  scale_color_bhf(palette = "red and light blue") +
  scale_alpha_manual(values=c(1,0.4,0.4,0.4,1), guide = "none") +
  bhf_style() +
  scale_y_continuous(labels = scales::percent,limits = c(0,0.5)) +
  labs(title ="Percentage of those who drink above weekly limit in each deprivation quintile in Northern Ireland from 2010-2023", caption="Data Source: Health Survey Northern Ireland 2022-23")+
  theme(plot.caption.position = "plot")+
  theme(axis.text = element_text(size = 14),
        title = element_text(size = 17),
        axis.title.x = element_text(size = 20),
        axis.title.y= element_text(size = 20),
        plot.title= element_text(size = 20),
        legend.text=element_text(size=15))
alc2_plot

##figure 7 prevalence
qof_data <- read_excel("~/Projects/qof_data.xlsx")
library(reshape2)
library(ggplot2)
qof_data$Deprivation_Rank<- as.factor(qof_data$Deprivation_Rank)
qof_melted <- melt(qof_data, id.vars = "Deprivation_Rank", measure.vars = c("Atrial Fibrillation", "Coronary Heart Disease", "Diabetes Mellitus", "Heart Failure", "Hypertension", "Stroke/TIA"))
qof_melted <- qof_melted %>%
  rename(Condition=variable)
qof_plot <- ggplot(qof_melted, aes(x = Deprivation_Rank, y = value, group = Condition, fill = Condition)) +
  geom_bar(stat = 'identity')+
  bhf_style() +
  facet_wrap(~Condition)+
  scale_fill_bhf(palette = 'red and light blue') +
  labs(title = "Prevalence of heart and circulatory conditions and risk factors at LGD level, ranked by level of LGD deprivation in 2022-2023", x= 'Deprivation Rank (1 = most deprived, 11 = least deprived)', y='Percentage', fill='Condition',caption='Data source:Northern Ireland Department of Health')+
  scale_y_continuous(limits = c(0,0.25),labels = scales::percent)+
  theme(strip.text =element_text(size=15))+
  theme(plot.caption.position = "plot")+
  theme(axis.text = element_text(size = 14),
        title = element_text(size = 17),
        axis.title.x = element_text(size = 20),
        axis.title.y= element_text(size = 20),
        plot.title= element_text(size = 20),
        legend.text=element_text(size=15))
qof_plot

##figure 8 BHF CVD estimate
qof2_data <- read_excel("~/Projects/qof2_data.xlsx")
qof2_data$Deprivation_Rank<-as.factor(qof2_data$Deprivation_Rank)
qof2_plot <- ggplot(qof2_data, aes(x=Deprivation_Rank, y=BHF_CVD_Estimate, fill=Deprivation_Rank)) +
  geom_bar(stat = 'identity') +
  labs(title = 'BHF CVD Estimate in N. Ireland by deprivation rank in 2022/23',
       x = 'Deprivation Rank (1 = most deprived, 11 = least deprived)',
       y = 'Prevalence',
       fill='Deprivation Rank',
       caption='Data source:Northern Ireland Department of Health')+
  scale_fill_bhf(palette = "red and light blue") +
  scale_alpha_manual(values=c(1,0.4,0.4,0.4, 0.4,0.4,0.4,0.4, 0.4, 1), guide = "none") +
  bhf_style() +
  theme(strip.text =element_text(size=15))+
  theme(plot.caption.position = "plot")+
  theme(axis.text = element_text(size = 14),
        title = element_text(size = 17),
        axis.title.x = element_text(size = 20),
        axis.title.y= element_text(size = 20),
        plot.title= element_text(size = 20),
        legend.text=element_text(size=15))
qof2_plot

##figure 9 SAR
sar_data <- read_excel("~/Projects/sar_data.xlsx")
sar_data <- sar_data %>%
  rename(Population=`Admissions_per_100,000`)
sar_long <- sar_data %>%
  pivot_longer(!Population, names_to = "Year", values_to="Admissions_per_100k")
sar_plot <- ggplot(sar_long, aes(x = Year, y = Admissions_per_100k, group = Population, color = Population)) +
  geom_point(size = 3) +
  geom_line(size = 2.3) +
  bhf_style() +
  scale_color_bhf(palette = 'red and light blue') +
  scale_y_continuous(limits = c(0,2400), label=comma)+
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  labs(title = "Heart and circulatory condition hospital admissions per 100,000 population from 2015-2022", y= "Admissions per 100,000 population",caption="Data Source: Northern Ireland Health Inequalities Annual Report 2023" )+
  theme(plot.caption.position = "plot")+
  theme(axis.text = element_text(size = 14),
        title = element_text(size = 17),
        axis.title.x = element_text(size = 20),
        axis.title.y= element_text(size = 20),
        plot.title= element_text(size = 20),
        legend.text=element_text(size=15))
sar_plot

#figure 10 SAR u75
SARU75 <- read_excel("~/Projects/SARU75.xlsx")
SARU75 <- SARU75 %>%
  rename(Population=Admissions)
SARU75 <- SARU75 %>%
  pivot_longer(!Population, names_to = "Year", values_to="Admissions")
saru75_plot <- ggplot(SARU75, aes(x = Year, y = Admissions, group = Population, color = Population)) +
  geom_point(size = 3) +
  geom_line(size = 2.3) +
  bhf_style() +
  scale_color_bhf(palette = "red and light blue")+
  scale_y_continuous(limits = c(0,1700),label=comma)+
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  labs(title = "Under 75s circulatory condition hospital admission rate from 2015-2022", y="Admissions per 100,000 population", fill= "Population Group", caption="Data Source: Northern Ireland Health Inequalities Annual Report 2023")+
  theme(plot.caption.position = "plot")+
  theme(axis.text = element_text(size = 14),
        title = element_text(size = 17),
        axis.title.x = element_text(size = 20),
        axis.title.y= element_text(size = 20),
        plot.title= element_text(size = 20),
        legend.text=element_text(size=15))
saru75_plot

#figure 11
gp2$`Deprivation rank` <-factor(gp2$`Deprivation rank`, levels=c("1","2","3","4","5","6","7","8","9","10","11"))
gp2_plot <- ggplot(gp2, aes(x=`Deprivation rank`, y=`GPs per 100,000`,fill=`Deprivation rank`))+
  geom_bar(position = "dodge", stat = "identity") +
  labs(title = 'Number of GPs per 100,000 population, by deprivation rank',
       x = 'Deprivation Rank (1=most deprived, 11=least deprived)',
       y = 'Number of GPs per 100,000',
       fill='Deprivation Rank', caption="Data Source:General Medical Services Statistics")+
  scale_fill_bhf(palette = "not reds") +
  bhf_style()+
  theme(plot.caption.position = "plot")+
  theme(axis.text = element_text(size = 14),
        title = element_text(size = 17),
        axis.title.x = element_text(size = 20),
        axis.title.y= element_text(size = 20),
        plot.title= element_text(size = 20),
        legend.text=element_text(size=15))
gp2_plot

#figure 12
gp_loss <- read_excel("~/Projects/gp_loss.xlsx")
gp_loss <- gp_loss %>% 
  rename(Rank=IMD)
gp_loss$Rank<-as.factor(gp_loss$Rank)

gp_loss_plot <- ggplot(gp_loss, aes(x=Rank,y=Percent_Loss,fill=Rank))+
  geom_bar(stat = "identity") +
  scale_fill_bhf(palette = 'red and light blue') +
  bhf_style() +
  theme(legend.position = "none") +
  scale_x_discrete(labels = function(Rank) str_wrap(Rank, width = 20)) +
  labs(title = "Percent change in number of GPs from 2014 to 2023, by decreasing deprivation",
       x = "Deprivation Rank (1=Most deprived)", y = "Percentage (%)",caption="Data Source: General Medical Services Statistics")+
  theme(plot.caption.position = "plot")+
  theme(axis.text = element_text(size = 14),
        title = element_text(size = 17),
        axis.title.x = element_text(size = 20),
        axis.title.y= element_text(size = 20),
        plot.title= element_text(size = 20),
        legend.text=element_text(size=15))
gp_loss_plot

##figure 13 pp_anti
pp_anti <- read_excel("~/Projects/pp_anti.xlsx")
ppanti_long <- pp_anti %>%
  pivot_longer(!pp, names_to ="Year", values_to = "Standardised_Prescription_Rate")
ppanti_long <- ppanti_long %>%
  rename(Population=pp)
ppanti_long <- ppanti_long %>%
  rename(Persons_prescribed_per_1000_population=Standardised_Prescription_Rate)
ppanti_plot <- ggplot(ppanti_long, aes(x = Year, y = Persons_prescribed_per_1000_population, group=Population, color= Population)) +
  geom_point(size = 3) +
  geom_line(size = 2.5) +
  bhf_style() +
  scale_color_bhf(palette = 'red and light blue') +
  labs(title = "Standardised Prescription Rate for Antihypertensives from 2017-2021", y="Persons prescribed per 1000 population", caption="Data Source: Northern Ireland Health Inequalities Annual Report 2023")+
  scale_y_continuous(limits = c(0,260))+
  theme(plot.caption.position = "plot")+
  theme(axis.text = element_text(size = 14),
        title = element_text(size = 17),
        axis.title.x = element_text(size = 20),
        axis.title.y= element_text(size = 20),
        plot.title= element_text(size = 20),
        legend.text=element_text(size=15))
ppanti_plot


#figure 14 statins
ppstatin_long <- pp_statin %>%
  pivot_longer(!Persons_Prescribed_per_1000_population, names_to ="Year", values_to = "Persons_prescribed_per_1000_population")
ppstatin_long <- ppstatin_long %>%
  rename(Population=Persons_Prescribed_per_1000_population)
ppstatin_plot <- ggplot(ppstatin_long, aes(x = Year, y = Persons_prescribed_per_1000_population, group = Population, color = Population)) +
  geom_point(size = 3) +
  geom_line(size = 2.5) +
  bhf_style() +
  scale_color_bhf(palette = 'red and light blue') +
  scale_alpha_manual(values=c(1,0.5,0.5,0.5, 1), guide = "none") +
  labs(title = "Standardised Prescription Rate for Statins from 2017-2021",y="Persons prescribed per 1000 population",caption="Data Source: Northern Ireland Health Inequalities Annual Report 2023")+
  scale_y_continuous(limits = c(0,210))+
  theme(plot.caption.position = "plot")+
  theme(axis.text = element_text(size = 14),
        title = element_text(size = 17),
        axis.title.x = element_text(size = 20),
        axis.title.y= element_text(size = 20),
        plot.title= element_text(size = 20),
        legend.text=element_text(size=15))
ppstatin_plot

#figur15 sdr u75
sdr_u75 <- read_excel("~/Projects/sdr_u75.xlsx")
sdr_u75 <- sdr_u75 %>% 
  rename(Deaths_100k=`Deaths per 100k`)
sdr_u75_plot<- ggplot(sdr_u75, aes(x = Year, y = Deaths_100k, group = `Deprivation Quintile`, color = `Deprivation Quintile`)) +
  geom_point(size = 2.2) +
  geom_line(size = 2.0) +
  scale_color_bhf(palette = 'not reds') +
  bhf_style() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  geom_text(aes(label =Deaths_100k ), vjust = -0.3, size=8,show.legend = FALSE ) +
  labs(title = "Standardised death rate for under-75s for circulatory conditions from 2013-2021", y="Deaths per 100,000 population",fill="Deprivation Quintile", caption="Data Source: Northern Ireland Health Inequalities Annual Report 2023")+
  scale_y_continuous(limits = c(0,130)) +
  theme(plot.caption.position = "plot")+
  theme(axis.text = element_text(size = 14),
        title = element_text(size = 17),
        axis.title.x = element_text(size = 20),
        axis.title.y= element_text(size = 20),
        plot.title= element_text(size = 20),
        legend.text=element_text(size=15))
sdr_u75_plot

#figure 16 u75 asdr
###
asdr_u75 <- read_excel("~/Projects/asdr_u75.xlsx")
asdr_melted <- melt(asdr_u75, id.vars = c("LGD", "Deprivation Rank"), measure.vars = c("asdr_men", "asdr_women", "asdr_total"))
asdr_melted$variable <- gsub("asdr_men", "Men", asdr_melted$variable)
asdr_melted$variable <- gsub("asdr_women", "Women", asdr_melted$variable)
asdr_melted$variable <- gsub("asdr_total", "Total", asdr_melted$variable)
asdr_melted <- asdr_melted %>%
  rename(ASDR=value)
asdr_melted <- asdr_melted %>%
  rename(Gender=variable)
asdr_melted <- asdr_melted %>%
  rename(IMD=`Deprivation Rank`)


asdr_melted$IMD <-as.factor(asdr_melted$IMD)
asdr_melted$Gender <-factor(asdr_melted$Gender, levels=c("Men","Women","Total"))
asdr_bar75<-ggplot(asdr_melted, aes(x = IMD, y =ASDR, fill = IMD)) +
  geom_bar(stat = "identity") +
  scale_fill_bhf(palette = 'red and light blue') +
  bhf_style() +
  theme(legend.position = "none") +
  scale_x_discrete(labels = function(IMD) str_wrap(IMD, width = 20)) +
  labs(title = "Under-75 Age-standardised death rates per 100,000 for CVD, by gender in Northern Ireland in 2019-21",
       x = "Deprivation Rank (1 = most deprived, 11 = least deprived)", y = "ASDR", caption="Data Source: BHF Compendium, Mortality rates calculated in partnership with Northern Ireland Statistics and Research Agency")+
  facet_wrap(~Gender)+
  theme(plot.caption.position = "plot")+
  theme(strip.text =element_text(size=15))+
  theme(axis.text = element_text(size = 14),
        title = element_text(size = 17),
        axis.title.x = element_text(size = 20),
        axis.title.y= element_text(size = 20),
        plot.title= element_text(size = 20),
        legend.text=element_text(size=15))

asdr_bar75

#figure 17 life expectancy
le_comp <- read_excel("~/Projects/le_comp.xlsx")
life_melted <-melt(le_comp, id.vars=c("Population","Gender"), measure.vars=c("At Birth","Healthy","Disability-Free"))
life_melted <- life_melted %>%
  rename(Life_Expectancy=`variable`)
life_plot <-ggplot(life_melted)+
  geom_bar(aes(x = Population, y = value, fill = Life_Expectancy), position = "dodge", stat = "identity") +
  labs(title = "Life Expectancy of those in Northern Ireland living in the most and least deprived quintiles from 2019-21",
       x = "Population Group",
       y = "Life Expectancy (years)") +
  theme_bw() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  scale_fill_bhf(palette = 'not reds') +
  bhf_style() +
  facet_wrap(~Gender)+
  theme(strip.text =element_text(size=15))+
  theme(plot.caption.position = "plot")+
  theme(axis.text = element_text(size = 14),
        title = element_text(size = 17),
        axis.title.x = element_text(size = 20),
        axis.title.y= element_text(size = 20),
        plot.title= element_text(size = 20),
        legend.text=element_text(size=15))
life_plot

#geom_bar(stat = "identity", position = position_dodge(width = 1)) +
#scale_fill_bhf(palette = 'red and light blue') +
#geom_text(aes(label = sprintf("%.1f", Percentage)), position = position_dodge(width = 1), hjust = 0.5, vjust=-0.4) +

##
life_melted$Gender<-as.factor(life_melted$Gender)
life_plot2 <-ggplot(life_melted, aes(x = Gender, y = value, fill = Population))+
  geom_bar(stat = "identity", position = position_dodge(width = 1)) +
  labs(title = "Life Expectancy of those in Northern Ireland living in the most and least deprived quintiles, by gender in 2020-22",
       x = "Population Group",
       y = "Life Expectancy (years)",
       caption="Data Source: Life expectancy Northern Ireland") +
  theme_bw() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  scale_fill_bhf(palette = 'red and light blue') +
  geom_text(aes(label = value), position = position_dodge(width = 1), hjust = 0.5, vjust=-0.4) +
  bhf_style() +
  facet_wrap(~Life_Expectancy) +
  theme(strip.text =element_text(size=15))+
  theme(plot.caption.position = "plot")+
  theme(axis.text = element_text(size = 14),
        title = element_text(size = 17),
        axis.title.x = element_text(size = 20),
        axis.title.y= element_text(size = 20),
        plot.title= element_text(size = 20),
        legend.text=element_text(size=15))
life_plot2
#figure 18 life expectancy
##life_change
life_change <- read_excel("~/Projects/life_change.xlsx")

life_change$Measure <- gsub("At_birth", "At Birth", life_change$Measure)
life_change$Measure <- gsub("At_65", "At 65", life_change$Measure)
life_change$Measure <- gsub("Disability_Free", "Disability-Free", life_change$Measure)
life_change$Gender<-as.factor(life_change$Gender)
life_change_plot <-ggplot(life_change, aes(x = Year, y = Inequality_Gap, fill = Gender),)+
  geom_bar(position = "dodge", stat = "identity") +
  labs(title = "Life expectancy gap between the most and least deprived quintiles in N. Ireland, by gender from 2015-22",
       x = "Gender",
       y = "Life Expectancy (years)",
       caption=" Data Source:Life expectancy Northern Ireland") +
  scale_fill_bhf(palette = 'red and light blue') +
  bhf_style() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  facet_wrap(~Measure)+
  theme(strip.text =element_text(size=15))+
  theme(plot.caption.position = "plot")+
  theme(axis.text = element_text(size = 14),
        title = element_text(size = 17),
        axis.title.x = element_text(size = 20),
        axis.title.y= element_text(size = 20),
        plot.title= element_text(size = 20),
        legend.text=element_text(size=15))
life_change_plot

#figure 19 appendix A
alcohol_NI <- read_excel("~/Project 1 Northern Ireland HE/alcohol_NI.xlsx")
alcohol_sub <- alcohol_NI %>%
  pivot_longer(!Category, names_to = "Deprivation_Quintile", values_to = "Percentage")
alcohol_sub$Deprivation_Quintile <- gsub("Most deprived", "1 (most deprived)", alcohol_sub$Deprivation_Quintile) 
alcohol_sub$Deprivation_Quintile <- gsub("Quintile 2", "2", alcohol_sub$Deprivation_Quintile) 
alcohol_sub$Deprivation_Quintile <- gsub("Quintile 3", "3", alcohol_sub$Deprivation_Quintile)
alcohol_sub$Deprivation_Quintile <- gsub("Quintile 4", "4", alcohol_sub$Deprivation_Quintile)
alcohol_sub$Deprivation_Quintile <- gsub("Least deprived", "5 (least deprived)", alcohol_sub$Deprivation_Quintile)
alcohol_plot2 <- ggplot(alcohol_sub, aes(x=Deprivation_Quintile, y=Percentage, fill=Deprivation_Quintile)) +
  geom_bar(stat = 'identity') +
  labs(title = 'Alcohol consumption in N. Ireland in 2019/20 by deprivation',
       x = 'Deprivation Quintile',
       y = 'Proportion',
       fill='Quintile',
       caption='Data Source Health Survey Northern Ireland') +
  scale_fill_manual(values = c("#FF0030", "#474E5A", "#474E5A","#474E5A", "#2D91FF"))+
  bhf_style() +
  theme(legend.position = "none") +
  scale_x_discrete(labels = function(Deprivation_Quintile) str_wrap(Deprivation_Quintile, width = 20)) +
  geom_text(aes(label = scales::percent(Percentage, accuracy = 0.1)), vjust = -0.3) +
  facet_wrap(~Category)+
  theme(plot.caption.position = "plot")+
  theme(strip.text =element_text(size=15))+
  theme(axis.text = element_text(size = 14),
        title = element_text(size = 17),
        axis.title.x = element_text(size = 20),
        axis.title.y= element_text(size = 20),
        plot.title= element_text(size = 20),
        legend.text=element_text(size=15))
alcohol_plot2

#figure 20 appendix b
##change in gps
numgp2 <- read_excel("~/Projects/numgp2.xlsx")
numgp2$`Deprivation Rank`<-as.factor(numgp2$`Deprivation Rank`)
numgp2 <- melt(numgp2, id.vars = "Deprivation Rank", measure.vars = c("Number of GP Practices 2014", "Number of GP Practices 2023"))
numgp2$variable <- gsub("Number of GP Practices 2014", "2014",numgp2$variable)
numgp2$variable <- gsub("Number of GP Practices 2023", "2023",numgp2$variable)
numgp2_plot<-ggplot(numgp2, aes(x=`Deprivation Rank`, y=value, fill=variable))+
  geom_bar(position = "dodge", stat = "identity")+
  labs(title = "Number of GPs in 2014 and 2023 by LGD of decreasing deprivation",
       x = "Deprivation Rank (1 = most deprived, 11 = least deprived)",
       y = 'Number of GPs per 100,000',
       fill='Deprivation Rank',
       caption="Data Source: General Medical Services Statistics")+
  theme(plot.title = element_text(hjust = .5)) +
  scale_fill_bhf(palette = 'not reds') +
  bhf_style() +
  theme(plot.caption.position = "plot")+
  theme(strip.text =element_text(size=15))+
  theme(axis.text = element_text(size = 14),
        title = element_text(size = 17),
        axis.title.x = element_text(size = 20),
        axis.title.y= element_text(size = 20),
        plot.title= element_text(size = 20),
        legend.text=element_text(size=15))
  
  numgp2_plot

#figure 21
  asdr_ni <- read_excel("asdr_ni.xlsx")
  asdr_melted <- melt(asdr_ni, id.vars = c("LGD", "Deprivation rank"), measure.vars = c("asdr_men", "asdr_women", "asdr_total"))
  asdr_melted$variable <- gsub("asdr_men", "Men", asdr_melted$variable)
  asdr_melted$variable <- gsub("asdr_women", "Women", asdr_melted$variable)
  asdr_melted$variable <- gsub("asdr_total", "Total", asdr_melted$variable)
  asdr_melted <- asdr_melted %>%
    rename(ASDR=value)
  asdr_melted <- asdr_melted %>%
    rename(Gender=variable)
  asdr_melted <- asdr_melted %>%
    rename(IMD=`Deprivation rank`)
  
  
  asdr_melted$IMD <-as.factor(asdr_melted$IMD)
  asdr_melted$Gender <-factor(asdr_melted$Gender, levels=c("Men","Women","Total"))
  asdr_bar<-ggplot(asdr_melted, aes(x = IMD, y =ASDR, fill = IMD)) +
    geom_bar(stat = "identity") +
    scale_fill_bhf(palette = 'red and light blue') +
    bhf_style() +
    theme(legend.position = "none") +
    scale_x_discrete(labels = function(IMD) str_wrap(IMD, width = 20)) +
    labs(title = "Age-standardised death rates per 100,000 for CVD, by gender in Northern Ireland in 2019-21",
         x = "Deprivation Rank (1 = most deprived, 11 = least deprived)", y = "ASDR", caption="Data Source: BHF Compendium, Mortality rates calculated in partnership with Northern Ireland Statistics and Research Agency")+
    facet_wrap(~Gender)+
    theme(plot.caption.position = "plot")+
    theme(strip.text =element_text(size=15))+
    theme(axis.text = element_text(size = 14),
          title = element_text(size = 17),
          axis.title.x = element_text(size = 20),
          axis.title.y= element_text(size = 20),
          plot.title= element_text(size = 20),
          legend.text=element_text(size=15))
  
  asdr_bar
  
  ## figure 22 hosp_admin (elective vs emergency)
  hosp_admin <- read_excel("~/Projects/hosp_admin.xlsx")
  hosp_melted <-melt(hosp_admin, id.vars=c("Population","Type"), measure.vars=c("2017/18","2018/19","2019/20","2020/21","2021/22"))
  hosp_melted <- hosp_melted %>%
    rename(Years=variable)
  hosp_melted<-hosp_melted %>% 
    rename(Admissions=value)
  hosp_plot <- ggplot(hosp_melted, aes(x=Years,y=Admissions,fill=Population))+
    geom_bar(position = "dodge", stat = "identity") +
    labs(title = 'Comparison of the emergency and elective hospital admission rate in N.Ireland from 2017-22',
         x = 'Years',
         y = 'Admissions per 100,000 population',
         fill='Deprivation Rank',
         caption= "Data Source: Health Inequalities Northern Ireland 2023")+
    scale_fill_bhf(palette = 'red and light blue') +
    bhf_style() +
    theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
    scale_y_continuous(label=comma)+
    facet_wrap(~Type)+
    theme(plot.caption.position = "plot")+
    theme(strip.text =element_text(size=15))+
    theme(axis.text = element_text(size = 14),
          title = element_text(size = 17),
          axis.title.x = element_text(size = 20),
          axis.title.y= element_text(size = 20),
          plot.title= element_text(size = 20),
          legend.text=element_text(size=15))
  

  hosp_plot