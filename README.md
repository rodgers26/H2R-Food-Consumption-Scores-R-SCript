# H2R-Food-Consumption-Scores-R-SCript
This script show how to go about computation of Food consumption scores  for IDP humanitarian situation monitoring.For actual calculation refer to the lines  commented from ### Food consuption scores.
rm(list = ls())

options(scipen = 999)

if (!require("pacman")) install.packages("pacman")

pacman::p_load(tidyverse,
               hypegrammaR,
               rio,
               readxl,
               openxlsx)
setwd("C:\\Users\\rodhiambo\\Desktop\\H2R_08_2023\\Analysis/")

# source("src/functions/utils.R")
source("C:\\Users\\rodhiambo\\Desktop\\H2R_08_2023\\Analysis\\functions/results_table_functions_v2.R")

data<- read_excel("C:/Users/rodhiambo/Desktop/H2R_08_2023/Analysis/inputs/aggregation.xlsx")
# d<-data[grepl("hh_level|district",data)]
# data2<-read.csv("C:/Users/rodhiambo/Desktop/H2R_08_2023/Analysis/inputs/ HSM_Data 2023-10-09 .xlsx")
#data2<-data2[names(data2)%in%names(data)]
 #data<-data2
data <- data %>% filter(!is.na(district)) %>% select(-termintation_reason)
#removing spaces from data
as.data.frame(
  apply(data,2, function(x) gsub("\\s+", "", x)))->data
#data[data=="NC"]
# data$instance_name<-substring(data$instance_name,1,36)
# data<-merge(data,data2,all.x = T,by="instance_name") %>% select(ends_with(".y"))
# names(data)<-gsub(".y","",names(data))
#Food consumption scores
# vars<-c("days_no_resources",
#         "days_sleep_hungry",
#         "times_whole_day_noeat",
#         "cereals_tubers",
#         "lentils_beans",
#         "vegetables",
#         "fruits",
#         "sugar_honey",
#         "oils_fats",
#         "milk_yorghut",
#        "beef_eggs_fish"
# )
# data[vars]<-as.data.frame(lapply(data[vars], as.numeric))
names(data)<-gsub("/",".",names(data))
####Food consumption scores
# data$fcs_total<-as.numeric(data$cereals_tubers)*2+
#   as.numeric(data$fruits)*1+
#   as.numeric(data$vegetables)*1+
#   as.numeric(data$beef_eggs_fish)*4+
#   as.numeric(data$milk_yorghut)*4+
#   as.numeric(data$oils_fats)*0.5+
#   as.numeric(data$lentils_beans)*3+
#   as.numeric(data$sugar_honey)*0.5
# data<-data %>% relocate(fcs_total,.after = beef_eggs_fish)
# 
# data$fcs_score[data$fcs_total<=28]<-"poor"
# data$fcs_score[data$fcs_total>28&data$fcs_total<=42]<-"borderline"
# data$fcs_score[data$fcs_total>42]<-"Acceptable"
# #fcs log for fcs total equasls 0
# # fcs_log<-data[data$fcs_total<1,] %>% select(uuid,cereals_tubers,lentils_beans,	vegetables,	fruits,	sugar_honey,	oils_fats	,milk_yorghut,	beef_eggs_fish)
# # fcs_log<-fcs_log %>% pivot_longer(!uuid,names_to = "question.name",values_to = "old.value") %>% mutate(feedback="removing fcs equals 0",new.value="")
# #writexl::write_xlsx(fcs_log,"C:\\Users\\rodhiambo\\Desktop\\H2R_08_2023\\cleaning\\received_logs/fcs_log.xlsx")
# # 
# # #coding for going hungry
# data$days_no_resources<-as.integer(data$days_no_resources)
# data$hhs_1_score[data$days_no_resources>=1&data$days_no_resources<=2]<-1
# data$hhs_1_score[data$days_no_resources>=3&data$days_no_resources<=10]<-1
# data$hhs_1_score[data$days_no_resources>10]<-2
# data$hhs_1[data$days_no_resources=="no"]<- 0 #RM added
# data$hhs_1_score[data$no_resources==0]<-0
# #hhs_2
# data$times_whole_day_noeat<-as.integer(data$days_sleep_hungry)
# data$hhs_2_score[data$days_sleep_hungry>=1&data$days_sleep_hungry<=2]<-1
# data$hhs_2_score[data$days_sleep_hungry>=3&data$days_sleep_hungry<=10]<-1
# data$hhs_2_score[data$days_sleep_hungry>10]<-2
# data$hhs_2_score[data$sleep_hungry=="no"] <- 0 #RM added
# data$hhs_2_score[data$days_sleep_hungry==0]<-0
# 
# #hhs_3
# data$times_whole_day_noeat<-as.integer(data$times_whole_day_noeat)
# data$hhs_3_score[data$times_whole_day_noeat>=1&data$times_whole_day_noeat<=2]<-1
# data$hhs_3_score[data$times_whole_day_noeat>=3&data$times_whole_day_noeat<=10]<-1
# data$hhs_3_score[data$times_whole_day_noeat>10]<-2
# data$hhs_3_score[data$whole_day_no_eat=="no"] <- 0 #RM added
# data$hhs_3_score[is.na(data$times_whole_day_noeat)==0]<-0
# 
# 
# var<-c("hhs_1_score","hhs_2_score","hhs_3_score")
# data[var]<-lapply(data[var],as.numeric)
# class(data$hhs_1_score)
# data$hhs_score<-data$hhs_1_score+data$hhs_2_score+data$hhs_3_score
# data$hhs_total_score<-data$hhs_1_score+data$hhs_2_score+data$hhs_3_score
# 
# data$hhs_score<-
#   ifelse(data$hhs_total_score==0,"none",
#          ifelse(data$hhs_total_score==1,"slight",
#                 ifelse(data$hhs_total_score>=2&data$hhs_total_score<=3,"moderate",
#                        ifelse(data$hhs_total_score==4, "severe",
#                               ifelse(data$hhs_total_score>=5&data$hhs_total_score<=6,"very severe",
#                                      "no_score")))))
# 
#  
# #HWISE calculation
# data$worry_water_score<-ifelse(data$worry_water=="always"|data$worry_water=="often",3,ifelse(data$worry_water=="sometimes",2,ifelse(data$worry_water=="rarely",1,ifelse(data$worry_water=="never",0,NA))))
# data$change_plans_score<-ifelse(data$change_plans=="always"|data$change_plans=="often",3,ifelse(data$change_plans=="sometimes",2,ifelse(data$change_plans=="rarely",1,ifelse(data$change_plans=="never",0,NA))))
# data$without_washing_hands_score<-ifelse(data$without_washing_hands=="always"|data$without_washing_hands=="often",3,ifelse(data$without_washing_hands=="sometimes",2,ifelse(data$without_washing_hands=="rarely",1,ifelse(data$without_washing_hands=="never",0,NA))))
# data$no_drink_score<-ifelse(data$no_drink=="always"|data$no_drink=="often",3,ifelse(data$no_drink=="sometimes",2,ifelse(data$no_drink=="rarely",1,ifelse(data$no_drink=="never",0,NA))))
# data$wise_score<-data$worry_water_score+data$change_plans_score+data$without_washing_hands_score+data$no_drink_score
# data$hwise_score<-ifelse(data$wise_score>=4,"insecure","secure")
### export dataset with final column names

koboToolPath = 'inputs/aggregation - Copy.xlsx'

questions <- read_xlsx(koboToolPath,
                       guess_max = 50000,
                       na = c("NA","#N/A",""," "),
                       sheet = 1) %>% filter(!is.na(name)) %>% 
  mutate(q.type=as.character(lapply(type, function(x) str_split(x, " ")[[1]][1])),
         list_name=as.character(lapply(type, function(x) str_split(x, " ")[[1]][2])),
         list_name=ifelse(str_starts(type, "select_"), list_name, NA),
         type=gsub("\\s+or_other","",type))

choices <- read_xlsx(koboToolPath,
                       guess_max = 50000,
                       na = c("NA","#N/A",""," "),
                       sheet = 2)

#questions<-questions[!(questions$name%in%names(data)),]
questions$name<-gsub("\\+","",questions$name)
names(data)<-gsub("\\.$","",names(data))
data<-data[data$region!="NC",]
res <- generate_results_table(data = data,
                              questions = questions,
                              choices = choices,
                              weights.column = NULL,
                              use_labels = T,
                              labels_column = "label::English",
                              "region"
                              

)
export_table(res,"outputs/")
# export_table(res_received,"outputs/received_")
# export_table(res_not_received,"outputs/not_received_")
#write.xlsx(variables_medians, "outputs/medians_varibles.xlsx")
write.xlsx(data,"outputs/Clean.xlsx")
