rm(list = ls())
### function for convert PDF to siple text function
PDF_CORPUSM<-function(path,file_name,page,year,uid){
  library("pdftools")
  library(quanteda)
  library(quanteda.textstats)
  require(readtext)
  library(tm)
  library("corpus") 
  library(utf8)
  library(stringr)
  trim <- function (x) gsub("^\\s+|\\s+$", "", x)
  final<-list()
  find_mode <- function(x) {
    u <- unique(x)
    tab <- tabulate(match(x, u))
    u[tab == max(tab)]
  }
  Clean_String <- function(string){
    # Lowercase
    # temp <- tolower(string)
    temp <- string
    # Remove everything that is not a number or letter (may want to keep more 
    # stuff in your actual analyses). 
    temp <- stringr::str_replace_all(temp,"[^a-zA-Z0-9.,:;!?\"\'\\()$%#&@+=\\s]", " ")
    # Shrink down to just one white space
    temp <- stringr::str_replace_all(temp,"[\\s]+", " ")
    # Split it
    temp <- stringr::str_split(temp, " ")[[1]]
    # Get rid of trailing "" if necessary
    indexes <- which(temp == "")
    if(length(indexes) > 0){
      temp <- temp[-indexes]
    } 
    return(temp)
  }
  read_text <- function(text) {
    trim <- function (x) gsub("^\\s+|\\s+ $", "", x)
    result <- ''
    QTD_COLUMNS1<-c()
    lstops1_min<-100
    for(j in 1:length(text)){
      lstops1 <- gregexpr(pattern ="\\s{2,}",text[j])
      yj<-attr(lstops1[[1]],"match.length")
      if (is.na(yj)==FALSE & yj>1)lstops1_min<-min(lstops1_min,yj)
    }
    for(m in 1:length(text)){
      tm<-unlist(strsplit(text[m], paste0("\\s{",lstops1_min,",}")))
      # tm<-tm[tm != ""]
      lm<-length(tm)
      if (is.na(lm)==FALSE) QTD_COLUMNS1<-c(QTD_COLUMNS1, lm)
    }
    QTD_COLUMNS<-find_mode(QTD_COLUMNS1)
    if(QTD_COLUMNS>3){
      temp_result <- trim(text)
      result<-append(result, temp_result)
    }
    if(QTD_COLUMNS<2){
      temp_result <- trim(text)
      result<-append(result, temp_result)
    }
    if(QTD_COLUMNS>1 & QTD_COLUMNS<4){
      temp_result <- trim(text)
      result<-append(result, temp_result)
    }
    # if(QTD_COLUMNS>1 & QTD_COLUMNS<4){
    #   for (n in 1:QTD_COLUMNS) {
    #     for(m in 1:length(text)){
    #       tm<-unlist(strsplit(text[m], paste0("\\s{",lstops1_min,",}")))
    #       # tm<-tm[tm != ""]
    #       lm<-length(tm)
    #       result1<-tm[n]
    #       result<-append(result,result1)
    #     }
    #   }
    # }
    na.omit(result)
  }
  for (i in 1:length(file_name)) {
    # file_test<-gsub(".pdf|.PDF","",file_name[i])
    file_test<-uid[i]
    tes<-pdf_text(paste0(path,file_name[i]))
    test<-as_utf8(tes)
    # assign(file_test,tes)
    t<-page[i]
    
    tes1<-tes[(as.numeric(t)+1):length(tes)]
    test2<-''
    for (k in 1:length(tes1)) {
      # print(k)
      page1 <- tes1[k]
      t1 <- unlist(strsplit(page1, "\n"))
      t1<-t1[t1 != ""]
      if(length(t1)==0){
        test2 = append(test2,t1)
        # print(test2)
      }
      if(length(t1)>0){
        # t1<-str_replace(t1, ". ","")
        maxSize <- max(nchar(t1))
        t1 <- paste0(t1,strrep(" ", maxSize-nchar(t1)))
        test2 = append(test2,read_text(t1))
      }
      
    }
    tes2<-toString(test2)
    tes2<-toString(Clean_String(tes2))
    tes2<-gsub(",","",tes2)
    tes2<-paste(Filter(function(x) nchar(x) > 1,
                       unlist(strsplit(tes2, " "))), 
                collapse=" ")
    final[file_test]<-list(c(tes2))
    # return (final)
  }
  return (final)
}
# Function for convert text data into data frame
PDF_DATAFRAME<-function(x,columns){
  library(dplyr)
  x<-x %>%
    filter(is.na(x[,columns])==FALSE & pdf==TRUE) %>%
    arrange(year)
  path<-x$path
  # path<-unique(path)
  year<-unique(x$year)
  # print(path)
  # print(year)
  y<-year[1]
  pa<-path[1]
  # print(pa)
  x_y_char<-x %>%
    filter(year==y & is.na(x[,columns])==FALSE & pdf==TRUE)
  # print(x_y_char$file_name)
  f<-x_y_char$file_name
  y<-x_y_char$year
  p<-x_y_char[,columns]
  uid<-x_y_char$uid
  final_char<-PDF_CORPUSM(pa,f,p,y,uid)
  df <- do.call(rbind, final_char)
  data_frame <- as.data.frame(df)
  # data_frame<-rename(data_frame,c("txt"="V1"))
  data_frame$uid<-row.names(data_frame)
  # final<-data.frame(t(data.frame(final_char)))
  # final$txt<-final
  # final$uid<-row.names(final)
  for (i in 2:length(path)) {
    y_i<-year[i]
    pa_i<-path[i]
    x_y_char_i<-x %>%
      filter(year==y_i & is.na(x[,columns])==FALSE & pdf==TRUE)
    fi<-x_y_char_i$file_name
    yi<-x_y_char_i$year
    pi<-x_y_char_i[,columns]
    uidi<-x_y_char_i$uid
    final_char_i<-PDF_CORPUSM(pa_i,fi,pi,y_i,uidi)
    df_i <- do.call(rbind, final_char_i)
    data_frame_i <- as.data.frame(df_i)
    # data_frame_i<-rename(data_frame_i,c("txt"="V1"))
    data_frame_i$uid<-row.names(data_frame_i)
    data_frame<-rbind(data_frame,data_frame_i)
  }
  return (data_frame)
}
#Make a list Credit Union
library(readxl)
library(tidyr)
library(dplyr)
library("xlsx")
library(readxl)

setwd("")
x<-""
dic<-list.files("pdf", pattern = NULL)
file_list<-dic
file_list_data<-data.frame(file_list)
year<-0
file_list_data$year<-year
file_list_data$path<-paste0(x,"/","pdf","/")
file_list_data$pdf<-TRUE
file_list_data$Name<-file_list_data$file_list
file_list_data$Name <- gsub('.pdf', '', file_list_data$Name)
file_list_data$Name <- gsub('_', '', file_list_data$Name)
file_list_data$uid<-paste0(file_list_data$Name,"-",file_list_data$year)

library(readxl)
data_page<- read_excel("Legislation Page Selection_with Path.xlsx")
data_merge <- left_join(file_list_data,data_page,by ="Name")
data_merge$file_name<-data_merge$file_list

final_t_chair<-PDF_DATAFRAME(data_merge,columns=c("report"))
final_t_CEO<-PDF_DATAFRAME(credit_union_data,columns=c("CEO"))
final_t_Chair_CEO<-PDF_DATAFRAME(credit_union_data,columns=c("Char_CEO"))
final_t_chair<-rename(final_t_chair,c("txt_chair"="txt"))
final_t_CEO<-rename(final_t_CEO,c("txt_CEO"="txt"))
final_t_Chair_CEO<-rename(final_t_Chair_CEO,c("txt_Chair_CEO"="txt"))
credit_union_data_final<-left_join(credit_union_data,final_t_chair,by="uid")
credit_union_data_final<-left_join(credit_union_data_final,final_t_CEO,by="uid")
credit_union_data_final<-left_join(credit_union_data_final,final_t_Chair_CEO,by="uid")
credit_union_data_final<-credit_union_data_final %>%
  filter(is.na(Chair)==FALSE|is.na(CEO)==FALSE|is.na(Char_CEO)==FALSE & Availability_of_report=="Yes")
credit_union_data_final<-credit_union_data_final %>%
  select(uid,file_name,folder_name,Name,ID,year,
         Availability_of_report,Chair,txt_chair,CEO,txt_CEO,Char_CEO,
         txt_Chair_CEO,Remark,pdf,path)
library(readxl)
library(tidyr)
library(dplyr)
library("xlsx")
library(readxl)
setwd("")
setwd("")
rank<-read_excel("Data Frame.xlsx",sheet = "Rank_CU")
# rank$Name_1<-rank$Name
# Name<-rank$Name
# Name<-unique(Name)
# Name<-data.frame(Name)
# Name$CU_Name<NA
# rank$Name_1<-gsub("\\s+","", rank$Name_1)
# rank$CU_Name<-NA
library(readxl)
# name_CU <- read_excel("Data Frame.xlsx",sheet = "Name_CU")
Name<-rank$Name
Name<-unique(Name)
Name<-data.frame(Name)
Name$CU_Name<-NA
for (i in 1:nrow(Name)) {
  if (is.na(Name$CU_Name[i])==TRUE) {
    Name$CU_Name[i]<-ifelse(grepl("1st CHOICE|1ST Choice|1st Choice",Name$Name[i])==TRUE,"FirstChoice",
    ifelse(grepl("Access",Name$Name[i])==TRUE,"Access",
    ifelse(grepl("Advantage",Name$Name[i])==TRUE,"Advantage",
    ifelse(grepl("Alterna",Name$Name[i])==TRUE,"Alterna",
    ifelse(grepl("Alterna",Name$Name[i])==TRUE,"Alterna",
    ifelse(grepl("Assiniboine",Name$Name[i])==TRUE,"Assiniboine",
    ifelse(grepl("Auto Workers",Name$Name[i])==TRUE,"Autoworkers",
    ifelse(grepl("BATTLE RIVER|Battleriver",Name$Name[i])==TRUE,"Battle",
    ifelse(grepl("Bayview",Name$Name[i])==TRUE,"Bayview",
    ifelse(grepl("BlueShore",Name$Name[i])==TRUE,"Blueshore",
    ifelse(grepl("BOW|Bow Valley",Name$Name[i])==TRUE,"Bowvalley",
    ifelse(grepl("Buduchnist",Name$Name[i])==TRUE,"Buduchnist",
    ifelse(grepl("Bulkley",Name$Name[i])==TRUE,"Bulkley",
    ifelse(grepl("Caisse Financial",Name$Name[i])==TRUE,"CaisseFinancial",
    ifelse(grepl("Caisse Populaire Alliance",Name$Name[i])==TRUE,"CaisseAlliance",
    ifelse(grepl("Cambrian",Name$Name[i])==TRUE,"Cambrian",
    ifelse(grepl("Carpathia",Name$Name[i])==TRUE,"Carpathia",
    ifelse(grepl("Casera",Name$Name[i])==TRUE,"Casera",
    ifelse(grepl("CHINOOK|Chinook",Name$Name[i])==TRUE,"Chinook",
    ifelse(grepl("Coast Capital",Name$Name[i])==TRUE,"CoastCapital",
    ifelse(grepl("Coastal Community",Name$Name[i])==TRUE,"Coastal",
    ifelse(grepl("Community First",Name$Name[i])==TRUE,"Comm-First",
    ifelse(grepl("Comtech",Name$Name[i])==TRUE,"Comtech",
    ifelse(grepl("Connect First",Name$Name[i])==TRUE,"ConnectFirst",
    ifelse(grepl("Cornerstone Credit",Name$Name[i])==TRUE,"Cornerstone",
    ifelse(grepl("Prairie",Name$Name[i])==TRUE,"Prairie",
    ifelse(grepl("Atlantic",Name$Name[i])==TRUE,"Atlantic",
    ifelse(grepl("Crossroads",Name$Name[i])==TRUE,"Crossroads",
    ifelse(grepl("Desjardins",Name$Name[i])==TRUE,"Desjardins",
    ifelse(grepl("Diamond North",Name$Name[i])==TRUE,"Diamond",
    ifelse(grepl("DUCA",Name$Name[i])==TRUE,"DUCA",
    ifelse(grepl("East Coast",Name$Name[i])==TRUE,"EastCoast",
    ifelse(grepl("Encompass Credit Union",Name$Name[i])==TRUE,"Encompass",
    ifelse(grepl("Envision",Name$Name[i])==TRUE,"Envision",
    ifelse(grepl("FIRST CALGARY|First Calgary",Name$Name[i])==TRUE,"FirstCalgary",
    ifelse(grepl("First Ontario|FirstOntario",Name$Name[i])==TRUE,"FirstOntario",
    ifelse(grepl("First West Credit",Name$Name[i])==TRUE,"FirstWest",
    ifelse(grepl("Fusion Credit Union",Name$Name[i])==TRUE,"Fusion",
    ifelse(grepl("G&F|G & F|Gulf and Fraser",Name$Name[i])==TRUE,"GFFG",
    ifelse(grepl("Italian Canadian Savings | IC Savings",Name$Name[i])==TRUE,"ICSavings",
    ifelse(grepl("Innovation",Name$Name[i])==TRUE,"Innovation",
    ifelse(grepl("Integris",Name$Name[i])==TRUE,"Integris",
    ifelse(grepl("Interior Savings Credit",Name$Name[i])==TRUE,"Interior",
                                                                                                                                                                                                                                                                                                                                  NA)))))))))))))))))))))))))))))))))))))))))))
  }
}

for (i in 1:nrow(Name)) {
  if (is.na(Name$CU_Name[i])==TRUE) {
    Name$CU_Name[i]<-ifelse(grepl("Kawartha",Name$Name[i])==TRUE,"Kawartha",
    ifelse(grepl("Khalsa",Name$Name[i])==TRUE,"Khalsa",
    ifelse(grepl("Kindred",Name$Name[i])==TRUE,"Kindred",
    ifelse(grepl("Kootenay",Name$Name[i])==TRUE,"Kootenay",
    ifelse(grepl("Lake View|Lakeview ",Name$Name[i])==TRUE,"Lakeview",
    ifelse(grepl("LAKELAND|Lakeland",Name$Name[i])==TRUE,"Lakeland",
    ifelse(grepl("Libro|St Willibrord",Name$Name[i])==TRUE,"Libro",
    ifelse(grepl("Mainstreet Credit Union",Name$Name[i])==TRUE,"Mainstreet",
    ifelse(grepl("Mennonite",Name$Name[i])==TRUE,"Mennonite",
    ifelse(grepl("Meridian",Name$Name[i])==TRUE,"Meridian",
    ifelse(grepl("MOUNTAIN VIEW|Mountain View",Name$Name[i])==TRUE,"MountainView",
    ifelse(grepl("Moya Financial Credit",Name$Name[i])==TRUE,"Moya",
    ifelse(grepl("Newfoundland & Labrador|Newfoundland and Labrador|NEWFOUNDLAND/LABRADOR",Name$Name[i])==TRUE,"NLCU",
    ifelse(grepl("Niverville",Name$Name[i])==TRUE,"Niverville",
    ifelse(grepl("Island Savings",Name$Name[i])==TRUE,"Island",
    ifelse(grepl("Affinity",Name$Name[i])==TRUE,"Affinity",
    ifelse(grepl("Aldergrove",Name$Name[i])==TRUE,"Aldergrove",
    ifelse(grepl("Conexus",Name$Name[i])==TRUE,"Conexus",
    ifelse(grepl("Catalyst",Name$Name[i])==TRUE,"Catalyst",
    ifelse(grepl("Community Savings",Name$Name[i])==TRUE,"Comm-Savings",
    ifelse(grepl("Crosstown",Name$Name[i])==TRUE,"Crosstown",
    ifelse(grepl("Entegra",Name$Name[i])==TRUE,"Entegra",
    ifelse(grepl("VantageOne",Name$Name[i])==TRUE,"VantageOne",
    ifelse(grepl("Community Credit Union",Name$Name[i])==TRUE,"Community",
    ifelse(grepl("North Shore Credit Union",Name$Name[i])==TRUE,"Northshore",
                                                                                                                                                                                                    NA)))))))))))))))))))))))))
  }
}
for (i in 1:nrow(Name)) {
  if (is.na(Name$CU_Name[i])==TRUE) {
    Name$CU_Name[i]<-ifelse(grepl("Northern Credit Union",Name$Name[i])==TRUE,"Northern",
    ifelse(grepl("Northern Savings",Name$Name[i])==TRUE,"Northernsavings",
    ifelse(grepl("Noventis Credit Union",Name$Name[i])==TRUE,"Noventis",
    ifelse(grepl("Pace|PACE Savings|PACE Savings",Name$Name[i])==TRUE,"Pace",
    ifelse(grepl("Parama",Name$Name[i])==TRUE,"Parama",
    ifelse(grepl("Pathwise Credit Union",Name$Name[i])==TRUE,"Pathwise",
    ifelse(grepl("PenFinancial",Name$Name[i])==TRUE,"Penfinancial",
    ifelse(grepl("Police",Name$Name[i])==TRUE,"ThePolice",
    ifelse(grepl("Prairie Centre",Name$Name[i])==TRUE,"Prairie",
    ifelse(grepl("Prospera",Name$Name[i])==TRUE,"Prospera",
    ifelse(grepl("Provincial Credit Union",Name$Name[i])==TRUE,"Provincial",
    ifelse(grepl("Radius Credit Union",Name$Name[i])==TRUE,"Radius",
    ifelse(grepl("Rapport Credit Union",Name$Name[i])==TRUE,"Rapport",
    ifelse(grepl("ROCKY|Rocky Credit Union",Name$Name[i])==TRUE,"Rocky",
    ifelse(grepl("Salmon Arm Savings",Name$Name[i])==TRUE,"SASCU",
    ifelse(grepl("SERVUS|Servus",Name$Name[i])==TRUE,"Servus",
    ifelse(grepl("South Interlake",Name$Name[i])==TRUE,"S-Interlake",
    ifelse(grepl("Spectra",Name$Name[i])==TRUE,"Spectra",
    ifelse(grepl("St Stanislaus|St. Stanislaus",Name$Name[i])==TRUE,"Parishes",
    ifelse(grepl("Steinbach",Name$Name[i])==TRUE,"Steinbach",
    ifelse(grepl("Stride Credit Union",Name$Name[i])==TRUE,"Stride",
    ifelse(grepl("Sudbury",Name$Name[i])==TRUE,"Sudbury",
    ifelse(grepl("Summerland",Name$Name[i])==TRUE,"Summerland",
    ifelse(grepl("Sunova Credit Union",Name$Name[i])==TRUE,"Sunova",
    ifelse(grepl("Sunrise|SunRise",Name$Name[i])==TRUE,"Sunrise",
    ifelse(grepl("Sunshine Coast",Name$Name[i])==TRUE,"Sunshine",
    ifelse(grepl("Swan Valley",Name$Name[i])==TRUE,"SwanValley",
    ifelse(grepl("Synergy Credit Union",Name$Name[i])==TRUE,"Synergy",
    ifelse(grepl("Tandia|Tandem Financial",Name$Name[i])==TRUE,"Tandia",
    ifelse(grepl("TCU",Name$Name[i])==TRUE,"TCU",
    ifelse(grepl("Hamilton Teachers'|Hamilton Teachers",Name$Name[i])==TRUE,"Teachers",
    ifelse(grepl("Ukrainian",Name$Name[i])==TRUE,"Ukrainian",
    ifelse(grepl("UNI Financial",Name$Name[i])==TRUE,"UNIfinancial",
    ifelse(grepl("United Communities",Name$Name[i])==TRUE,"United",
    ifelse(grepl("Vancouver City Savings|Vancity",Name$Name[i])==TRUE,"Vancity",
    ifelse(grepl("Vanguard",Name$Name[i])==TRUE,"Vanguard",
    ifelse(grepl("Vision Credit Union",Name$Name[i])==TRUE,"Vision",
    ifelse(grepl("Westminster Savings",Name$Name[i])==TRUE,"Westminster",
    ifelse(grepl("Westoba",Name$Name[i])==TRUE,"Westoba",
    ifelse(grepl("Weyburn",Name$Name[i])==TRUE,"Weybury",
    ifelse(grepl("Windsor Family",Name$Name[i])==TRUE,"Windsor",
    ifelse(grepl("Your Neighbourhood",Name$Name[i])==TRUE,"YourNeighbourhood",
                                                                                                                                                                                                                                                                                                                         NA)))))))))))))))))))))))))))))))))))))))))) 
  }
}

rank<-left_join(rank,Name,by="Name")
rank$uid<-paste0(rank$CU_Name,".",rank$Year)
# for (i in 1:nrow(rank)) {
#   rank$CU_Name<-ifelse(grepl("CP La Prairie",rank$Name[i])==TRUE,NA,
# }
for (i in 1:nrow(rank)) {
  rank$uid[i]<-ifelse(grepl("CP La Prairie",rank$Name[i])==TRUE,NA,
              ifelse(grepl("East Kootenay Savings Credit Union",rank$Name[i])==TRUE,NA,
              ifelse(grepl("Motor City Community Credit Union",rank$Name[i])==TRUE,NA,
              ifelse(grepl("East Kootenay Community Credit Union",rank$Name[i])==TRUE,NA,
              ifelse(grepl("Greater Vancouver Community",rank$Name[i])==TRUE,NA,
                     rank$uid[i])))))
}
# n_occur <- data.frame(table(rank$uid))
# n_occur<-rename(n_occur, c("uid"="Var1"))
# 
# rank$uid<-paste0(rank$CU_Name,".",rank$Year)
# rank$uid[i]

credit_union_data_final<-left_join(credit_union_data_final,rank,by="uid")
credit_union_data_final<-credit_union_data_final %>%
  select(uid,file_name,folder_name,Name.x,ID,year,
         Availability_of_report,Chair,txt_chair,CEO,txt_CEO,Char_CEO,
         txt_Chair_CEO,Remark,pdf,path,Rank,Province,Assets,Membership,location)
credit_union_data_final<-rename(credit_union_data_final,c("Name"="Name.x"))
credit_union_data_check<-credit_union_data_final %>%
  filter(is.na(Rank)==TRUE)
credit_union_data_final$Entity<-"CU"
credit_union_data_final<-credit_union_data_final %>%
  filter(uid!="Cornerstone.2016")


setwd("")
setwd("")
credit_union_data_final_unprocessed<-credit_union_data_final
save(credit_union_data_final_unprocessed, file = "credit_union_data_final_unprocessed.RData")
#Make a list Bank
library(tidyr)
library(dplyr)
setwd("")
x<-""
setwd("")
x<-""
dic_list_bank<-list.files("Banks", pattern = NULL)
year_bank<-readr::parse_number(dic_list_bank[1])
dic_bank<-paste0("Banks/",dic_list_bank[1])
file_list_bank<-list.files(dic_bank)
file_data_bank<-data.frame(file_list_bank)
file_data_bank$year<-year_bank
file_data_bank$folder_name<-dic_list_bank[1]
file_data_bank$path<-paste0(x,dic_bank,"/")
for (i in 2:length(dic_list_bank)) {
  dic_bank<-paste0("Banks/",dic_list_bank[i])
  file_list_bank<-list.files(dic_bank)
  # print(file_list_bank)
  if (length(file_list_bank)!=0) {
    year_bank<-readr::parse_number(dic_list_bank[i])
    file_data_bank_1<-data.frame(file_list_bank)
    file_data_bank_1$year<-year_bank
    file_data_bank_1$folder_name<-dic_list_bank[i]
    file_data_bank_1$path<-paste0(x,dic_bank,"/")
    file_data_bank<-rbind(file_data_bank,file_data_bank_1)
  }
}
###Genrate Id based one credit union name
file_data_bank$file_list_1<-file_data_bank$file_list_bank
file_data_bank$file_list_bank<-gsub("\\s+","", file_data_bank$file_list_bank)
file_data_bank$Bank_Name<-NA
setwd("")
setwd("")
name_Bank <- read_excel("Data Frame.xlsx",sheet = "Name_Bank")
name<-name_Bank$S.name
for (i in 1:nrow(file_data_bank)) {
  for (j in 1:length(name)) {
    n<-name[j]
    pa<-paste0("^(.*?)",n)
    # print(n)
    # print(pa)
    if (is.na(file_data_bank$Bank_Name[i])==TRUE) {
      file_data_bank$Bank_Name[i]<-ifelse(grepl(pa,file_data_bank$file_list_bank[i])==TRUE,n,NA)
    }
    
  }
}
name_Bank<-rename(name_Bank,c("Bank_Name"="S.name"))
file_data_bank<-left_join(file_data_bank,name_Bank,by="Bank_Name")
file_data_bank<-file_data_bank %>%
  select(-file_list_bank) %>%
  relocate(file_list_1,folder_name,Name,Bank_Name,year)
file_data_bank<-rename(file_data_bank, c("file_name"="file_list_1"))
####to generate unique id and idetified duplicate files
file_data_bank$uid<-paste0(file_data_bank$Bank_Name,".",file_data_bank$year)
n_occur <- data.frame(table(file_data_bank$uid))
# n_occur
n_occur<-rename(n_occur, c("uid"="Var1"))
file_data_bank<-left_join(file_data_bank,n_occur, by="uid")
file_data_bank<-file_data_bank %>%
  arrange(Bank_Name)
file_page_Bank <- read_excel("Data Frame.xlsx",
                             sheet = "Bank data long")

file_page_Bank$Bank_Name<-file_page_Bank$Bank_name
file_page_Bank$year<-file_page_Bank$Year
file_page_Bank$uid<-paste0(file_page_Bank$Bank_Name,".",file_page_Bank$year)
file_page_Bank<-left_join(file_page_Bank,name_Bank, by="Bank_Name")

data_merge_bank <- left_join(file_data_bank,file_page_Bank,by ="uid")
bank_data<-data_merge_bank %>%
  select(uid,file_name,folder_name,Name.x,Bank_Name.x,year.x,
         Availability_of_report,Chair,CEO,Char_CEO,Remark,path,Rank,Assets) %>%
  filter(is.na(Name.x)==FALSE)
bank_data<-rename(bank_data,c("Bank.Name"="Name.x","ID"="Bank_Name.x",
                              "year"="year.x"))
bank_data$pdf<-grepl("^(.*?).pdf|.PDF",bank_data$file_name)
final_t_chair<-PDF_DATAFRAME(bank_data,columns=c("Chair"))
final_t_CEO<-PDF_DATAFRAME(bank_data,columns=c("CEO"))
final_t_Chair_CEO<-PDF_DATAFRAME(bank_data,columns=c("Char_CEO"))
final_t_chair<-rename(final_t_chair,c("txt_chair"="txt"))
final_t_CEO<-rename(final_t_CEO,c("txt_CEO"="txt"))
final_t_Chair_CEO<-rename(final_t_Chair_CEO,c("txt_Chair_CEO"="txt"))
bank_data_final<-left_join(bank_data,final_t_chair,by="uid")
bank_data_final<-left_join(bank_data_final,final_t_CEO,by="uid")
bank_data_final<-left_join(bank_data_final,final_t_Chair_CEO,by="uid")
bank_data_final<-bank_data_final %>%
  select(uid,file_name,folder_name,Bank.Name,ID,year,
         Availability_of_report,Chair,txt_chair,CEO,txt_CEO,Char_CEO,
         txt_Chair_CEO,Remark,pdf,path,Rank,Assets)
bank_data_final<-bank_data_final %>%
  filter(is.na(Chair)==FALSE|is.na(CEO)==FALSE|is.na(Char_CEO)==FALSE & Availability_of_report=="Yes")
bank_data_final_1<-bank_data_final %>%
  filter(is.na(Chair)==TRUE & is.na(CEO)==TRUE & is.na(Char_CEO)==TRUE)
bank_data_final$Province<-"CA"
bank_data_final$Entity<-"Bank"
bank_data_final_unprocessed<-bank_data_final
save(bank_data_final_unprocessed, file = "Our data/bank_data_final_unprocessed.RData")

#### Merge CU and Bank

bank_data_final<-rename(bank_data_final,c("Name"="Bank.Name"))
credit_union_data_final<-credit_union_data_final%>%
  select(-Membership,-location)
data_final<-rbind(credit_union_data_final,bank_data_final)
data_final_unprocessed<-data_final
save(data_final_unprocessed, file = "Our data/data_final_unprocessed.RData")
#### prepared data for analysis 
# load("Our Data/data_final.RData")
data_final_merge<-data_final
library(stringr)
data_final_merge$text<-ifelse(is.na(data_final_merge$txt_Chair_CEO)==FALSE,data_final_merge$txt_Chair_CEO,
                              ifelse(is.na(data_final_merge$txt_CEO)==FALSE & is.na(data_final_merge$txt_chair)==TRUE,data_final_merge$txt_CEO,
                                     ifelse(is.na(data_final_merge$txt_CEO)==TRUE & is.na(data_final_merge$txt_chair)==FALSE,data_final_merge$txt_chair,
                                            ifelse(is.na(data_final_merge$txt_CEO)==FALSE & is.na(data_final_merge$txt_chair)==FALSE,str_c(data_final_merge$txt_CEO,'/n',data_final_merge$txt_chair),NA))))
data_final_merge<-data_final_merge %>%
  select(uid,text,Name,ID,year,Rank,Province,Entity,Assets)
data_final_merge_unprocessed<-data_final_merge
save(data_final_merge_unprocessed, file = "Our Data/data_final_merge_unprocessed.RData")
