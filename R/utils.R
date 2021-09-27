latin_to_utf8<-function(x, from="latin1", to="UTF-8"){Encoding(x) <- from;iconv(x, from, to,sub='')}

remove_blank_headings<-function(data){data[,names(data)!=""]}
remove_vars<-function(data,vars){data[,names(data) %!in%vars]}

`%!in%` = Negate(`%in%`)

humanTime <- function() {
  format(Sys.time(), "%Y%m%d-%H%M%OS")
}

rec_missing<-function(x,missings=c(NULL,'NULL','N/A','n/a',' ','(vide)','d/m','','NA','na',""," ")) {
  x[x %in% missings] <- NA
  return(x)
}

rec_missing_all<-function(data){lapply(data,rec_missing) %>% bind_cols}

cleanheaders<-function(data){
    names(data)<-gsub("^X_","",names(data))
    names(data)<-gsub("^_","",names(data))
    names(data)<-gsub("\\/",".",names(data))
    names(data)[which(names(data)=="settlement_other_001")]<-"settlement_type_other"

  return(data)
    }

prepdata<-function(data){data %>% cleanheaders(.) %>% rec_missing_all %>% remove_blank_headings %>% type_convert}

ch<-as.character
chr<-as.character

label_clog<- function(clog,survey,choices,survey_label,choices_label){

  choices_label <- choices[[choices_label]]
  survey_label <- survey[[survey_label]]
  question.name_label <- match(clog[["question.name"]], survey[["name"]])
  # old.value_label <- match(clog[["original.value"]], choices[["name"]])
  parent.other.question_label <- match(clog[["parent.other.question"]], survey[["name"]])
  # parent.other.answer_label<-match(clog[["parent.other.answer"]], choices[["name"]])

  original.value_label<-str_split(clog[["original.value"]]," ")
  original.value_label<-lapply(original.value_label, function(x)match(x, choices[["name"]])) %>%
    lapply(.,function(x){ifelse(is.na(x),x,choices_label[x])}) %>% lapply(., function(x)paste(x,collapse = " ")) %>% unlist

  parent.other.answer_label<-str_split(clog[["parent.other.answer"]]," ")
  parent.other.answer_label<-lapply(parent.other.answer_label, function(x)match(x, choices[["name"]])) %>%
    lapply(.,function(x){ifelse(is.na(x),x,choices_label[x])}) %>% lapply(., function(x)paste(x,collapse = " ")) %>% unlist

  labeled_clog <- clog %>%
    mutate(question.name_label = ifelse(is.na(question.name_label),question.name,survey_label[question.name_label]),
           original.value_label =ifelse(is.na(original.value_label)|original.value_label=="NA",original.value,original.value_label),
           parent.other.question_label = ifelse(is.na(parent.other.question_label),parent.other.question,survey_label[parent.other.question_label]),
           parent.other.answer_label =ifelse(is.na(parent.other.answer_label)|parent.other.answer_label=="NA",parent.other.answer,parent.other.answer_label)
    )

  # labeled_clog <- clog %>%
  #   mutate(question.name_label = ifelse(is.na(question.name_label),question.name,survey_label[question.name_label]),
  #          old.value_label = ifelse(is.na(old.value_label),old.value,choices_label[old.value_label]),
  #          parent.other.question_label = ifelse(is.na(parent.other.question_label),parent.other.question,survey_label[parent.other.question_label])
  #          )

  vars<-c("date","enumerator","state","locality","uuid","question.name","question.name_label","original.value","original.value_label","new.value","parent.other.question","parent.other.question_label","parent.other.answer","parent.other.answer_label")
  labeled_clog<-labeled_clog %>% select(all_of(vars),everything())

  return(labeled_clog)
}

load_file <- function(name, path, sheetname=1) {
  ext <- tools::file_ext(name)
  switch(ext,
         csv=read.csv(path,stringsAsFactors = F),
         xlsx=readxl::read_excel(path,col_types = "text",sheet = sheetname),
         xls=readxl::read_excel(path,col_types = "text",sheet = sheetname),
         validate("Invalid file; Please upload a .csv .xlsx or .xls file")
  )
}

pulluuid<-function(data,logiquetest){data$uuid[which(logiquetest)]}

from_xml_tolabel<-function(db,choices,survey,choices_label,survey_label){
  multiple_choices <- filter(survey, str_detect(type, "(\\bselect_multiple\\b)"))$name
  chr_names<-db %>% select_if(~ !(all(is.na(.x)))) %>% select_if(~ is.character(.)) %>% names
  chr_names<-chr_names[!str_detect(chr_names,paste(paste0(multiple_choices,"."),collapse = "|"))]
  # names(choices)<-gsub(":.*","",names(choices))
  # names(survey)<-gsub(":.*","",names(survey))
  choice_labels <- choices[[choices_label]]
  survey_labels <- survey[[survey_label]]

  for (i in 1: length(chr_names)){
    if(chr_names[i]%in%multiple_choices){
      split_sm<-str_split(db[[chr_names[i]]]," ")
      db[[chr_names[i]]]<-lapply(split_sm, function(x)match(x, choices[["name"]])) %>%
        lapply(.,function(x){ifelse(is.na(x),x,choice_labels[x])}) %>% lapply(., function(x)paste(x,collapse = " ")) %>% unlist
    } else{
      var_label <- match(db[[chr_names[i]]], choices[["name"]])
      db[[chr_names[i]]]<-ifelse(is.na(var_label),db[[chr_names[i]]],choice_labels[var_label])
    }
  }
  names(db)<-gsub(".*[.]","",names(db))
  label_indices<-match(names(db),survey[["name"]])
  names(db)<-ifelse(is.na(label_indices)|is.na(survey_labels[label_indices]),names(db),survey_labels[label_indices])
  choices_indices<-match(names(db),choices[["name"]])
  names(db)<-ifelse(is.na(choices_indices),names(db),choice_labels[choices_indices])
  return(db)
}
