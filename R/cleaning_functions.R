calc_duration<-function(df){
  df[["start"]]<-lubridate::ymd_hms(df[["start"]])
  df[["end"]]<-lubridate::ymd_hms(df[["end"]])
  difftime(as.POSIXct(df[["end"]]),as.POSIXct(df[["start"]]),units = "mins")
}

split_multiple_choice<-function(hh,questions,choices,sep="."){

  questions$type %>% ch %>% strsplit(.," ") %>% do.call(rbind,.)-> tosplit
  questions$choices <- ifelse(tosplit[,1]==tosplit[,2],NA,tosplit[,2])
  names(choices)<-paste0("ch_",names(choices))
  questionnaires<-merge(questions,choices,by.x="choices",by.y="ch_list_name",all=T)


  splitsmult<-function(hh,questionnaires,varname,sep=sep){
    chlist<-questionnaires$ch_name[which(questionnaires$name %in% varname)]
    binarysmult<-lapply(chlist,
                        function(x,hh,varname,sep){
                          filt<-grep(paste0("^",x," ","|"," ",x,"$","|","^",x,"$","|"," ",x," "),hh[[varname]])
                          hh[[paste0(varname,sep,x)]]<-c()
                          hh[[paste0(varname,sep,x)]][is.na(hh[[varname]])|hh[[varname]]=="NA"]<-NA
                          hh[[paste0(varname,sep,x)]][!is.na(hh[[varname]])&hh[[varname]]!="NA"]<-0
                          hh[[paste0(varname,sep,x)]][filt]<-1
                          return(hh[[paste0(varname,sep,x)]])
                        },hh=hh,varname=varname,sep=sep) %>% bind_cols
    names(binarysmult)<-paste(varname,chlist,sep=sep)
    return(binarysmult)
  }

  varname=questionnaires$name[grep("select_multiple",questionnaires$type)] %>% unique
  varname<-varname[varname%in%names(hh)]

  lapply(varname,splitsmult,hh=hh,questionnaires=questionnaires,sep=sep) %>% bind_cols -> splitteddata

  for (j in names(splitteddata)){
    hh[[j]]<-splitteddata[[j]]
  }
  return(hh)
}

other_check<-function(dbs,survey){

  dbs<-dbs %>% as.data.frame(stringAsFActors=F) %>% type_convert()
  date_u<-humanTime()

  cleantemplate<-data.frame(
    date=c(),
    enumerator= c(),
    uuid= c(),
    question.name = c(),
    original.value = c(),
    new.value = c(),
    parent.other.question = c(),
    parent.other.answer=c(),
    problem = c(),
    checkid= c(),
    action=c()
  )

  # names(dbs)<-tolower(names(dbs))

  # oth<-which((stringr::str_detect(names(dbs),"_other")|stringr::str_detect(names(dbs),"_other$"))&( sapply(dbs,class)=="factor"|sapply(dbs,class)=="character"))
  oth<-which((stringr::str_detect(names(dbs),"_other$"))&( sapply(dbs,class)=="factor"|sapply(dbs,class)=="character"))
  outother<-cleantemplate

  for (k in oth){

    oth_qname<-survey[["name"]][which(survey[["name"]]==names(dbs)[k])-1]
    indexoth<-which(!is.na(dbs[[names(dbs)[k]]]))

    if(sum(!is.na(dbs[[names(dbs)[k]]]))>0){
      ds<-data.frame(
        date=as.character(dbs[["date"]][indexoth]),
        enumerator=dbs[["enumerator_name"]][indexoth],
        uuid=dbs[["uuid"]][indexoth],
        question.name=rep(names(dbs)[k],length(indexoth)),
        original.value=dbs[indexoth,k],
        parent.other.question=rep(oth_qname,length(indexoth)),
        parent.other.answer=dbs[[oth_qname]][indexoth],
        problem=rep("others: to check if could be recoded",length(indexoth)),
        action="check",
        checkid="others"
      )

      ds<-ds[!is.na(ds$question.name)&!is.na(ds$original.value),]
      outother<-rbind(outother,ds)
    }
  }
  return(outother)
}

impl_clean<-function(data,uuid,dclean,uuid_log,qmname,newval,oldval,action,othermain){
  for (k in 1:nrow(dclean))
  {
    Taction<-dclean[[action]][k]
    x1<-as.character(dclean[[uuid_log]][k])
    if(any(data[[uuid]]==x1)){
      if(!is.na(Taction)&Taction!="note"&Taction!="nothing"&Taction!="check"){
        if(Taction=="remove"){
          data<-data[which(!data[[uuid]]%in%dclean[[uuid_log]][k]),]
        } else if(Taction=="recode_all"){
          data[[dclean[[qmname]][k]]][data[[dclean[[qmname]][k]]]==dclean[[oldval]][k]]<-dclean[[newval]][k]
        } else if(Taction=="recode"){
          X<-as.character(dclean[[uuid_log]][k])
          Y<-as.character(dclean[[othermain]][k])
          val<-dclean[[newval]][k]
          data[[Y]]<-as.character(data[[Y]])
          # data[which(data[[uuid]]==X),which(names(data)==Y)]<-as.character(val)
          data[[Y]][which(data[[uuid]]==X)]<-as.character(val)

        # } else if(Taction=="recode_sm"){
        #   X<-as.character(dclean[[uuid_log]][k])
        #   Y<-as.character(dclean[[othermain]][k])
        #   val<-dclean[[othertextvar]][k]
        #   data[[Y]]<-as.character(data[[Y]])
        #   data[which(data[[uuid]]==X),which(names(data)==Y)]<-gsub("autre",as.character(val),data[which(data[[uuid]]==X),which(names(data)==Y)])
        # } else if(Taction=="append_sm"){
        #   X<-as.character(dclean[[uuid_log]][k])
        #   Y<-as.character(dclean[[othermain]][k])
        #   val<-dclean[[othertextvar]][k]
        #   if(data[which(data[[uuid]]==X),which(names(data)==Y)]=="NA"|is.na(data[which(data[[uuid]]==X),which(names(data)==Y)])){
        #     data[which(data[[uuid]]==X),which(names(data)==Y)]<-as.character(val)
        #   } else {
        #     data[which(data[[uuid]]==X),which(names(data)==Y)]<-paste0(data[which(data[[uuid]]==X),which(names(data)==Y)]," ",as.character(val))
        #   }
        } else if(Taction=="change") {
          X<-as.character(dclean[[uuid_log]][k])
          Y<-as.character(dclean[[qmname]][k])
          val<-dclean[[newval]][k]
          data[[Y]]<-as.character(data[[Y]])
          data[[Y]][which(data[[uuid]]==X)]<-as.character(val)
        }
        #
        # if(!is.na(dclean[[variabletoclean]][k])){
        #   Ytoclean<-dclean[[variabletoclean]][k]
        #   valtoclean<-dclean[[choicestoclean]][k]
        #   data[,which(names(data)==Ytoclean)]<-as.character(data[,which(names(data)==Ytoclean)])
        #   data[which(data[[uuid]]==X),which(names(data)==Ytoclean)]<-as.character(valtoclean)
        # }
      }
    }
  }
  return(data)
}

cleaning_data <- function(db, clog, questions, choices){
  clean<-db %>% impl_clean("uuid",clog,"uuid","question.name","new.value","original.value","action","parent.other.question")
  clean<- rec_missing_all(clean)
  clean<-clean %>% type_convert()
  clean<-clean %>% split_multiple_choice(questions,choices)
  return(clean)

}

makeslog<-function(data,logbook,checkid="empty",index,question.name,explanation,parent.other.question="NULL",parent.other.answer="NULL",new.value="NULL",action="check"){
  if(length(index)>0){
    if(question.name=="all"){oldval<-"-"}else{oldval<-as.character(data[[question.name]][data$uuid%in%index])}
    newlog<-data.frame(
      date=as.character(data$date[data$uuid%in%index]),
      enumerator=data$enumerator_name[data$uuid%in%index],
      uuid= index,
      question.name = question.name,
      original.value=oldval,
      new.value=new.value,
      problem = explanation,
      parent.other.question=ifelse(parent.other.question=="NULL","NULL",as.character(data[[parent.other.question]][data$uuid%in%index])),
      parent.other.answer=ifelse(parent.other.answer=="NULL","NULL",as.character(data[[parent.other.answer]][data$uuid%in%index])),
      checkid= checkid,
      action=action)
    bind_rows(logbook,newlog)
  } else{
    logbook
  }
}

check_blanks<- function(data){
  apply(data, 1,function(x) {length(which(is.na(x)))})/ncol(data)*100
}
