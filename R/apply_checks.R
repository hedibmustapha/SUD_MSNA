apply_checks<-function(db, dc_method){
  logbook<-data.frame(
    date=character(),
    enumerator= character(),
    state=character(),
    locality=character(),
    uuid= character(),
    question.name = character(),
    original.value = character(),
    new.value = character(),
    parent.other.question = character(),
    parent.other.answer = character(),
    problem = character(),
    action=character()
  )

# new variables
data<-db
data$difftime<-calc_duration(data)

if(dc_method=="phone"){
  index<-pulluuid(data, data$phone_outcome%in%c("not_avai","no_ans"))
  logbook<- makeslog(data,logbook,"id04",index,"phone_outcome","No response",action = "remove")
}

index<-pulluuid(data,data$consent=="no")
logbook<- makeslog(data,logbook,"id05",index,"consent","Consent declined",action = "remove")

index<-pulluuid(data,data$origin_status!="sudanese")
logbook<- makeslog(data,logbook,"id06",index,"origin_status","Household is not Sudanese",action = "remove")

index<-pulluuid(data,data$age_consent_hm== "no")
logbook<- makeslog(data,logbook,"id07",index,"age_consent_hm","Consent declined", action = "remove")

index<-pulluuid(data,data$age_consent_hhh== "no")
logbook<- makeslog(data,logbook,"id08",index,"age_consent_hhh","Consent declined", action = "remove")

if(dc_method=="phone"){
  f2f_state<-c("SD08","SD06","SD05","SD12","SD11","SD01","SD02","SD03","SD07","SD04","SD18")
  index<-pulluuid(data,data$assessment_state%in%f2f_state)
  logbook<- makeslog(data,logbook,"id19",index,"assessment_state","state for F2F collection", action = "remove")
}

index<-pulluuid(data, is.na(data$enumerator_name))
logbook<- makeslog(data,logbook,"id01",index,"enumerator_name","missing enumerator name")

index<-pulluuid(data, is.na(data$enumerator_phone))
logbook<- makeslog(data,logbook,"id02",index,"enumerator_phone","missing enumerator phone")

if(dc_method=="phone"){
  index<-pulluuid(data, duplicated(data$resp_phone))
  logbook<- makeslog(data,logbook,"id03",index,"resp_phone","duplicated respondent phone")
}

index<-pulluuid(data,data$hh_size>15)
logbook<- makeslog(data,logbook,"id09",index,"hh_size","Household size > 15")

index<-pulluuid(data,data$hhh_age>90)
logbook<- makeslog(data,logbook,"id10",index,"hhh_age","Head of Household is age over 90")

index<-pulluuid(data,data$child_absent>3)
logbook<- makeslog(data,logbook,"id11",index,"child_absent","Over 3 children away from home")

index<-pulluuid(data,data$displ_now=="yes"&data$locality_origin==data$assessment_locality)
logbook<- makeslog(data,logbook,"id12",index,"locality_origin","IDP but still lives in the same area of origin")

index<-pulluuid(data,(data$total_children/data$total_women)>8&data$total_women>0)
logbook<- makeslog(data,logbook,"id13",index,"total_children","More than 8 children per Adult woman")

index<-pulluuid(data,data$water_problems=="no"&sm_selected(data$priority_needs,any = c("water_drink")))
logbook<- makeslog(data,logbook,"id14",index,"priority_needs","No problem related to water (WASH section) but stated as priority need (AAP)")

index<-pulluuid(data,data$barriers_access=="no"&sm_selected(data$priority_needs,any = c("health")))
logbook<- makeslog(data,logbook,"id15",index,"priority_needs","No Health barriers (HEALTH section) but stated as priority need (AAP)")

index<-pulluuid(data,check_blanks(data)>70)
logbook<- makeslog(data,logbook,"id16",index,"uuid","More than 70% of columns are empty")

index<-pulluuid(data,data$difftime<15)
logbook<- makeslog(data,logbook,"id17",index,"difftime","Survey done in less than 15min")

index<-pulluuid(data,data$difftime>180)
logbook<- makeslog(data,logbook,"id18",index,"difftime","Survey took 3 hours to complete")

# index<-pulluuid(data,!is.na(data$birth_care)&data$total_women==0)
# logbook<- makeslog(data,logbook,"id19",index,"birth_care","One or more women have given birth, but no woman in HH composition")

return(logbook)
}
