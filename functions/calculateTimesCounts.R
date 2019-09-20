view_page_time = function(diffTime,nextDiffTime,prevDiffTime,sess_ref,nextSess,prevSess,action,nextAction,prevAction){
    
  times = nextDiffTime[sess_ref==nextSess&action %in% c("VIEW_PAGE","VIEW_MODULE_PAGE")&!nextAction%in%c("START_ATTEMPT","START_SESSION","EVALUATE_QUESTION","VIEW_HINT")]
  page_view_time = sum(times)
  return(page_view_time)
}

view_page_count = function(diffTime,nextDiffTime,prevDiffTime,sess_ref,nextSess,prevSess,action,nextAction,prevAction){
  
  times = nextDiffTime[sess_ref==nextSess&action %in% c("VIEW_PAGE","VIEW_MODULE_PAGE")&!nextAction%in%c("START_ATTEMPT","START_SESSION","EVALUATE_QUESTION","VIEW_HINT")] 
  
  page_view_count = length(times)
  return(page_view_count)
}

view_page_time_trimmed = function(diffTime,nextDiffTime,prevDiffTime,sess_ref,nextSess,prevSess,action,nextAction,prevAction){
  
  times = nextDiffTime[sess_ref==nextSess&action %in% c("VIEW_PAGE","VIEW_MODULE_PAGE")&!nextAction%in%c("START_ATTEMPT","START_SESSION","EVALUATE_QUESTION","VIEW_HINT")]
  
  meanTimes = mean(times)
  low10 = quantile(times,c(0.10,0.90),na.rm = TRUE)[1]
  high10 = quantile(times,c(0.10,0.90),na.rm = TRUE)[2]
  
  times = times[times>low10]
  times[times>high10] = meanTimes
  page_view_time_trimmed = sum(times)
  
  return(page_view_time_trimmed)
}
  

view_page_count_trimmed = function(diffTime,nextDiffTime,prevDiffTime,sess_ref,nextSess,prevSess,action,nextAction,prevAction){
  
  times = nextDiffTime[sess_ref==nextSess&action %in% c("VIEW_PAGE","VIEW_MODULE_PAGE")&!nextAction%in%c("START_ATTEMPT","START_SESSION","EVALUATE_QUESTION","VIEW_HINT")]
  
  meanTimes = mean(times)
  low10 = quantile(times,c(0.10,0.90),na.rm = TRUE)[1]
  high10 = quantile(times,c(0.10,0.90),na.rm = TRUE)[2]
  
  times = times[times>low10]
  times[times>high10] = meanTimes
  page_view_count_trimmed = length(times)
  
  return(page_view_count_trimmed)
}


action_time = function(diffTime,nextDiffTime,prevDiffTime,sess_ref,nextSess,prevSess,action,nextAction,prevAction){
 
  times = diffTime[sess_ref==prevSess&action%in%c("START_ATTEMPT","START_SESSION","EVALUATE_QUESTION","VIEW_HINT") & prevAction%in%c("START_ATTEMPT","START_SESSION","EVALUATE_QUESTION","VIEW_HINT")]
  action_time = sum(times)
  
  return(action_time)
}

action_count = function(diffTime,nextDiffTime,prevDiffTime,sess_ref,nextSess,prevSess,action,nextAction,prevAction,time,nextTime,prevTime){
  
  times = diffTime[sess_ref==prevSess&action%in%c("START_ATTEMPT","START_SESSION")&!action==prevAction & diffTime!=0 & prevAction != "VIEW_PAGE"]
  
  #times = diffTime[sess_ref==prevSess & action%in%c("START_ATTEMPT","START_SESSION") & prevAction!="VIEW_PAGE"]
  
  #times = diffTime[action%in%c("START_ATTEMPT","START_SESSION","EVALUATE_QUESTION","VIEW_HINT","SAVE_ATTEMPT","SUBMIT_ATTEMPT")]
  action_count = length(times)
  
  return(action_count)
}

page_to_action_time = function(diffTime,nextDiffTime,prevDiffTime,sess_ref,nextSess,prevSess,action,nextAction,prevAction){
 
  times = diffTime[sess_ref==prevSess&action%in%c("START_ATTEMPT","START_SESSION","EVALUATE_QUESTION","VIEW_HINT") & prevAction %in% c("VIEW_PAGE","VIEW_MODULE_PAGE")]
  page_to_action_time = sum(times)
  
  return(page_to_action_time)
}

page_to_action_count = function(diffTime,nextDiffTime,prevDiffTime,sess_ref,nextSess,prevSess,action,nextAction,prevAction){
  
  times = diffTime[sess_ref==prevSess&action%in%c("START_ATTEMPT","START_SESSION","EVALUATE_QUESTION","VIEW_HINT") & prevAction %in% c("VIEW_PAGE","VIEW_MODULE_PAGE")]
  page_to_action_count = length(times)

return(page_to_action_count)
}

action_to_page_time = function(diffTime,nextDiffTime,prevDiffTime,sess_ref,nextSess,prevSess,action,nextAction,prevAction){
  
  times = diffTime[sess_ref==prevSess&action %in% c("VIEW_PAGE","VIEW_MODULE_PAGE") & prevAction%in%c("START_ATTEMPT","START_SESSION","EVALUATE_QUESTION","VIEW_HINT")]
  action_to_page_time = sum(times)
  
  return(action_to_page_time)
}

action_to_page_count = function(diffTime,nextDiffTime,prevDiffTime,sess_ref,nextSess,prevSess,action,nextAction,prevAction){
  
  times = diffTime[sess_ref==prevSess&action %in% c("VIEW_PAGE","VIEW_MODULE_PAGE") & prevAction%in%c("START_ATTEMPT","START_SESSION","EVALUATE_QUESTION","VIEW_HINT")]
  action_to_page_count = length(times)
  
  return(action_to_page_count)
}
 
