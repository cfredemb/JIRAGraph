require(tidyverse)
require(plotly)
require(shiny)
require(shinymaterial)
require(DT)
require(streamgraph)
require(shinyWidgets)
require(shinyjs)
require(networkD3)
# TO UPLOAD FROM INTRANET
# setwd(); options(rsconnect.check.certificate = FALSE) ; rsconnect::deployApp() 




ui <- material_page(
       # useShinyjs(),
    title="HoG JIRA Statistics", nav_bar_color  = "deep-purple",
    material_tabs(
      tabs= c(
        "JIRA Data Quality" = "JIRA_data_quality"
        ,"JIRA Effort measures" = "JIRA_effort_measures"
      ), color = "deep-purple"
    ),
     
    material_tab_content(tab_id = "JIRA_data_quality"
                         
            ,material_row(
              material_column(
                         material_card(title= "upload your JIRA file"
                                        ,depth = 5
                                        ,color = "blue-grey lighten-4"
                                        ,fileInput("file1", "Choose CSV File",
                                                   accept = c(
                                                     "text/csv",
                                                     "text/comma-separated-values,text/plain",
                                                     ".csv")
                                                   
                                                   )
                                       ,checkboxInput("header", "Header", TRUE)
                         ), width = 4)
              ,material_column(
                           material_card(title = "select status to include"
                                         ,depth = 5
                                         ,color = "blue-grey lighten-4"
                                         ,material_checkbox("bklg","inlcude backlog",T)
                                         ,material_checkbox("ipr","inlcude in progress",T)
                                         ,material_checkbox("itest","inlcude in test",T)
                                         ,material_checkbox("donetasks","inlcude done",T)
                                         )
                           ,width = 4 )
                         ,material_column(
              
                            material_card(title = "Include tasks active in the last [X] days"
                                        ,depth = 5
                                        ,color = "blue-grey lighten-4"
                                        ,material_slider('time_include','activity threshold (days)', min_value=1,max_value=90,initial_value = 15)
                         ) ,width = 4)
             )
            ,material_row(
              material_column(material_card("Unassigned Tasks",DTOutput('unass_tasks')),width=6)
              ,material_column(material_card("Unestimated Tasks",DTOutput('Unestimated_Tasks')),width=6)
            )
            ,material_row(
              material_column(material_card("Orphaned tasks", DTOutput('orphaned_tasks')),width=6)
              ,material_column(material_card("Potentially Late Tasks", sliderInput('late_1','Late Tasks, in progress threshold (days)',min=1,max=60,value=7)
                                            ,sliderInput('late_2','Late Tasks, backlog threshold (days)',min=1,max=180,value=30)
                                            ,DTOutput('late_Tasks')),width=6)
              
            )
            ,material_row(material_card("People with outstanding actions",plotlyOutput('naughty_people')))
            
            
    ), material_tab_content(tab_id = "JIRA_effort_measures"
                            ,material_row(material_card(
                                          title = "Actual and Planned Effort"
                                          ,switchInput("effort_switch",value=T,onLabel = "Time-based",offLabel = "Task-based",size='mini')
                                          ,plotlyOutput('planned_effort')
                            ))
                            ,material_row(material_column(
                                            material_card(title = "stream-centric past period effort"
                                           ,material_switch("time_task", "",on_label = "Time-based",off_label = "Task-based",initial_value=T)
                                           ,br()
                                           ,br()
                                           ,plotlyOutput('sunburst_wep')
                                            )
                                           ,width = 6
                                            )
                                          ,material_column(
                                            material_card(title = "person-centric past period effort"
                                                          ,material_switch("time_task2", "",on_label = "Time-based",off_label = "Task-based",initial_value=T)
                                                          ,br()
                                                          ,br()
                                                          ,plotlyOutput('sunburst_pew')
                                            ),width = 6
                                          )
                              )
                            ,material_row(material_card("Task Summary" , DTOutput('total_table')))
                            ,material_row(material_card("Effort Evolution" 
                                                        ,switchInput("sankey_effort",value=T,onLabel = "Time-based",offLabel = "Task-based",size='mini')
                                                        ,sankeyNetworkOutput('sankey')))
                            
      )
)
 
server <- function(input, output,session) {

  #close session when browser closes
  session$onSessionEnded(stopApp)
  
  
  # would like to use a pop up here but doesn't seem to work in material at the moment??
  
  # showModal( modalDialog(
  #   
  #   "upload your JIRA file"
  #   # ,fileInput("file1", "Choose CSV File",
  #   #            accept = c(
  #   #              "text/csv",
  #   #              "text/comma-separated-values,text/plain",
  #   #              ".csv")
  #   #            
  #   # )
  #   # ,checkboxInput("header", "Header", TRUE)
  #   ,easyClose = T
  #   ,footer = NULL
  #   )
  # )
#  showModal(query_modal)
  
  ##read the csv in a reactive container
  re <- reactive({
    #inFile <- req(input$file1)
    req(input$file1)
    inFile <- input$file1 
     # if (is.null(inFile))
     #   return(NULL)
     # print(inFile)
     # print(inFile$datapath)
     # print(input$header)
     tmp <- read.csv(inFile$datapath, header = input$header)
     tmp2 <- JIRA_data_cleanup(tmp)
#View(tmp)
  #filter for time (remove obsolete tasks) use the last updated task
     tmp <- tmp2[(Sys.Date() - as.Date(tmp2$date_updated)) < input$time_include ,]
     #create data for the sankey graph
     tmp_sankey <- tmp2[(Sys.Date() - as.Date(tmp2$date_updated)) < 2*input$time_include ,]
     results <- list()
     results$mydf <- tmp
     results$mydf_sankey <- tmp_sankey
     return(results)
  })  
  
  
  
  ## Orphaned task table
  output$unass_tasks <- renderDT(
    {
      toto <- re()
        mytable <- toto$mydf
        mytable %>% 
          transmute(Assignee = gsub('([0-9]|@).*','',Assignee), Creator = gsub('([0-9]|@).*','',Creator), Summary, Issue_key, Status, Labels) %>%
          filter(Assignee == 'unassigned') -> tmp
        if(!input$donetasks)
          tmp %>% filter(Status!='done') -> tmp
        if(!input$bklg)
          tmp %>% filter(Status!='backlog') -> tmp
        if(!input$ipr)
          tmp %>% filter(Status!='in progress' & Status!='selected for development') -> tmp
        if(!input$itest)
          tmp %>% filter(Status!='testing' & Status!='released to b.o.') -> tmp
        
        datatable(tmp, escape = FALSE,options=list(pageLength = 10, paging = FALSE, scrollY = "350px", autoHideNavigation=T),rownames = F)
      
  })
  
  ## Unestimated task table
  output$Unestimated_Tasks <- renderDT(
    {
      toto <- re()
      mytable <- toto$mydf
      
        mytable %>% 
          transmute(Assignee = gsub('([0-9]|@).*','',Assignee), Effort, Summary, Issue_key, Status, Labels) %>%
          filter(is.na(Effort)|Effort=='') -> tmp
        if(!input$donetasks)
          tmp %>% filter(Status!='done') -> tmp
        if(!input$bklg)
          tmp %>% filter(Status!='backlog') -> tmp
        if(!input$ipr)
          tmp %>% filter(Status!='in orogress' & Status!='selected for development') -> tmp
        if(!input$itest)
          tmp %>% filter(Status!='testing' & Status!='released to b.o.') -> tmp
        datatable(tmp, escape = FALSE,options=list(pageLength = 10, paging = FALSE, scrollY = "350px", autoHideNavigation=T),rownames = F)
      
    })
  
  ## dead people tasks task table
  output$orphaned_tasks <- renderDT(
    {
      toto <- re()
      mytable <- toto$mydf
      
        mytable %>% 
          transmute(Assignee = gsub('([0-9]|@).*','',Assignee), Creator = gsub('([0-9]|@).*','',Creator), Summary, Issue_key, Status, Labels) %>%
          filter(Assignee %in% c('adouglas','hwang')) -> tmp
        if(!input$donetasks)
          tmp %>% filter(Status!='done') -> tmp
        if(!input$bklg)
          tmp %>% filter(Status!='backlog') -> tmp
        if(!input$ipr)
          tmp %>% filter(Status!='in progress' & Status!='selected for development') -> tmp
        if(!input$itest)
          tmp %>% filter(Status!='testing' & Status!='released to b.o.') -> tmp
        
        datatable(tmp, escape = FALSE,options=list(pageLength = 10, paging = FALSE, scrollY = "350px", autoHideNavigation=T))
      
    })
    
  ## late tasks table
  output$late_Tasks <- renderDT(
    {
      toto <- re()
      mytable <- toto$mydf
      
        today <- Sys.Date()
        
        mytable %>% 
          transmute(Assignee = gsub('([0-9]|@).*','',Assignee), Summary, Issue_key, Status, Labels, last_date = as.Date(date_updated)) %>%
          filter( 
            (Status %in% c("in progress","selected for development") &  (today - last_date) > input$late_1)  | (Status %in% c("backlog") &  (today - last_date) > input$late_2)) -> tmp
        if(!input$donetasks)
          tmp %>% filter(Status!='done') -> tmp
        if(!input$bklg)
          tmp %>% filter(Status!='backlog') -> tmp
        if(!input$ipr)
          tmp %>% filter(Status!='in progress' & Status!='selected for development') -> tmp
        if(!input$itest)
          tmp %>% filter(Status!='testing' & Status!='released to b.o.') -> tmp
        
        datatable(tmp, filter='top', escape = FALSE,options=list(pageLength = 10, paging = FALSE, scrollY = "350px", autoHideNavigation=T),rownames = F)
      
    })

  #naughty graph
  output$naughty_people <- renderPlotly({
    
    toto <- re()
    mytable <- toto$mydf
    
      if(!input$donetasks)
        mytable %>% filter(Status!='done') -> mytable
      if(!input$bklg)
        mytable %>% filter(Status!='backlog') -> mytable
      if(!input$ipr)
        mytable %>% filter(Status!='in progress' & Status!='selected for development') -> mytable
      if(!input$itest)
        mytable %>% filter(Status!='testing' & Status!='released to b.o.') -> mytable
      
      #not super efficient, but we'll group people according to their category using the same logic as for the tables
      
      #get the creator of unassigned tasks
      mytable %>% 
        transmute(Assignee = gsub('([0-9]|@).*','',Assignee), Creator = gsub('([0-9]|@).*','',Creator), Summary, Issue_key, Status, Labels) %>%
        filter(Assignee == '') %>% group_by(Creator) %>% summarise(ttl = n()) %>% transmute(Person=Creator,ttl,type='unassigned') -> tmp_df1
      
      #get the assignee of unestimated tasks
      mytable %>% 
        transmute(Assignee = gsub('([0-9]|@).*','',Assignee), Effort, Summary, Issue_key, Status, Labels) %>%
        filter(is.na(Effort)|Effort=='') %>% group_by(Assignee) %>% summarise(ttl = n()) %>% transmute(Person=Assignee,ttl,type='unestimated') -> tmp_df2
      
      #get the creator of dead people tasks
      mytable %>% 
        transmute(Assignee = gsub('([0-9]|@).*','',Assignee), Creator = gsub('([0-9]|@).*','',Creator), Summary, Issue_key, Status, Labels) %>%
        filter(Assignee %in% c('adouglas','epegg')) %>% group_by(Creator) %>% summarise(ttl = n()) %>% transmute(Person=Creator,ttl,type='non_member') -> tmp_df3
      
      #get the assignee of late tasks
      mytable %>%
        transmute(Assignee = gsub('([0-9]|@).*','',Assignee) , Summary, Issue_key, Status, Labels, last_date = as.Date(date_updated)) %>%
        filter( 
          (Status %in% c("in progress","selected for development") &  (Sys.Date() - last_date) > input$late_1)  | (Status %in% c("backlog") &  (Sys.Date() - last_date) > input$late_2)) %>% 
        group_by(Assignee) %>%   
        summarise(ttl = n()) %>% transmute(Person=Assignee,ttl,type='late') -> tmp_df4
      
      tmp_df_tot <- bind_rows(tmp_df1,tmp_df2,tmp_df3,tmp_df4)
      
      plot_ly(data=tmp_df_tot[tmp_df_tot$type=='late',],x=~ttl,y=~Person,type='bar',orientation='h', name=if(dim(tmp_df_tot[tmp_df_tot$type=='late',])[1] > 0) 'late' else  NULL) %>%
        add_trace(data=tmp_df_tot[tmp_df_tot$type=='non_member',],x=~ttl,y=~Person, name=if(dim(tmp_df_tot[tmp_df_tot$type=='non_member',])[1] > 0) 'not a member' else  NULL) %>% 
        add_trace(data=tmp_df_tot[tmp_df_tot$type=='unestimated',],x=~ttl,y=~Person,name=if(dim(tmp_df_tot[tmp_df_tot$type=='unestimated',])[1] > 0) 'unestimated' else  NULL) %>% 
        add_trace(data=tmp_df_tot[tmp_df_tot$type=='unassigned',],x=~ttl,y=~Person,name=if(dim(tmp_df_tot[tmp_df_tot$type=='unassigned',])[1] > 0) 'unassigned' else  NULL) %>% 
        layout(barmode='stack')
        
    
  })
  
  
  #effort graph
  output$planned_effort <- renderPlotly({
    
  #  if(!is.null(re())){
    toto <- re()
    mytable2 <- toto$mydf
    
    mytable <- JIRA_effort_graph(mytable2,input$effort_switch)
    if(input$effort_switch){
   #   print("toto input switch true")
    plot_ly(data=mytable,x=~effort_done,y=~Person,type='bar',orientation='h',name='jira hours past period') %>%
      add_trace(x=~effort_planned,name='commited jira hours next period') %>% layout(barmode='stacked')
    }else{
    #  print("toto input switch false")
      plot_ly(data=mytable,x=~effort_done,y=~Person,type='bar',orientation='h',name='jira tasks past period') %>%
        add_trace(x=~effort_planned,name='commited jira tasks next period') %>% layout(barmode='stacked')
      
      
    }
   # }
  })
  
  
  #streamgraph
  output$backlog_growth <- renderStreamgraph({
  #   if(!is.null(re())){
  #     mytable <- JIRA_status_streamgraph(re())
  #     #print(mytable)
  # streamgraph(mytable,Status,my_count,my_date, offset="zero", interpolate="step") %>%
  #   sg_axis_x(2,"day", '%d/%b')
  #  }
  })
  
  #first sunburst graph
  output$sunburst_wep <- renderPlotly({
    toto <- re()
    mytable <- toto$mydf
    
      if(input$time_task)
      JIRA_sunburst_sep(mytable,"time")
      else
        JIRA_sunburst_sep(mytable,"task")
    
  })
  
  #second sunburst graph
  output$sunburst_pew <- renderPlotly({
    toto <- re()
    mytable <- toto$mydf
    
      if(input$time_task2)
      JIRA_sunburst_psE(mytable,"time")
      else
        JIRA_sunburst_psE(mytable,"task")
    
  })
  
  #dumb table total_table
  output$total_table <- renderDT(
    {
      toto <- re()
      mytable <- toto$mydf
      
        mytable %>% 
          transmute(Assignee = gsub('([0-9]|@).*','',Assignee), Creator = gsub('([0-9]|@).*','',Creator), Summary, Issue_key, Status, Labels, Epic_Link,Parent_id) -> tmp
        
        datatable(tmp, filter='top',escape = FALSE,options=list(pageLength = 10, paging = FALSE, scrollY = "350px", autoHideNavigation=T),rownames = F)
      
    })
  
  #sankey plot
  output$sankey <- renderSankeyNetwork({
    toto <- re()
    mytable <- toto$mydf_sankey
    
    
      if(input$sankey_effort)
      eff_type="time"
    else
      eff_type = "task"
    
    
    JIRA_sankey(mytable,input$time_include,eff_type)
    
    
  })
}

############################ outside functions ####################################
JIRA_effort_graph <- function(df,effort_switch=T){
 # print("toto")
  df %>%
    transmute(Assignee, Creator, Labels, Status, Issue_id, Parent_id, Summary,Epic_Link, Effort) %>%
    mutate(work_effort = str_c(gsub('([0-9]|@).*','',Assignee),Labels,sep="-")) ->
    df_temp
  #remove issues that are parent issues (because the effort gets automatically summed)
  #print(head(df_temp))
  #select the parents
#  is_parent <- (distinct(df2,Parent_id)) %>% drop_na()

#remove the parents from the df
#df2 %>% filter(!Issue_id %in% is_parent$Parent_id) -> df_temp

#filter to get the workload (time or nb task based depending on switch input)
if(effort_switch){
df_temp %>% transmute(Person = gsub('([0-9]|@).*','',Assignee) , Status = case_when(
  Status == 'done' ~ 'Done'
  ,Status == 'released to b.o.' ~ 'Done'
  ,TRUE ~ 'Planned') 
  , Effort = coalesce(Effort,'1')) %>% group_by(Person, Status) %>% summarise(total_effort = sum(as.numeric(Effort),na.rm=T)) %>% 
  arrange(Person,Status) -> work_effort_chart
} else {df_temp %>% transmute(Person = gsub('([0-9]|@).*','',Assignee) , Status = case_when(
  Status == 'done' ~ 'Done'
  ,Status == 'released to b.o.' ~ 'Done'
  ,TRUE ~ 'Planned') 
  , Effort = coalesce(Effort,'1')) %>% group_by(Person, Status) %>% summarise(total_effort = as.double(n())) %>% 
    arrange(Person,Status) -> work_effort_chart
  }
#try to speed up the plotting (it is very slow if names are unbalanced)
we_plan <- work_effort_chart[work_effort_chart$Status=='Planned',]
we_done <- work_effort_chart[work_effort_chart$Status=='Done',]

full_join(we_done, we_plan, by = "Person") %>% mutate(status_done = coalesce(Status.x,'Done')
                                                      ,effort_done = coalesce(total_effort.x,0)
                                                      ,status_planned = coalesce(Status.y,'Planned')
                                                      ,effort_planned = coalesce(total_effort.y,0)
) %>% select(Person,status_done,effort_done,status_planned,effort_planned) -> effort_join

return(effort_join)
}

JIRA_status_streamgraph <- function(df){
  
  
  #get the closed ones and the date they were closed
  
  df %>% filter(Status %in% c('released to b.o.','done')) %>%
    transmute(Status= 'Closed'
              ,my_date = as.Date.character(
                gsub('^$','31/dec/30',date_resolved)
                ,format = '%d/%b/%y')
              
              ,date_updated ) %>% rowwise() %>%
    transmute(Status, my_date = min(my_date,date_updated)) -> tmp_df1
  #print(tmp_df1)
  #get the ones in progress: cheat here, use the last updated date as the time it gor progressed
  
  df %>% filter(Status %in% c('selected for development','in progress','testing')) %>%
    transmute(Status = 'in progress', my_date = date_updated ) -> tmp_df2
  #print(tmp_df2)
  
  # get created ones (note: everything is created at some point)
  df %>% transmute(Status = 'created', my_date = date_created) -> tmp_df3
  #print(tmp_df3)
  
  #combine
  tmp_df_tot <- bind_rows(tmp_df1,tmp_df2,tmp_df3)
  
  #group by
  tmp_df_tot %>% group_by(Status,my_date) %>% summarise(my_count = n()) -> df_out
  return(df_out)
}

JIRA_sunburst_sep <- function(df,eff_type="time"){

  
  #lowercase as JIRA is case blind yet the extract is case sensitive -_-
  df <- mutate_all(df, .funs=str_to_lower)
  #df <- str_to_lower(df)
  df %>% 
    transmute(Assignee, Creator, Labels, Status, Issue_id, Parent_id , Epic_Link, Effort) %>% 
    mutate(work_effort = str_c(gsub('([0-9]|@).*','',Assignee),Labels,sep="-")) -> 
    df2
  #remove issues that are parent issues (because the effort gets automatically summed)
  
  # #select the parents
  # is_parent <- (distinct(df2,Parent_id)) %>% drop_na()
  # 
  # #remove the parents from the df
  # df2 %>% filter(!Issue_id %in% is_parent$Parent_id) -> df2
  
  df2 %>%
    transmute(layer1 = Labels, layer2 = Epic_Link, layer3 = Assignee ,Effort = as.numeric(coalesce(Effort,"1"))) -> 
    df3
  
  #print(head(df3))
  ## clean some of the data, notably the missing values
  # df3[df3$layer1=='',1] <- 'no label'
  # df3[df3$layer2=='',2] <- 'no epic'
  # df3[df3$layer3=='',3] <- 'unassigned'
  
  ##toggle the difference in effort vs number of tasks

    df3 %>% 
      transmute(layer1,Effort) %>% 
      group_by(layer1) %>%
      summarise(ttl=n(), ttlE = sum(Effort)) %>% 
      arrange(-ttl) -> 
      root_level
    
    df3 %>% 
      transmute(layer1,layer2,Effort) %>% 
      group_by(layer1,layer2) %>% 
      summarise(ttl=n(),ttlE = sum(Effort)) %>% 
      arrange(-ttl) -> 
      bottom_level
    
    df3 %>% 
      group_by(layer1,layer2,layer3) %>% 
      summarise(ttl=n(),ttlE = sum(Effort)) %>% 
      arrange(-ttl) -> 
      top_level
  
  #try to create the layers
  
  ##part 1: the root level: easy, no duplicates
  label_1 <- str_c(root_level$layer1,root_level$ttlE,sep='<br>')
  id_1 <- root_level$layer1
  weight_tsk <- root_level$ttl
  weight_time <- root_level$ttlE
  weight_1 <- rep(0,length(root_level$layer1))
  parent_1 <- rep("",length(weight_1))
  
  ##part 2: middle level: harder, duplicates
  
  label_2 <- str_c(bottom_level$layer2, bottom_level$ttlE,sep='<br>')
  id_2 <- str_c(bottom_level$layer1,bottom_level$layer2,sep='-')
  parent_2 <- bottom_level$layer1
  weight_2 <- rep(0,length(bottom_level$layer1))
  weight_tsk2 <- root_level$ttl
  weight_time2 <- root_level$ttlE
  #weight_2 <-bottom_level$ttl
  
  ##part 3: top layer: harder, duplicates and need to reference the right id for parent
  label_3 <- str_c(top_level$layer3,top_level$ttlE,sep='<br>')
  id_3 <- str_c(str_c(top_level$layer1,top_level$layer2,sep='-'),top_level$layer3,sep='-')
  parent_3 <- str_c(top_level$layer1,top_level$layer2,sep='-')
  if(eff_type=="time")
    weight_3 <-top_level$ttlE
  else{
    weight_3 <-top_level$ttl  
    label_1 <- str_c(root_level$layer1,root_level$ttl,sep='<br>')
    label_2 <- str_c(bottom_level$layer2, bottom_level$ttl,sep='<br>')
    label_3 <- str_c(top_level$layer3,top_level$ttl,sep='<br>')
    
  }
  weight_tsk3 <- root_level$ttl
  weight_time3 <- root_level$ttlE
  
  #combine_layers
  plot_labels <- append(append(label_1,label_2),label_3)
  plot_ids <- append(append(id_1,id_2),id_3)
  plot_parents <- append(append(parent_1,parent_2),parent_3)
  plot_weightss <- append(append(weight_1,weight_2),weight_3)
  
  plot_weighttsk <- append(append(weight_tsk,weight_tsk2),weight_tsk3)
  plot_weighttim <- append(append(weight_time,weight_time2),weight_time3)
  
  #create data frame to hold over data
  hover_data <- data.frame(key=plot_ids,stringsAsFactors = F)
  #hover_data$key <- plot_labels
  # hover_data$title <- gsub('-.+','',hover_data$key)
  # hover_data$nb_tasks <- unlist(lapply(plot_ids, function(toto){tt <- weight_tsk[which(id_1==gsub('-.+','',toto))]}))
  # hover_data$nb_time <- unlist(lapply(plot_ids, function(toto){tt <- weight_time[which(id_1==gsub('-.+','',toto))]}))
  # hover_data$ttltask <- rep.int(sum(weight_tsk),length(plot_labels))
  # hover_data$ttltime <- rep.int(sum(weight_time),length(plot_labels))
  # hover_data$top1_name <- 
  # hover_data$top2_name
  # hover_data$top3_name
  # hover_data$top1_val
  # hover_data$top2_val
  # hover_data$top3_val
  
  #return the data 
  # p <- plot_ly(ids=plot_ids,labels=plot_labels,parents=plot_parents,values=plot_weightss 
  #              ,type='sunburst',branchvalues='remainder'
  #              ,hoverinfo="text"
  #              ,hovertext = paste(hover_data$title,": <br><br>" 
  #                                 ,"#Tasks: " , hover_data$nb_tasks , "     [" ,format(round(100 * hover_data$nb_tasks/hover_data$ttltask,2)) , "% of total]"
  #                                 ,"<br> #Hours: ", hover_data$nb_time , "     [" ,format(round(100 * hover_data$nb_time/hover_data$ttltime,2)) , "% of total]"
  #                                 ,"<br><br>" , plot_labels , ": ", plot_weighttsk , "tasks ( " , plot_weighttim , " hours)"
  #                                 )
  #              
  #              )

  p <- plot_ly(ids=plot_ids,labels=plot_labels,parents=plot_parents,values=plot_weightss 
               ,type='sunburst',branchvalues='remainder'
  )
  
    return(p)
}

JIRA_sunburst_psE <- function(df,eff_type="time"){
  
  #lowercase as JIRA is case blind yet the extract is case sensitive -_-
  #df <- mutate_all(df, .funs=str_to_lower)
  #df <- str_to_lower(df)
  df %>% 
    transmute(Assignee, Creator, Labels, Status, Issue_id, Parent_id , Epic_Link, Effort) %>% 
    mutate(work_effort = str_c(gsub('([0-9]|@).*','',Assignee),Labels,sep="-")) -> 
    df2
  #remove issues that are parent issues (because the effort gets automatically summed)
  
  # #select the parents
  # is_parent <- (distinct(df2,Parent_id)) %>% drop_na()
  # 
  # #remove the parents from the df
  # df2 %>% filter(!Issue_id %in% is_parent$Parent_id) -> df2
  ##
  df2 %>%
    transmute(layer1 = Assignee , layer2 = Labels, layer3 = Epic_Link ,Effort = as.numeric(coalesce(Effort,"1"))) -> 
    df3
  
  
  #print(head(df3))
  ## clean some of the data, notably the missing values
  # df3[df3$layer1=='',1] <- 'unassigned'
  # df3[df3$layer2=='',2] <- 'no label'
  # df3[df3$layer3=='',3] <- 'no epic'
  
  if(eff_type=="time"){
    ##make the layers
    df3 %>% 
      transmute(layer1,Effort) %>% 
      group_by(layer1) %>%
      summarise(ttl=sum(Effort)) %>% 
      arrange(-ttl) -> 
      root_level
    
    df3 %>% 
      transmute(layer1,layer2,Effort) %>% 
      group_by(layer1,layer2) %>% 
      summarise(ttl=sum(Effort)) %>% 
      arrange(-ttl) -> 
      bottom_level
    
    df3 %>% 
      group_by(layer1,layer2,layer3) %>% 
      summarise(ttl=sum(Effort)) %>% 
      arrange(-ttl) -> 
      top_level
  } else {
    df3 %>% 
      transmute(layer1,Effort) %>% 
      group_by(layer1) %>%
      summarise(ttl=n()) %>% 
      arrange(-ttl) -> 
      root_level
    
    df3 %>% 
      transmute(layer1,layer2,Effort) %>% 
      group_by(layer1,layer2) %>% 
      summarise(ttl=n()) %>% 
      arrange(-ttl) -> 
      bottom_level
    
    df3 %>% 
      group_by(layer1,layer2,layer3) %>% 
      summarise(ttl=n()) %>% 
      arrange(-ttl) -> 
      top_level
  }
  
  #try to create the layers
  
  ##part 1: the root level: easy, no duplicates
  label_1 <- root_level$layer1
  id_1 <- root_level$layer1
  #weight_1 <- root_level$ttl
  weight_1 <- rep(0,length(root_level$layer1))
  parent_1 <- rep("",length(weight_1))
  
  ##part 2: middle level: harder, duplicates
  
  label_2 <- bottom_level$layer2
  id_2 <- str_c(bottom_level$layer1,bottom_level$layer2,sep='-')
  parent_2 <- bottom_level$layer1
  weight_2 <- rep(0,length(bottom_level$layer1))
  #weight_2 <-bottom_level$ttl
  
  ##part 3: top layer: harder, duplicates and need to reference the right id for parent
  label_3 <- top_level$layer3
  id_3 <- str_c(str_c(top_level$layer1,top_level$layer2,sep='-'),top_level$layer3,sep='-')
  parent_3 <- str_c(top_level$layer1,top_level$layer2,sep='-')
  weight_3 <-top_level$ttl
  
  
  # if(eff_type=="time"){
  # label_1 <- str_c(root_level$layer1,root_level$ttlE,sep='<br>')
  # label_2 <- str_c(bottom_level$layer2, bottom_level$ttlE,sep='<br>')
  # label_3 <- str_c(top_level$layer3,top_level$ttlE,sep='<br>')
  # }else{
    label_1 <- str_c(root_level$layer1,root_level$ttl,sep='<br>')
    label_2 <- str_c(bottom_level$layer2, bottom_level$ttl,sep='<br>')
    label_3 <- str_c(top_level$layer3,top_level$ttl,sep='<br>')
    
#  }
  
  #combine_layers
  plot_labels <- append(append(label_1,label_2),label_3)
  plot_ids <- append(append(id_1,id_2),id_3)
  plot_parents <- append(append(parent_1,parent_2),parent_3)
  plot_weightss <- append(append(weight_1,weight_2),weight_3)
  #calling the sunburst
  p<-plot_ly(ids=plot_ids,labels=plot_labels,parents=plot_parents,values=plot_weightss ,type='sunburst',branchvalues='remainder')
  return(p)
}  

JIRA_sankey <- function(df, time_period,eff_type="time"){
  ## Load data
  df %>% 
    transmute(Assignee, Creator, Labels, Status, Issue_id, Parent_id , Epic_Link, Summary, Effort,date_updated) %>% 
    filter(Status %in% c('in progress','released to b.o.','testing','done')) %>% #not counting scoping/backlog
    mutate(work_effort = str_c(gsub('([0-9]|@).*','',Assignee),Labels,sep="-")) %>%
    transmute(layer1 = Assignee , layer2 = Labels, layer3 = Summary ,date_updated, Effort = as.numeric(coalesce(Effort,"1"))) -> 
    df2
  
  #create list of distinct nodes
  #node_names <- append(unique(df2$layer1),unique(df2$layer2))
  
  #index the nodes
  #nodes <- data.frame(node = c(0:(length(node_names)-1)),name = c(node_names) )
  
  #create links
  df_p1 <- df2[(Sys.Date() - as.Date(df2$date_updated)) > time_period ,] #necessarily < 2 * X basically the previous period data
  
  #get all targets
  temp_targetlist <- unique(df2$layer2)
  
  #get all sources for the period
  temp_sourcelist <- unique(df_p1$layer1)
  
  #check all targets for hte current periog
  
  tmp_currenttargets <- temp_targetlist[!temp_targetlist %in% unique(df_p1[df_p1$layer1=='unassigned',]$layer2)]
  
  #find the targets with a source of unassigned
  
  #create the links if unassigned does not exist
  add_on_list <- data.frame(layer1=rep( 'unassigned',length(tmp_currenttargets))
                            ,layer2=tmp_currenttargets
                            ,layer3= rep( 'placeholder',length(tmp_currenttargets))
                            ,date_updated = rep (min(df_p1$date_updated), length(tmp_currenttargets))
                            ,Effort = rep(1,length(tmp_currenttargets)))
  
  df_p1 <- bind_rows(df_p1,add_on_list)
  
  node_names <- append(unique(df_p1$layer1),unique(df2$layer2))
  
  nodes <- data.frame(node = c(0:(length(node_names)-1)),name = c(node_names),stringsAsFactors = F )
  #print(nodes)
  
  #create links person -  task for period 1
  inner_join(df_p1 , nodes, by = c("layer1" = "name")) %>% 
    inner_join(nodes, by = c("layer2" = "name")) %>%
    select(layer1,layer2,Effort,source = node.x, target = node.y) -> links_1
  #View(links_1)
  
  
  
  ##################### make second period, should be label to people
  #create links, space is there to prevent loops
  df_p2 <- df2[(Sys.Date() - as.Date(df2$date_updated)) <=  time_period ,]
  node_names <- append(node_names, paste0(unique(df_p2$layer1),' '))
  
  node_x <- data.frame(node = c(dim(nodes)[1]:(dim(nodes)[1]+length(unique(df_p2$layer1))-1))
                       ,name = c(paste0(unique(df_p2$layer1),' ') ) )
  
  nodes <- rbind(nodes, node_x)
  print(node_x)
  #create links person -  task for period 1
  df_p2 %>% mutate(layer1 = paste0(layer1," ")) %>%
    inner_join(nodes, by = c("layer1" = "name" ) ) %>% 
    inner_join(nodes, by = c("layer2" = "name")) %>%
    select(layer1,layer2,Effort,source = node.y, target = node.x) -> links_2
  
  
  #create fake links for visual, this is awkward but has to do with 
  # aaa<- full_join(links_1[links_1$layer1 == 'unassigned',],unique(select(ungroup(links_1),layer2,target)),by = c('layer2'='layer2'))
  # aaa$layer1 = coalesce(aaa$layer1,'unassigned')
  # aaa$source = coalesce(aaa$source , min(aaa$source,na.rm=T))
  # aaa$Effort = coalesce(aaa$Effort, 1.0)
  # aaa$target = aaa$target.y
  # 
  # aaa %>% filter(is.na(target.x)) %>% select(layer1,layer2,source,target,Effort) -> aaa
  # links_1 <- rbind(links_1,aaa)
  # 
  
  #make the weights
  if(eff_type=='time')
    links_1 %>% group_by(layer1,layer2,source,target) %>% summarise(Effort=sum(Effort)) -> links_1
  if(eff_type=='task')
    links_1 %>% group_by(layer1,layer2,source,target) %>% summarise(Effort=n()) -> links_1
  #make the weights
  if(eff_type=='time')
    links_2 %>% group_by(layer1,layer2,source,target) %>% summarise(Effort=sum(Effort)) -> links_2
  if(eff_type=='task')
    links_2 %>% group_by(layer1,layer2,source,target) %>% summarise(Effort=n()) -> links_2
  
  #find sources in links_2 that are not targets in links_1
  #####################################################
  ## TEST
  #View(links_1)
  #View(links_2)
  
  #   mytarget <- links_2[!(links_2$source %in% links_1$target),]$source
  # 
  #     if(!is_empty(mytarget)){
  #       
  # #if the 'unassigned node doesn't exist, create one'
  #       if(!is_empty(nodes[nodes$name == 'unassigned',]$node)){
  # 
  #           mysource <- rep( (nodes[nodes$name == 'unassigned',]$node)  ,length(mytarget))
  #       }else{
  #         tt <- max(nodes$node) + 1
  #         rbind(nodes,c("unassigned",tt))
  #         mysource <- rep( tt  ,length(mytarget))
  #         
  #       }
  #         print(mysource)
  #   myeffort <- rep(1,length(mytarget))
  #   mylayer1 <- rep('toto',length(mytarget))
  #   mylayer2 <- rep('toto',length(mytarget))
  # 
  #   mytempdf <- data.frame(layer1=mylayer1
  #                          ,layer2=mylayer2
  #                          ,source=mysource
  #                          ,target=mytarget
  #                          ,Effort=myeffort
  #                          ,stringsAsFactors = F)
  #   print(mytempdf)
  # 
  #   links_1 <- bind_rows(links_1,mytempdf)
  #View(links_1)
  #  }
  ########################################
  ## END TEST
  
  ####################### create the whole stuff
  #nodes <- rbind(nodes_1, nodes_2)
  links <- bind_rows(links_1,links_2)
  #print(links)
  #print(nodes)
  #print(links,n=100)
  #############################make the sankey diagram
  if(eff_type=='time'){
    networkD3::sankeyNetwork(Links = links, Nodes = nodes, 
                             Source = 'source', 
                             Target = 'target', 
                             Value = 'Effort', 
                             NodeID = 'name',
                             units = 'hours',fontSize = 12,sinksRight = F,fontFamily = 'Helvetica')
  }else{
    networkD3::sankeyNetwork(Links = links, Nodes = nodes, 
                             Source = 'source', 
                             Target = 'target', 
                             Value = 'Effort', 
                             NodeID = 'name',
                             units = 'tasks',fontSize = 12,sinksRight = F,fontFamily = 'Helvetica')
  }

  
  
}
JIRA_data_cleanup <- function(df){
 
  #step 1: select only relevant columns
  df %>% transmute(Assignee = gsub('([0-9]|@).*','',Assignee)
                   , Creator = gsub('([0-9]|@).*','',Creator)
                   , Labels
                   , Status
                   , Epic_Name = Custom.field..Epic.Name.
                   , Issue_id = Issue.id
                   , Parent_id = Parent.id
                   , Epic_Link=Custom.field..Epic.Link.
                   , Effort=Custom.field..Story.Points.
                   , Summary
                   , Issue_key = Issue.key
#not sure how that happened, previous version were am/pm but some files are 24h???
                   , date_updated = as.Date.character(Updated,format = '%d/%b/%y %I:%M %p')
                   , date_created = as.Date.character(Created,format = '%d/%b/%y %I:%M %p')
                   , date_resolved = as.Date.character(Resolved,format = '%d/%b/%y %I:%M %p')
                   , date_viewed = as.Date.character(Last.Viewed,format = '%d/%b/%y %I:%M %p')) %>%
# 
#                   , date_updated = as.Date.character(Updated,format = '%d/%m/%Y %I:%M')
#                   , date_created = as.Date.character(Created,format = '%d/%m/%Y %I:%M')
#                   , date_resolved = as.Date.character(Resolved,format = '%d/%m/%Y %I:%M')
#                   , date_viewed = as.Date.character(Last.Viewed,format = '%d/%m/%Y %I:%M')) %>%

    mutate_all(.funs=str_to_lower) %>% #JIRA is nominally case-blind, R is not
    mutate(Epic_Link = case_when(Epic_Link=='' ~ 'no epic', is.na(Epic_Link) ~ 'inno epic', TRUE ~ as.character(Epic_Link)) #impute missing values
           ,Assignee = case_when(Assignee == '' ~ 'unassigned', is.na(Assignee) ~ 'unassigned', TRUE ~ as.character(Assignee))
           ,Labels = case_when(Labels == '' ~ 'no label', is.na(Labels) ~ 'no label', TRUE ~as.character(Labels))) -> tmp_df
  
  
  #step 2: get the proper epic link for child tasks, i.e., we'll replace the epic link of the child by the epic link of the parent
  tmp_df2 <- select(tmp_df, Issue_id, Parent_id, Epic_Link, Effort)
  tmp_df2 <- inner_join(tmp_df2,tmp_df2,by=c("Parent_id"="Issue_id")) %>%
    mutate(parent_epic_link = Epic_Link.y) 
  
  #2.2: assign the epic likn
  #tmp_df <- mutate(tmp_df, Epic_Link = ifelse(Issue_id %in% tmp_df2$Issue_id, tmp_df2$parent_epic_link, Epic_Link))
  tmp_df$Epic_Link <- coalesce(tmp_df2$parent_epic_link[match(tmp_df$Issue_id,tmp_df2$Issue_id)],tmp_df$Epic_Link)
  #step 3: re-estimate the effort of the parent task (for individual effort estimation). The Parent genuine effort field contains all non-allocated hours
  tmp_df2 %>% group_by(Parent_id) %>% 
    summarise(ttl = sum(as.numeric(Effort.x),na.rm=T),Epic_Link = first(Epic_Link.y), Parent_Effort = as.numeric(first(Effort.y)))  %>% 
    transmute (Parent_id, Parent_genuine_effort = coalesce(Parent_Effort - ttl, 1) , Epic_Link) %>% 
    mutate(toto = case_when(Parent_genuine_effort < 0.0 ~ 1.0, TRUE ~ as.numeric(Parent_genuine_effort))) -> tmp_df3
  
  tmp_df <- mutate(tmp_df, Effort = ifelse(Issue_id %in% tmp_df3$Parent_id, tmp_df3$toto, Effort))
  
  #step 4: get the name of the epics and replace them
  #tmp_df <- mutate(tmp_df, Epic_Link = ifelse(!is.na(Epic_Link), Summary, 'no epic') )
  titi<- inner_join(tmp_df,tmp_df,by = c("Epic_Link" = "Issue_key"))
 # View(titi)
  tmp_df$Epic_Link <- titi$Epic_Name.y[match(tmp_df$Issue_id,titi$Issue_id.x)]
  tmp_df <- mutate(tmp_df , Epic_Link = case_when(Epic_Link=='' ~ 'no epic', is.na(Epic_Link) ~ 'no epic', Epic_Link == 'NA' ~ 'no epic', TRUE ~ as.character(Epic_Link)))
#View(tmp_df)
  return(tmp_df)
}


shinyApp(ui, server)
