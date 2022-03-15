library(shinyjs)
library(bs4Dash)
library(DT)
library(pool)
library(shinyjs)
library(dplyr)
library(tidyverse)
library(DBI)
library(RPostgres)
library(RPostgreSQL)
library(ggcharts)
library(ggplot2)
library(echarts4r)
library(lubridate)
library(zoo)
library(janitor)
library(readr)
library(readxl)
library(stringr)
library(shinysurveys)
library(shinyvalidate)
library(bslib)
library(tibble)
library(fresh)
library(officedown)
library(officer)
# library(reshape2)
library(flextable)
library(glue)
library(shinyjs)
library(RPostgreSQL)


db <- 'mzprocava'  
host_db <- "mzprocava.c3n7ltv3naza.eu-west-1.rds.amazonaws.com"
db_port <- '5432'  
db_user <- "mzprocava"
db_password <- "GoMPROCAVA;2030"
conn <- dbConnect(RPostgres::Postgres(), dbname = db, host=host_db, port=db_port, user=db_user, password=db_password)
pool <- dbPool(RPostgres::Postgres(), dbname = db, host=host_db, port=db_port, user=db_user, password=db_password)

onStop(function() {poolClose(pool)})
thematic::thematic_shiny(font = "auto")

options(warn = 0)

function(input, output, session) {

  filtered_awpb <-reactive({
    awpb_updates <- dbGetQuery(conn, "SELECT awpb_id, descricao_da_actividade, unidades, current_year_budget_us, target, ungp, q1, q2, q3, q4, area, 
                               situacao, comentarios , npmu_actuals, mpc_actuals, gaza_actuals, inhambane_actuals, maputo_actuals, sofala_actuals, 
                               manica_actuals, tete_actuals, zambezia_actuals, nampula_actuals, cabo_delgado_actuals, niassa_actuals, urgps_actuals, 
                               urgpc_actuals, urgpn_actuals, upgpn_actuals, internal_responsible, institution, relevance, critical_path, mader FROM procava.awpb_updates")
    
    if(input$responsaveis != "Todos"){awpb_updates <- awpb_updates[awpb_updates$internal_responsible %in% input$responsaveis,]}
    if(input$mader_relavant != "Todas"){awpb_updates <- awpb_updates[awpb_updates$mader %in% input$mader_relavant,]}
    if(input$relevance != "Todas"){awpb_updates <- awpb_updates[awpb_updates$relevance %in% input$relevance,]}
    if(input$critical_activity != "Todas"){awpb_updates <- awpb_updates[awpb_updates$critical_path %in% input$critical_activity,]}
    if(input$responsible_technician != "Todos"){awpb_updates <- awpb_updates[awpb_updates$internal_responsible %in% input$responsible_technician,]}
    
    awpb_updates
  })
  
  
  
  filtered_dossiers <-reactive({
    ppdossiers <- DBI::dbGetQuery(conn, "SELECT activ_id, idpp, activity_description_pt,  method_name_pt,  basic_usd, responsible, procurement_stage, pp_comment, eo_i_submission, no_eo_i, inv_r_eo_i, 
                  close_r_eo_i, report_eo_i, no_eo_i_report, rfp_rno, rfp_no, invitation, closing, rno_eva_r, no_eva_r, 
                  full_eva_rno, full_eva_no, noita, contract_awards, negotiations, rno_contract, no_contract, signature, envelopes, 
                  contract_n, vendor_id, contract_usd, contract_completion, lot,  ifadpp_sheet, ifad_review, qualification, proc_methods,
                  non_consulting, activity_pt, lotes, is_grant, shortlisting_pp, non_consultancies, activity from fiduciary.procurement_dossiers WHERE plan2 = 'Actual'")
    if(input$dossier_responsibles !="Todos"){ppdossiers <- ppdossiers[ppdossiers$responsible %in% input$dossier_responsibles,]}
    ppdossiers
  })
  
  
  
  callback <- c(
    "var id = $(table.table().node()).closest('.datatables').attr('id');",
    "$.contextMenu({",
    "  selector: '#' + id + ' td.factor input[type=text]',",
    "  trigger: 'hover',",
    "  build: function($trigger, e){",
    "    var levels = $trigger.parent().data('levels');",
    "    if(levels === undefined){",
    "      var colindex = table.cell($trigger.parent()[0]).index().column;",
    "      levels = table.column(colindex).data().unique();",
    "    }",
    "    var options = levels.reduce(function(result, item, index, array){",
    "      result[index] = item;",
    "      return result;",
    "    }, {});",
    "    return {",
    "      autoHide: true,",
    "      items: {",
    "        dropdown: {",
    "          name: 'Edit',",
    "          type: 'select',",
    "          options: options,",
    "          selected: 0",
    "        }",
    "      },",
    "      events: {",
    "        show: function(opts){",
    "          opts.$trigger.off('blur');",
    "        },",
    "        hide: function(opts){",
    "          var $this = this;",
    "          var data = $.contextMenu.getInputValues(opts, $this.data());",
    "          var $input = opts.$trigger;",
    "          $input.val(options[data.dropdown]);",
    "          $input.trigger('change');",
    "        }",
    "      }",
    "    };",
    "  }",
    "});"
  )
  
  createdCell <- function(levels){
    if(missing(levels)){
      return("function(td, cellData, rowData, rowIndex, colIndex){}")
    }
    quotedLevels <- toString(sprintf("\"%s\"", levels))
    c(
      "function(td, cellData, rowData, rowIndex, colIndex){",
      sprintf("  $(td).attr('data-levels', '[%s]');", quotedLevels),
      "}"
    )
  }
  
#List of mandatory fields for submission
  fieldsMandatory <- c("situacao", "comentarios")
  observe({
    mandatoryFilled <-
      vapply(fieldsMandatory,
             function(x) {
               !is.null(input[[x]]) && input[[x]] != ""
             },
             logical(1))
    mandatoryFilled <- all(mandatoryFilled)
    shinyjs::toggleState(id = "submeter", condition = mandatoryFilled)
  })
  
  # Form for data entry
  entry_form <- function(button_id){
    showModal(
      modalDialog(
        div(id=("entry_form"),
            tags$head(tags$style(".modal-dialog{ width:500px}")),
            tags$head(tags$style(HTML(".shiny-split-layout > div {overflow: visible}"))),
            fluidPage(
              fluidRow(

                splitLayout(
                  cellWidths = c("500px"),
                  cellArgs = list(style = "vertical-align: top"),
                  # textInput("descricao_da_actividade", labelMandatory("Actividade"), placeholder = "")
                  textAreaInput("descricao_da_actividade", labelMandatory("Actividade"), placeholder = "", height = 55, width = "470px")
                ),

                splitLayout(
                  cellWidths = c("165px", "365px"),
                  cellArgs = list(style = "vertical-align: top"),
                  selectInput("awpb_id", labelMandatory("Código"), multiple = FALSE, choices = unique(filtered_awpb()$awpb_id)),
                  selectInput("situacao", labelMandatory("Situação"), multiple = FALSE, choices = awpb_situacoes)
                ),

                splitLayout(
                  cellWidths = c("75px", "75px", "75px", "75px", "75px", "75px"),
                  cellArgs = list(style = "vertical-align: top"),
                  numericInput('npmu_actuals',label='UNGP', value=0),
                  numericInput('mpc_actuals',label='MPC', value=0),
                  numericInput('gaza_actuals',label='Gaza', value=0),
                  numericInput('inhambane_actuals',label='Inhambane', value=0),
                  numericInput('maputo_actuals',label='Maputo', value=0),
                  numericInput('sofala_actuals',label='Sofala', value=0)),

                splitLayout(
                  cellWidths = c("75px", "75px", "75px", "75px", "75px", "75px"),
                  cellArgs = list(style = "vertical-align: top"),
                  numericInput('manica_actuals',label='Manica', value=0),
                  numericInput('tete_actuals',label='Tete', value=0),
                  numericInput('zambezia_actuals',label='zambézia', value=0),
                  numericInput('nampula_actuals',label='Nampula', value=0),
                  numericInput('cabo_delgado_actuals',label='C. Delgado', value=0),
                  numericInput('niassa_actuals',label='Niassa', value=0)
                ),

                splitLayout(
                  cellWidths = c("75px", "75px", "75px", "75px", "75px", "75px"),
                  cellArgs = list(style = "vertical-align: top"),
                  numericInput('urgps_actuals',label='UNGPS', value=0),
                  numericInput('urgpc_actuals',label='UNGPC', value=0),
                  numericInput('urgpn_actuals',label='UNGPN', value=0),
                  numericInput('upgpn_actuals',label='UPGPN', value=0),
                  numericInput('npmu_actuals',label='UNGP', value=0),
                  numericInput('ungp',label='NACIONAL', value=0)
                ),

                textAreaInput("comentarios", "Comentários", placeholder = "", height = 100, width = "500px"),
                helpText(labelMandatory(""), paste("Campo Obrigatório!")),
                actionButton("submeter_actividade", "SALVAR", class = "btn-success"),
              ),
              easyClose = TRUE
            )
        )
      )
    )
  }
  
  fieldsAll <- c("awpb_id", "descricao_da_actividade", "situacao", "ungp", "comentarios", 'npmu_actuals', 'mpc_actuals', 'gaza_actuals',
                 'inhambane_actuals', 'maputo_actuals', 'sofala_actuals', 'manica_actuals', 'tete_actuals', 'zambezia_actuals', 'nampula_actuals', 
                 'cabo_delgado_actuals', 'niassa_actuals', 'urgps_actuals', 'urgpc_actuals', 'urgpn_actuals', 'upgpn_actuals')
  #save form data into data_frame format
  # formData <- reactive({
  #   formData <- as.data.frame(awpb_id = input$awpb_id,
  #                          descricao_da_actividade = input$descricao_da_actividade,
  #                          situacao = input$situacao,
  #                          ungp = input$ungp, 
  #                          comentarios = input$comentarios,
  #                          npmu_actuals  =   input$npmu_actuals, 
  #                          mpc_actuals  =   input$mpc_actuals, 
  #                          gaza_actuals  =   input$gaza_actuals, 
  #                          inhambane_actuals  =   input$inhambane_actuals, 
  #                          maputo_actuals  =   input$maputo_actuals, 
  #                          sofala_actuals  =   input$sofala_actuals, 
  #                          manica_actuals  =   input$manica_actuals, 
  #                          tete_actuals  =   input$tete_actuals, 
  #                          zambezia_actuals  =   input$zambezia_actuals, 
  #                          nampula_actuals  =   input$nampula_actuals, 
  #                          cabo_delgado_actuals  =   input$cabo_delgado_actuals, 
  #                          niassa_actuals  =   input$niassa_actuals, 
  #                          urgps_actuals  =   input$urgps_actuals, 
  #                          urgpc_actuals  =   input$urgpc_actuals, 
  #                          urgpn_actuals  =   input$urgpn_actuals, 
  #                          upgpn_actuals  =   input$upgpn_actuals, 
  #                          
  #                          data_updated = as.character(format(Sys.Date(), format="%m-%d-%Y")),
  #                          stringsAsFactors = FALSE)
  #   return(formData)
  # })
  
  # appendData <- function(data){
  #   quary <- sqlAppendTable(pool, SQL("procava.awpb_updates"), data, row.names = FALSE)
  #   dbExecute(pool, quary)
  # }
  
  # observeEvent(input$add_button, priority = 20,{
  #   entry_form("submeter")
  # })
  
  # observeEvent(input$submit_edit, priority = 20,{
  #   appendData(formData())
  #   shinyjs::reset("entry_form")
  #   removeModal()
  # })
  
  #delete data
  # deleteData <- reactive({
  #   SQL_df <- dbReadTable(pool, "procava.awpb_updates")
  #   row_selection <- SQL_df[input$responses_table_rows_selected, "awpb_id"]
  #   quary <- lapply(row_selection, function(nr){
  #     query <- paste0('DELETE FROM procava.awpb_updates WHERE awpb_id = ', "'", row_selection, "'")
  #     # dbSendQuery(conn = conn, statement = qry)
  #     print(query)
  #     tryCatch({res <- dbSendQuery(conn, query)
  #     dbClearResult(res)},
  #     error = function(cond) {stop(paste("Database error: ", cond))})
  #     return(awpb_updates)
  #   })
  # })
  # 
  # observeEvent(input$delete_button, priority = 20,{if(length(input$responses_table_rows_selected)>=1 ){deleteData()}
  #   
  #   showModal(if(length(input$responses_table_rows_selected) < 1 ){modalDialog(title = "Warning", paste("Por favor selecione linhas." ), easyClose = TRUE)})
  # })
  
  # copyData <- reactive({
  #   SQL_df <- dbReadTable(pool, SQL("procava.awpb_updates"))
  #   row_selection <- SQL_df[input$responses_table_rows_selected, "awpb_id"] 
  #   SQL_df <- SQL_df %>% filter(row_id %in% row_selection)
  #   # SQL_df$awpb_id <- unique_id(SQL_df)
  #   
  #   quary <- sqlAppendTable(pool, SQL("procava.awpb_updates"), SQL_df, row.names = FALSE)
  #   dbExecute(pool, quary)
  # })
  
  # observeEvent(input$copy_button, priority = 20,{
  #   if(length(input$responses_table_rows_selected)>=1 ){
  #     copyData()
  #   }
  #   
  #   showModal(
  #     if(length(input$responses_table_rows_selected) < 1 ){
  #       modalDialog(
  #         title = "Warning",
  #         paste("Please select row(s)." ),easyClose = TRUE
  #       )
  #     })
  # })
  #edit data
  observeEvent(input$edit_button, priority = 20,{
    
    SQL_df <- filtered_awpb()
    
    showModal(
      if(length(input$responses_table_rows_selected) > 1 ){
        modalDialog(
          title = "Warning",
          paste("Por favor, selecione só uma linha." ), easyClose = TRUE)
      } else if(length(input$responses_table_rows_selected) < 1){
        modalDialog(
          title = "Warning",
          paste("Por favor, selecione só uma linha." ), easyClose = TRUE)
      })  
    
    if(length(input$responses_table_rows_selected) == 1){
      entry_form("entry_form")
      updateTextAreaInput(session, "descricao_da_actividade", value = SQL_df[input$responses_table_rows_selected, "descricao_da_actividade"])
      updateSelectInput(session, "situacao", selected = SQL_df[input$responses_table_rows_selected, "situacao"])
      updateSelectInput(session, "awpb_id", selected = SQL_df[input$responses_table_rows_selected, "awpb_id"])
      updateNumericInput(session, "ungp", value = SQL_df[input$responses_table_rows_selected, "ungp"])
      updateTextAreaInput(session, "comentarios", value = SQL_df[input$responses_table_rows_selected, "comentarios"])
      
      updateNumericInput(session, "npmu_actuals", value = SQL_df[input$responses_table_rows_selected, "npmu_actuals"])
      updateNumericInput(session, "mpc_actuals", value = SQL_df[input$responses_table_rows_selected, "mpc_actuals"])
      updateNumericInput(session, "gaza_actuals", value = SQL_df[input$responses_table_rows_selected, "gaza_actuals"])
      updateNumericInput(session, "inhambane_actuals", value = SQL_df[input$responses_table_rows_selected, "inhambane_actuals"])
      updateNumericInput(session, "maputo_actuals", value = SQL_df[input$responses_table_rows_selected, "maputo_actuals"])
      updateNumericInput(session, "sofala_actuals", value = SQL_df[input$responses_table_rows_selected, "sofala_actuals"])
      updateNumericInput(session, "manica_actuals", value = SQL_df[input$responses_table_rows_selected, "manica_actuals"])
      updateNumericInput(session, "tete_actuals", value = SQL_df[input$responses_table_rows_selected, "tete_actuals"])
      updateNumericInput(session, "zambezia_actuals", value = SQL_df[input$responses_table_rows_selected, "zambezia_actuals"])
      updateNumericInput(session, "nampula_actuals", value = SQL_df[input$responses_table_rows_selected, "nampula_actuals"])
      updateNumericInput(session, "cabo_delgado_actuals", value = SQL_df[input$responses_table_rows_selected, "cabo_delgado_actuals"])
      updateNumericInput(session, "niassa_actuals", value = SQL_df[input$responses_table_rows_selected, "niassa_actuals"])
      updateNumericInput(session, "urgps_actuals", value = SQL_df[input$responses_table_rows_selected, "urgps_actuals"])
      updateNumericInput(session, "urgpc_actuals", value = SQL_df[input$responses_table_rows_selected, "urgpc_actuals"])
      updateNumericInput(session, "urgpn_actuals", value = SQL_df[input$responses_table_rows_selected, "urgpn_actuals"])
      updateNumericInput(session, "upgpn_actuals", value = SQL_df[input$responses_table_rows_selected, "upgpn_actuals"])
      
    }
  })
  
  
  
  
  observeEvent(input$submeter_actividade, priority = 10, {
    
    SQL_df <- filtered_awpb()
    
    row_selection <- SQL_df[input$responses_table_rows_selected, "awpb_id"]
    
    qry = paste0("UPDATE procava.awpb_updates SET  comentarios = '", paste(input$comentarios),"', ", "ungp = ", input$ungp,", ",
                 
                 "npmu_actuals =  ", input$npmu_actuals,", ",
                 "mpc_actuals =  ", input$mpc_actuals,", ",
                 "gaza_actuals =  ", input$gaza_actuals,", ",
                 "inhambane_actuals =  ", input$inhambane_actuals,", ",
                 "maputo_actuals =  ", input$maputo_actuals,", ",
                 "sofala_actuals =  ", input$sofala_actuals,", ",
                 "manica_actuals =  ", input$manica_actuals,", ",
                 "tete_actuals =  ", input$tete_actuals,", ",
                 "zambezia_actuals =  ", input$zambezia_actuals,", ",
                 "nampula_actuals =  ", input$nampula_actuals,", ",
                 "cabo_delgado_actuals =  ", input$cabo_delgado_actuals,", ",
                 "niassa_actuals =  ", input$niassa_actuals,", ",
                 "urgps_actuals =  ", input$urgps_actuals,", ",
                 "urgpc_actuals =  ", input$urgpc_actuals,", ",
                 "urgpn_actuals =  ", input$urgpn_actuals,", ",
                 
                 "upgpn_actuals =  ", input$upgpn_actuals,", ",
                 
                 "data_updated =  '", paste(as.character(format(Sys.Date(), format="%Y-%m-%d"))),"', ",
                 
                 "situacao = '", paste(input$situacao),"' ",
                 " WHERE awpb_id = '", paste(row_selection),"'")
    
    dbGetQuery(pool, qry)
    removeModal()
    
  })

  
  # filtered_awpb <-reactive({
  #   awpb_updates <- dbGetQuery(conn, "SELECT * from procava.awpb_updates")
  #   if(input$responsaveis !="Todos"){awpb_updates <- awpb_updates[awpb_updates$internal_responsible %in% input$responsaveis,]}
  #   awpb_updates
  # })
  
  output$responses_table <- DT::renderDataTable({
    table <- filtered_awpb() %>% select(awpb_id, descricao_da_actividade, unidades, current_year_budget_us, target, ungp, q1, q2, q3, q4, area, situacao, comentarios, everything())
    names(table) <- c('Código', 'Actividade', 'Unidades','Orçamento (US$)', 'Meta','Real Nacional', 'Q1', 'Q2','Q3','Q4', 'Localização', 'Situação','Comentários', 'detailed_activity', 'activity_indicator', 'costab_code', 'costab_description', 'indicador', 'unidades', 'full_contract_budget_us', 'current_year_budget_us', 'appraisal_target', 'cumulative_actuals', 'previous_years', 'current_year_actuals', 'output_performance', 'output', 'output_target', 'output_achieved', 'performance', 'mpc', 'maputo', 'gaza', 'inhambane', 'sofala', 'manica', 'tete', 'zambezia', 'nampula', 'cd', 'niassa', 'south_region_pmu', 'centre_region_pmu', 'north_province_pmu', 'niassa_province_pmu', 'jan', 'feb', 'mar', 'apr', 'may', 'jun', 'jul', 'ago', 'sep', 'oct', 'nov', 'dec', 'q1_us', 'q2_us', 'q3_us', 'q4_us', 'area', 'categories', 'ifad_loan_us', 'ifad_grant_us', 'gcf_us', 'rpsf_a1_us', 'rpsf_a2_us', 'government_us', 'beneficiaries_us', 'financiers', 'relevance', 'mader', 'sustenta', 'institution', 'npmu_actuals', 'mpc_actuals', 'gaza_actuals', 'inha_actuals', 'maputo_actuals', 'sofala_actuals', 'manica_actuals', 'tete_actuals', 'zambezia_actuals', 'nampula_actuals', 'cabo_del_actuals', 'niassa_actuals', 'urgps_actuals', 'urgpc_actuals', 'urgpn_actuals', 'upgpn_actuals', 'totals', 'data_updated')
    datatable(table,  rownames=F, extensions = c('Buttons'), options = list(dom = 'Bfrtip', buttons = c('pageLength', 'copy', 'csv', 'excel', 'pdf', 'print'),  pagelength = 10, lengthMenu = list(c(10, 25, 100, -1), c('10', '25', '100','All')),
                                                                            columnDefs = list(list(targets = c(13:92), visible = FALSE))))%>%
      
      formatStyle('Situação', backgroundColor = styleEqual(c("Não iniciada (atrasada)", "Não iniciada (dentro do prazo)", "Iniciada (Execução < 50%)",
                                                             "Estado avançado (50% ≤ Execução < 75%)", "Quase concluída (75% ≤ Execução < 100%)",
                                                             "Concluída  (Execução ≥ 100%)"), c("#ff5334", "#bfb6b2", "#fcc60e", "#eafc0e", "#93fc0e", "#10970b"))) %>% 
      
      formatRound(c('Meta','Real Nacional', 'Q1', 'Q2','Q3','Q4'), digits = 0) %>% 
      formatStyle(columns = c(7:10),
                  backgroundColor = styleInterval(cuts =c(0,1), values = c("#f5f7f7", "none","#00f0ff")),
                  fontWeight = styleInterval(cuts = 1,values = c("normal","bold"))) %>% 
      formatCurrency('Orçamento (US$)', '') %>% 
      formatStyle(c('Unidades' ,'Situação','Comentários', 'Localização'), `text-align` = 'center')
  })
  
  ################### FILTERED DATASETS  #############################
  paao_granulado <-reactive({
    paao_granulado <- read_feather('granular_awpb_2022.feather')
    if(input$responsible_technician !="Todos"){paao_granulado <- paao_granulado[paao_granulado$responsibility %in% input$responsible_technician,]}
    paao_granulado
  })
  
  full_approved_payments <-reactive({
    full_approved_payments <- dbGetQuery(conn, "SELECT * from fiduciary.full_approved_payments")
    if(input$unidades_gestoras !="Todas"){full_approved_payments <- full_approved_payments[full_approved_payments$cost_centre %in% input$unidades_gestoras,]}
    full_approved_payments
  })

 ##################### AWPB VALUE BOXES  ######################
  output$actividades_iniciadas  <- renderValueBox({
    started <- awpb_updated %>% select(situacao, awpb_id) %>% dplyr::filter(situacao %in% c("Iniciada (Execução < 50%)", "Estado avançado (50% < Execução < 75%)", 
                                                              "Quase concluída (75% < Execução < 100%)", "Concluída  (Execução >= 100%)"))
    
    valueBox(tags$b(prettyNum(n_distinct(started$awpb_id)), big.mark=",",
                    style = "font-size: 400%;"),"Actividades iniciadas",icon = ionicon(name ="walk"), color = "indigo")
  })
  
  output$actividades_latentes   <- renderValueBox({
    started <- awpb_updated %>% select(situacao, awpb_id) %>% dplyr::filter(situacao %in% c("Não iniciada (atrasada)", "Não iniciada (dentro do prazo)"))
    
    valueBox(tags$b(prettyNum(n_distinct(started$awpb_id)), big.mark=",",
                    style = "font-size: 400%;"),"Actividades não iniciadas",icon = ionicon(name ="warning"), color = "danger")
  })
  
  output$actividades_concluidas   <- renderValueBox({
    started <- awpb_updated %>% select(situacao, awpb_id) %>% dplyr::filter(situacao %in% c("Concluída  (Execução >= 100%)"))
    
    valueBox(tags$b(prettyNum(n_distinct(started$awpb_id)), big.mark=",",
                    style = "font-size: 400%;"),"Actividades Concluídas",icon = icon("calendar-check"), color = "success")
  })
  
  
  output$taxa_conclusoes   <- renderValueBox({
    started <- awpb_updated %>% select(situacao, awpb_id) %>% dplyr::filter(situacao %in% c("Concluída  (Execução >= 100%)"))
    valor <- paste(round(n_distinct(started$awpb_id)/n_distinct(awpb_updated$awpb_id)*100, 2),"%")
    valueBox(tags$b(valor, style = "font-size: 400%;"),"Nível de Conclusão das Actividades",icon = icon("percent"), color = "info")
  })
  
  #####################  FINANCE VALUE BOXES
  output$montante_desembolsado  <- renderValueBox({
    valor <- paste0(round(sum(disbursed$contravalor_mzn)/1000000, 2))
    
    valueBox(tags$b(valor, style = "font-size: 400%;"),paste0("Milhões de MZN Desembolsados (",round(sum(disbursed$disbursed_usd)/42000000*100, 2),"%)"),icon = icon("landmark"), color = "info")
  })
  
  
  output$cumulativo_executado  <- renderValueBox({
    valor <- paste0(round(sum(paid_set$paid_ammount)/1000000, 2))
    
    valueBox(tags$b(valor, style = "font-size: 400%;"),paste0("Milhões de MZN Aplicados (",round(sum(paid_set$paid_ammount)/(42000000*64.46)*100, 2),"%)"),icon = icon("comments-dollar"), color = "success")
  })
  
  output$executado_rpsf  <- renderValueBox({
    valor_rpsf <- pagamentos_aprovados %>% dplyr::filter(pdr_financiers_en %in% c("RPSF 2nd Allocation", "RPSF 1st Allocation"))
    valor <- paste0(round(sum(valor_rpsf$paid_ammount)/1000000,2))
      
    valueBox(tags$b(valor, style = "font-size: 400%;"),paste0("Milhões de MZN do RPSF - COVID (",round(sum(valor_rpsf$paid_ammount)/(1698945*64.46)*100, 2),"%)"),icon = icon("coins"), color = "warning")
  })
  
  output$comparticipacao_governo  <- renderValueBox({
    valor_governo <- pagamentos_aprovados %>% dplyr::filter(pdr_financiers_en %in% c("Government"))
    valor <- paste0(round(sum(valor_governo$paid_ammount)/1000000,2))
    
    valueBox(tags$b(valor, style = "font-size: 400%;"),paste0("Milhões de MZN do Governo (",round(sum(valor_governo$paid_ammount)/(5453821*64.46)*100, 2),"%)"),icon = icon("flag"), color = "teal")
  })

  
  #####################  PROCUREMENT VALUE BOXES
  output$dossiers_iniciados  <- renderValueBox({
    dossiers_iniciados <- procurement_view %>% dplyr::filter(procurement_stage != "Não iniciado")
    total_dossiers <- n_distinct(procurement_view$activ_id)
    iniciados <- n_distinct(dossiers_iniciados$activ_id)
    valueBox(tags$b(iniciados, style = "font-size: 400%;"), paste0("Dossiers Iniciados (",round(iniciados/total_dossiers*100, 2),"%)"), ionicon(name ="walk"), color = "success")
  })
  
  output$dossiers_contratados  <- renderValueBox({
    dossiers_contratados <- procurement_view %>% dplyr::filter(procurement_stage %in% c("Contrato Assinado", "Contrato visado", "Contrato em Implementação", "Contrato encerrado"))
    total_dossiers <- n_distinct(procurement_view$activ_id)
    dossiers_contratados <- n_distinct(dossiers_contratados$activ_id)
    valueBox(tags$b(dossiers_contratados, style = "font-size: 400%;"), paste0("Finalizados - contrato assinado (",round(dossiers_contratados/total_dossiers*100, 2),"%)"), ionicon(name ="book"), color = "orange")
  })
  
  output$usd_em_procurement   <- renderValueBox({
    dossiers_iniciados <- procurement_view %>% dplyr::filter(procurement_stage != "Não iniciado")
    total_usd <- sum(procurement_view$basic_usd)/1000000
    usd_dossiers_iniciados <- round(sum(dossiers_iniciados$basic_usd)/1000000,2)
    valueBox(tags$b(usd_dossiers_iniciados, style = "font-size: 400%;"), paste0("Milhões USD em Dossiers Iniciados (",round(usd_dossiers_iniciados/total_usd*100, 2),"%)"), ionicon(name ="cart"), color = "lightblue")
  })
  
  output$usd_latentes   <- renderValueBox({
    dossiers_latentes <- procurement_view %>% dplyr::filter(procurement_stage == "Não iniciado")
    total_usd <- round(sum(procurement_view$basic_usd)/1000000,0)
    usd_latentes <- round(sum(dossiers_latentes$basic_usd)/1000000,2)
    valueBox(tags$b(usd_latentes, style = "font-size: 400%;"), paste0("Milhões USD em Dossiers Não Iniciados (",round(usd_latentes/total_usd*100, 2),"%)"), ionicon(name ="warning"), color = "danger")
  })
  
  
  
  #####################  CONTRACT VALUE BOXES
  output$contratos_celebrados   <- renderValueBox({
    contratos_assinados <- n_distinct(long_aggreements$contract_number)
    valueBox(tags$b(contratos_assinados, style = "font-size: 400%;"), paste0("Contratos Celebrados"), icon=icon("file"), color = "warning")
  })
  
  output$usd_contratados   <- renderValueBox({
    contratos_assinados <- round(sum(long_aggreements$revised_ammount)/1000000,2)
    valueBox(tags$b(contratos_assinados, style = "font-size: 400%;"), paste0("Milhões USD Comprometidos nos Contratos"), icon=icon("usd"), color = "orange")
  })

  output$usd_pagos   <- renderValueBox({
    valor_pago <- round(sum(long_aggreements$value_paid)/1000000,2)
    valueBox(tags$b(valor_pago, style = "font-size: 400%;"), paste0("Milhões de USD Pagos em Contratos (", round(valor_pago/(sum(long_aggreements$revised_ammount)/1000000),2),"%)"), icon=icon("money"), color = "lightblue")
  })
  
  
  output$contratos_fechados  <- renderValueBox({
    
    completed_contracts <- long_aggreements %>% filter(contract_status %in% c("Closed", "Completed"))
    completed <- n_distinct(completed_contracts$contract_number)
    n_distinct(long_aggreements$contract_number)
    
    valueBox(tags$b(completed, style = "font-size: 400%;"), paste0("Contratos fechados (", round(completed/n_distinct(long_aggreements$contract_number)*100,2),"%)"), icon=icon("vote-yea"), color = "success")
  })

  #################  AWPB CHARTS ##################
  
  output$responsaveis_estado <- renderPlot({
    chart <- awpb_updated %>% group_by(internal_responsible) %>% summarize(actividades = n_distinct(awpb_id))
    chart <- chart %>% bar_chart(internal_responsible, actividades, highlight = "Irrigation and Infrastructure Specialist")
    
    # chart <- chart %>% bar_chart(internal_responsible, actividades)
    chart <- chart + theme_void() +
      labs(x = "", y = "# de actividades planificadas", caption = "Fonte: Actualizações do PAAO @ FAR,FP - PROCAVA")+
      theme(axis.text.y = element_text(size = 10))   
    chart <- chart + geom_text(aes(label=round(actividades), hjust = 1.5), colour="white", size=3, position=position_dodge(width=0.1))
    chart
  })
  
  output$responsaveis_estado <- renderPlot({
    finalizadas <- awpb_for_summary %>% group_by(internal_responsible, simplified_status) %>% summarize(n_distinct(awpb_id)) %>% spread(simplified_status, -internal_responsible) %>% adorn_totals("col")
    finalizadas$Iniciadas <- finalizadas$Total - finalizadas$`Latente`
    finalizadas[is.na(finalizadas)] <- 0
    finalizadas$internal_responsible <- fct_reorder(finalizadas$internal_responsible, finalizadas$Total, min)
    ggplot(finalizadas, aes(x= internal_responsible, y = Total)) + geom_bar(stat= "identity", col = "#c4fb83",fill="#c4fb83") +
      geom_bar(aes(x= internal_responsible, y = Iniciadas), stat= "identity", width = 0.5,col = "#10b005",fill="#10b005") + theme_void() +
      labs(x = "", y = "# de actividades", caption = "Fonte: Actualizações do PAAO @ FAR,FP - PROCAVA")+
      theme(axis.text.y = element_text(size = 10))+
      geom_text(aes(label=round(Total), hjust = 1.5), colour="#1f5502", size=3, position=position_dodge(width=0.1))+
      coord_flip()
  })
  
  output$por_iniciar <- renderPlot({
    finalizadas <- awpb_for_summary %>% group_by(internal_responsible, simplified_status) %>% summarize(n_distinct(awpb_id)) %>% spread(simplified_status, -internal_responsible) %>% adorn_totals("col")
    finalizadas[is.na(finalizadas)] <- 0
    finalizadas$Iniciadas <- finalizadas$Total - finalizadas$Latente
    finalizadas$internal_responsible <- fct_reorder(finalizadas$internal_responsible, finalizadas$Iniciadas, min)
    
    ggplot(finalizadas, aes(x = internal_responsible, y = Iniciadas)) + geom_bar(stat= "identity", col = "#c4fb83",fill="#c4fb83") +
      geom_bar(aes(x= internal_responsible, y = Finalizada), stat= "identity", width = 0.5,col = "#10b005",fill="#10b005") + theme_void() +
      labs(x = "", y = "# de actividades", caption = "Fonte: Actualizações do PAAO @ FAR,FP - PROCAVA")+
      theme(axis.text.y = element_text(size = 10))+
      geom_text(aes(label=round(Iniciadas), hjust = 1.5), colour="#1f5502", size=3, position=position_dodge(width=0.1))+
      coord_flip()
  })
  
  output$resumo_actividades <- DT::renderDataTable({
    totais <- awpb_for_summary %>% group_by(chart_status) %>% summarize(actividades = n_distinct(awpb_id))%>%
      pivot_wider(names_from = "chart_status", values_from = "actividades", values_fill = 0)
    tt_pct <- totais
    tt_pct <- tt_pct[,1:ncol(tt_pct)]/n_distinct(awpb_for_summary$awpb_id)*100
    tt_pct$components_pt <- "(%)"
    
    completions <- awpb_for_summary %>% group_by(components_pt, componentnum_pt, chart_status) %>% summarize(actividades = n_distinct(awpb_id)) %>%
      pivot_wider(names_from = "chart_status", values_from = "actividades", values_fill = 0)
    
    C1 <- completions %>% dplyr::filter(componentnum_pt == "Componente 1") %>% adorn_totals("row") %>% mutate(components_pt = replace(components_pt, components_pt == "Total", "Componente 1")) %>% arrange(components_pt)
    C2 <- completions %>% dplyr::filter(componentnum_pt == "Componente 2") %>% adorn_totals("row") %>% mutate(components_pt = replace(components_pt, components_pt == "Total", "Componente 2"))%>% arrange(components_pt)
    C3 <- completions %>% dplyr::filter(componentnum_pt == "Componente 3") %>% adorn_totals("row") %>% mutate(components_pt = replace(components_pt, components_pt == "Total", "Componente 3"))%>% arrange(components_pt)
    C4 <- completions %>% dplyr::filter(componentnum_pt == "Componente 4") %>% adorn_totals("row") %>% mutate(components_pt = replace(components_pt, components_pt == "Total", "Componente 4"))%>% arrange(components_pt)
    
    completions <- dplyr::bind_rows(C1, C2, C3, C4, totais, tt_pct)
    completions <- completions %>% dplyr::select(-2) %>% adorn_totals("col")
    completions$components_pt[is.na(completions$components_pt)] <- "Total"
    completions$col_share <- round(completions$Total/n_distinct(awpb_for_summary$awpb_id)*100,2)
    
    datatable(completions, rownames= FALSE, extensions = 'Buttons', options = list(paging = TRUE, searching = TRUE, fixedColumns = TRUE, autoWidth = TRUE, ordering = FALSE, dom = 'Blfrtip', buttons = c('copy', 'csv', 'excel')), class = "display")

    })
  
  output$impacto_orcamental <- DT::renderDataTable({
    totais <- completions <- awpb_for_summary %>% group_by(chart_status) %>% summarize(actividades = sum(current_year_budget_us))%>%
      pivot_wider(names_from = "chart_status", values_from = "actividades", values_fill = 0)
    tt_pct <- totais
    tt_pct <- tt_pct[,1:ncol(tt_pct)]/sum(awpb_for_summary$current_year_budget_us)*100
    tt_pct$components_pt <- "(%)"
    
    completions <- awpb_for_summary %>% group_by(components_pt, componentnum_pt, chart_status) %>% summarize(actividades = sum(current_year_budget_us)) %>%
      pivot_wider(names_from = "chart_status", values_from = "actividades", values_fill = 0)
    
    C1 <- completions %>% dplyr::filter(componentnum_pt == "Componente 1") %>% adorn_totals("row") %>% mutate(components_pt = replace(components_pt, components_pt == "Total", "Componente 1")) %>% arrange(components_pt)
    C2 <- completions %>% dplyr::filter(componentnum_pt == "Componente 2") %>% adorn_totals("row") %>% mutate(components_pt = replace(components_pt, components_pt == "Total", "Componente 2"))%>% arrange(components_pt)
    C3 <- completions %>% dplyr::filter(componentnum_pt == "Componente 3") %>% adorn_totals("row") %>% mutate(components_pt = replace(components_pt, components_pt == "Total", "Componente 3"))%>% arrange(components_pt)
    C4 <- completions %>% dplyr::filter(componentnum_pt == "Componente 4") %>% adorn_totals("row") %>% mutate(components_pt = replace(components_pt, components_pt == "Total", "Componente 4"))%>% arrange(components_pt)
    
    completions <- dplyr::bind_rows(C1, C2, C3, C4, totais, tt_pct)
    completions <- completions %>% dplyr::select(-2) %>% adorn_totals("col")
    completions$components_pt[is.na(completions$components_pt)] <- "Total"
    completions$col_share <- round(completions$Total/sum(awpb_for_summary$current_year_budget_us)*100,2)
    
    datatable(completions, rownames= FALSE, extensions = 'Buttons', options = list(paging = TRUE, searching = TRUE, fixedColumns = TRUE, autoWidth = TRUE, ordering = FALSE, dom = 'Blfrtip', buttons = c('copy', 'csv', 'excel')), class = "display")
     })

  output$components_quarters <- DT::renderDataTable({
    
    components_pd <-  dbGetQuery(conn, "SELECT * FROM procava.procavacomponents_full")
    components_pdr <- components_pd %>% select(components_pt, PDR = total_cost)
    full_approved_payments <-  dbGetQuery(conn, "SELECT * FROM fiduciary.full_approved_payments")
    costabs_summaries <- read_feather("pdr_costabs.feather")
    budgeted <- read_feather('granular_awpb_2022.feather')
    pdr <- components_pd %>% select(componentnum_pt, components_pt, PDR = total_cost)
    
    components_pdr <- costabs_summaries %>% filter(class %in% c("components", "Total")) %>% select(ordering, components_pt=description_pt, component_nums, PDR = total_cost)
    budgeted <- budgeted %>% group_by(subcomponent) %>% summarize(AWPB = sum(granularusd))
    budgeted$subcomponent <- str_replace(budgeted$subcomponent, "omponent", "omponente")
    full_approved_payments$paid_ammount <-  full_approved_payments$paid_ammount/e_rate
    full_approved_payments$quarters <- quarters(full_approved_payments$submission_date)
    full_approved_payments$years <- year(full_approved_payments$submission_date)
    totais <- full_approved_payments %>% group_by(quarters) %>% summarize(montante = sum(paid_ammount))%>%
      pivot_wider(names_from = "quarters", values_from = "montante", values_fill = 0)
    completions <- full_approved_payments %>% group_by(components_pt, quarters) %>% summarize(montante = sum(paid_ammount)) %>%
      pivot_wider(names_from = "quarters", values_from = "montante", values_fill = 0) %>% adorn_totals("col")
    completions <- merge(completions, budgeted, by.x = "components_pt",by.y = "subcomponent", all = TRUE)
    completions <- merge(completions, pdr, by = "components_pt", all = TRUE)
    completions[is.na(completions)] <- 0
    C1 <- completions %>% dplyr::filter(componentnum_pt == "Componente 1") %>% adorn_totals("row", name = "Component 1") %>% arrange(components_pt)
    C2 <- completions %>% dplyr::filter(componentnum_pt == "Componente 2") %>% adorn_totals("row", name = "Component 2")%>% arrange(components_pt)
    C3 <- completions %>% dplyr::filter(componentnum_pt == "Componente 3") %>% adorn_totals("row", name = "Component 3")%>% arrange(components_pt)
    C4 <- completions %>% dplyr::filter(componentnum_pt == "Componente 4") %>% adorn_totals("row", name = "Component 4")%>% arrange(components_pt)
    completions <- dplyr::bind_rows(C1, C2, C3, C4) %>% dplyr::filter(!is.na(PDR))
    TT <- completions %>% filter(componentnum_pt=="-")
    TT <- add_row(TT, summarize(TT, across(where(is.numeric), ~ sum(., na.rm = T)))) %>% dplyr::filter(is.na(components_pt))
    TT$components_pt[is.na(TT$components_pt)] <- "Total"
    completions <- dplyr::bind_rows(completions, TT) %>% mutate(awpb_pct = Total/AWPB*100, pdr_pct = Total/PDR*100) %>% dplyr::select(-componentnum_pt)
    completion <-completions  %>% relocate(awpb_pct, .after = AWPB)
    
    datatable(completion, rownames= FALSE, extensions = 'Buttons', options = list(paging = TRUE, searching = TRUE, fixedColumns = TRUE, autoWidth = TRUE, ordering = FALSE, dom = 'Blfrtip', buttons = c('copy', 'csv', 'excel')), class = "display") %>% 
    formatCurrency(2:ncol(completions), '')
  })
  
  output$category_quarters <- DT::renderDataTable({
    
    budgeted <- read_feather('granular_awpb_2022.feather')
    costabs_summaries <- read_feather("pdr_costabs.feather")
    full_approved_payments <-  dbGetQuery(conn, "SELECT * FROM fiduciary.full_approved_payments")
    full_approved_payments$quarters <- quarters(full_approved_payments$submission_date)
    full_approved_payments$years <- year(full_approved_payments$submission_date)
    categories_quarters <- full_approved_payments %>% group_by(pdr_category, quarters) %>% summarise(sum(paid_ammount)/e_rate) %>% spread(quarters, -pdr_category) %>% adorn_totals("col")
    pdr_categories <- costabs_summaries %>% dplyr::filter(class == "categories") %>% dplyr::select(component_nums, Description, PDR = total_cost)
    budget_categories <- budgeted %>% group_by(pdr_category) %>% summarise(AWPB = sum(granularusd))
    pdr_awpb <- merge(categories_quarters, budget_categories, by.x = "pdr_category", by.y = "pdr_category", all = TRUE)
    pdr_awpb_pdr <- merge(pdr_awpb, pdr_categories, by.x = "pdr_category", by.y = "Description", all = TRUE)
    pdr_awpb_pdr[is.na(pdr_awpb_pdr)] <- 0
    pdr_awpb_pdr <- pdr_awpb_pdr %>% adorn_totals("row")
    pdr_awpb_pdr <- pdr_awpb_pdr %>% mutate(AWPB_pct = Total/AWPB*100,
                                            PDR_pct = Total/PDR*100) %>% select(-pdr_category) %>% select(component_nums, everything())
    pdr_awpb_pdr$component_nums[pdr_awpb_pdr$component_nums == "-"] <- "Total"
    pdr_awpb_pdr <-pdr_awpb_pdr  %>% relocate(AWPB_pct, .after = AWPB) %>% relocate(PDR_pct, .after = PDR)
    
    datatable(pdr_awpb_pdr, rownames= FALSE, extensions = 'Buttons', options = list(paging = TRUE, searching = TRUE, fixedColumns = TRUE, autoWidth = TRUE, ordering = FALSE, dom = 'Blfrtip', buttons = c('copy', 'csv', 'excel')), class = "display") %>% 
      formatCurrency(2:ncol(pdr_awpb_pdr), '')
    
  })
  
  output$financiers_quarters <- DT::renderDataTable({
    
    budgeted <- read_feather('granular_awpb_2022.feather')
    costabs_summaries <- read_feather("pdr_costabs.feather")
    full_approved_payments <-  dbGetQuery(conn, "SELECT * FROM fiduciary.full_approved_payments")
    full_approved_payments$quarters <- quarters(full_approved_payments$submission_date)
    full_approved_payments$years <- year(full_approved_payments$submission_date)
    categories_quarters <- full_approved_payments %>% group_by(pdr_financiers_en, quarters) %>% summarise(sum(paid_ammount)/e_rate) %>% spread(quarters, -pdr_financiers_en) %>% adorn_totals("col")
    pdr_categories <- costabs_summaries %>% dplyr::filter(class == "financiers") %>% dplyr::select(component_nums, Description, PDR = total_cost)
    budget_categories <- budgeted %>% group_by(financiers) %>% summarise(AWPB = sum(granularusd))
    pdr_awpb <- merge(categories_quarters, budget_categories, by.x = "pdr_financiers_en", by.y = "financiers", all = TRUE)
    pdr_awpb_pdr <- merge(pdr_awpb, pdr_categories, by.x = "pdr_financiers_en", by.y = "Description", all = TRUE)
    pdr_awpb_pdr[is.na(pdr_awpb_pdr)] <- 0
    pdr_awpb_pdr <- pdr_awpb_pdr %>% adorn_totals("row")
    pdr_awpb_pdr <- pdr_awpb_pdr %>% mutate(AWPB_pct = Total/AWPB*100,
                                            PDR_pct = Total/PDR*100) %>% select(-pdr_financiers_en) %>% select(component_nums, everything())
    pdr_awpb_pdr$component_nums[pdr_awpb_pdr$component_nums == "-"] <- "Total"
    pdr_awpb_pdr <-pdr_awpb_pdr  %>% relocate(AWPB_pct, .after = AWPB) %>% relocate(PDR_pct, .after = PDR)
    
    datatable(pdr_awpb_pdr, rownames= FALSE, extensions = 'Buttons', options = list(paging = TRUE, searching = TRUE, fixedColumns = TRUE, autoWidth = TRUE, ordering = FALSE, dom = 'Blfrtip', buttons = c('copy', 'csv', 'excel')), class = "display") %>% 
      formatCurrency(2:ncol(pdr_awpb_pdr), '')
    # datatable(completions, rownames=F, extensions = 'Buttons', options = list(lengthMenu = c(5, 30, 50), pageLength = 5, paging = TRUE, searching = TRUE, fixedColumns = TRUE, autoWidth = TRUE, ordering = TRUE, dom = 'Bfrtip', buttons = c('copy', 'csv', 'excel')), class = "display")
  })
  
  output$execucao_ced <- DT::renderDataTable({
    
    budgeted <- read_feather('granular_awpb_2022.feather')
    full_approved_payments <-  dbGetQuery(conn, "SELECT * FROM fiduciary.full_approved_payments")
    e_sistafe <-  dbGetQuery(conn, "SELECT ced, e_sistafe_pt FROM fiduciary.esistafe_ced")
    full_approved_payments$ced <- ifelse(is.na(full_approved_payments$ced), parse_number(full_approved_payments$e_sistafe_w_code), full_approved_payments$ced)
    full_approved_payments$quarters <- quarters(full_approved_payments$submission_date)
    full_approved_payments$years <- year(full_approved_payments$submission_date)
    categories_quarters <- full_approved_payments %>% group_by(ced, quarters) %>% 
      summarise(montante = sum(paid_ammount)/e_rate) %>% 
      pivot_wider(names_from = "quarters", values_from = "montante", values_fill = 0) %>% adorn_totals("col")
    budget_categories <- budgeted %>% group_by(ced) %>% summarise(AWPB = sum(granularusd))
    pdr_awpb <- merge(categories_quarters, budget_categories, by = "ced", all=TRUE)
    pdr_awpb <- merge(pdr_awpb, e_sistafe, by = "ced")
    pdr_awpb[is.na(pdr_awpb)] <- 0
    pdr_awpb <- pdr_awpb %>% adorn_totals("row")
    pdr_awpb <- pdr_awpb %>% mutate(AWPB_pct = Total/AWPB*100) %>% select(ced, e_sistafe_pt, everything())
    datatable(pdr_awpb, rownames= FALSE, extensions = 'Buttons', options = list(paging = TRUE, searching = TRUE, fixedColumns = TRUE, autoWidth = TRUE, ordering = FALSE, dom = 'Blfrtip', buttons = c('copy', 'csv', 'excel')), class = "display") %>% 
      formatCurrency(3:ncol(pdr_awpb), '')
  })
  
  pp_dataset <-  DBI::dbGetQuery(conn, "SELECT * from fiduciary.procurement_view") %>% filter(plan2 == "Actual")
  
  output$situacao_pp_responsaveis <- renderPlot({
    responsible_status <- pp_dataset %>% group_by(responsible, simplexstage_en) %>% summarize(sum(basic_usd)/1000) %>% spread(simplexstage_en, -responsible) %>% adorn_totals("col")
    responsible_status[is.na(responsible_status)] <- 0
    responsible_status$Iniciados <- responsible_status$Total - responsible_status$`Latente`
    
    responsible_status$responsible <- fct_reorder(responsible_status$responsible, responsible_status$Total, min)
    
    ggplot(responsible_status, aes(x= responsible, y = Total)) + geom_bar(stat= "identity", col = "#c4fb83",fill="#c4fb83") +
      geom_bar(aes(x= responsible, y = Iniciados), stat= "identity", width = 0.5,col = "#10b005",fill="#10b005") + theme_void() +
      labs(x = "", y = "US$ em licitação", caption = "Fonte: Actualizações do PP @ FAR,FP - PROCAVA")+
      theme(axis.text.y = element_text(size = 10))+
      geom_text(aes(label=round(Total), hjust = 1.5), colour="#1f5502", size=3, position=position_dodge(width=0.1))+
      coord_flip()
  })
  
  output$dossiers_latentes_responsavel <- renderPlot({
    responsible_status <- pp_dataset %>% group_by(responsible, simplexstage_en) %>% summarize(sum(basic_usd)/1000) %>% spread(simplexstage_en, -responsible) %>% adorn_totals("col")
    responsible_status[is.na(responsible_status)] <- 0
    
    responsible_status$responsible <- fct_reorder(responsible_status$responsible, responsible_status$Total, min)
    
    ggplot(responsible_status, aes(x= responsible, y = Total)) + geom_bar(stat= "identity", col = "#fea471",fill="#fea471") +
      geom_bar(aes(x= responsible, y = Latente), stat= "identity", width = 0.5,col = "#f73622", fill = "#f73622") + theme_void() +
      labs(x = "", y = "US$ em licitação", caption = "Fonte: Actualizações do PP @ FAR,FP - PROCAVA")+
      theme(axis.text.y = element_text(size = 10))+
      geom_text(aes(label=round(Total), hjust = 1.5), colour="#1f5502", size=3, position=position_dodge(width=0.1))+
      coord_flip()
  })
  
  
  output$dossiers_methods <- DT::renderDataTable({
    
    categories_pp <- pp_dataset  %>% group_by(proc_methods, ifadpp_sheet, simplexstage_en) %>% summarize(montante = n_distinct(activ_id)) %>% pivot_wider(names_from = simplexstage_en, values_from = montante)
    ctots  <-   pp_dataset  %>% group_by(ifadpp_sheet, simplexstage_en) %>% summarize(montante = n_distinct(activ_id)) %>%
      pivot_wider(names_from = simplexstage_en, values_from = montante) %>% adorn_totals("row") %>% dplyr::filter(ifadpp_sheet== "Total")
    categories_pp  <- categories_pp  %>% dplyr::arrange(ifadpp_sheet) %>% split( .[,"ifadpp_sheet"] ) %>% purrr::map_df(., janitor::adorn_totals)
    categories_pp $proc_methods <- ifelse(categories_pp $ifadpp_sheet == "-", lag(categories_pp $ifadpp_sheet),  categories_pp $proc_methods)
    categories_pp $ifadpp_sheet <- ifelse(categories_pp $ifadpp_sheet == "-", categories_pp $proc_methods,  categories_pp $ifadpp_sheet)
    categories_pp$proc_methods[categories_pp$proc_methods == ""] <- "Outros"
    categories_pp  <- categories_pp  %>% arrange(ifadpp_sheet, proc_methods)
    ctots_pct <- ctots[, -1]/n_distinct(pp_dataset$activ_id)*100
    categories_pp  <- dplyr::bind_rows(categories_pp , ctots, ctots_pct) %>% adorn_totals("col")
    categories_pp $proc_methods[is.na(categories_pp $proc_methods)] <- "Total"
    categories_pp_activities  <- categories_pp  %>% select(-ifadpp_sheet)
    
    setnames(categories_pp_activities, "proc_methods", "Categorias e Métodos")
    datatable(categories_pp_activities, rownames = FALSE, extensions = 'Buttons', options = list(paging = TRUE, searching = TRUE, fixedColumns = TRUE, autoWidth = TRUE, ordering = FALSE, dom = 'Blfrtip', buttons = c('copy', 'csv', 'excel')), class = "display") %>% 
      formatCurrency(2:ncol(categories_pp_activities), '')
  })
  
  
  output$dossiers_components <- DT::renderDataTable({
    
    components_pp <- pp_dataset  %>% group_by(components_pt, componentnum_pt, simplexstage_en) %>% summarize(montante = n_distinct(activ_id)) %>% pivot_wider(names_from = simplexstage_en, values_from = montante)
    ctots  <-   pp_dataset  %>% group_by(componentnum_pt, simplexstage_en) %>% summarize(montante = n_distinct(activ_id)) %>%
      pivot_wider(names_from = simplexstage_en, values_from = montante) %>% adorn_totals("row") %>% dplyr::filter(componentnum_pt== "Total")
    components_pp <- components_pp %>% dplyr::arrange(componentnum_pt) %>% split( .[,"componentnum_pt"] ) %>% purrr::map_df(., janitor::adorn_totals)
    components_pp$components_pt <- ifelse(components_pp$componentnum_pt == "-", lag(components_pp$componentnum_pt),  components_pp$components_pt)
    components_pp$componentnum_pt <- ifelse(components_pp$componentnum_pt == "-", components_pp$components_pt,  components_pp$componentnum_pt)
    components_pp <- components_pp %>% arrange(componentnum_pt, components_pt)
    ctots_pct <- ctots[, -1]/n_distinct(pp_dataset$activ_id)*100
    components_pp <- dplyr::bind_rows(components_pp, ctots, ctots_pct) %>% adorn_totals("col")
    components_pp$components_pt[is.na(components_pp$components_pt)] <- "Total"
    components_pp_activities <- components_pp %>% select(-componentnum_pt) 
    setnames(components_pp_activities, "components_pt", "Componentes")
    datatable(components_pp, rownames = FALSE, extensions = 'Buttons', options = list(paging = TRUE, searching = TRUE, fixedColumns = TRUE, autoWidth = TRUE, ordering = FALSE, dom = 'Blfrtip', buttons = c('copy', 'csv', 'excel')), class = "display") %>% 
      formatCurrency(2:ncol(categories_pp_activities), '')
  })
  
  
  output$dossiers_components_usd <- DT::renderDataTable({
    
    components_pp <- pp_dataset  %>% group_by(components_pt, componentnum_pt, simplexstage_en) %>% summarize(montante = sum(basic_usd)) %>% pivot_wider(names_from = simplexstage_en, values_from = montante)
    ctots  <-   pp_dataset  %>% group_by(componentnum_pt, simplexstage_en) %>% summarize(montante = sum(basic_usd)) %>%
      pivot_wider(names_from = simplexstage_en, values_from = montante) %>% adorn_totals("row") %>% dplyr::filter(componentnum_pt== "Total")
    components_pp <- components_pp %>% dplyr::arrange(componentnum_pt) %>% split( .[,"componentnum_pt"] ) %>% purrr::map_df(., janitor::adorn_totals)
    components_pp$components_pt <- ifelse(components_pp$componentnum_pt == "-", lag(components_pp$componentnum_pt),  components_pp$components_pt)
    components_pp$componentnum_pt <- ifelse(components_pp$componentnum_pt == "-", components_pp$components_pt,  components_pp$componentnum_pt)
    components_pp <- components_pp %>% arrange(componentnum_pt, components_pt)
    ctots_pct <- ctots[, -1]/sum(pp_dataset$basic_usd)*100
    components_pp <- dplyr::bind_rows(components_pp, ctots, ctots_pct) %>% adorn_totals("col")
    components_pp$components_pt[is.na(components_pp$components_pt)] <- "Total"
    components_pp_value <- components_pp %>% select(-componentnum_pt) 
    setnames(components_pp_value, "components_pt", "Componentes")
    datatable(components_pp_value, rownames = FALSE, extensions = 'Buttons', options = list(paging = TRUE, searching = TRUE, fixedColumns = TRUE, autoWidth = TRUE, ordering = FALSE, dom = 'Blfrtip', buttons = c('copy', 'csv', 'excel')), class = "display") %>% 
      formatCurrency(2:ncol(categories_pp_activities), '')
  })
  
  
  output$dossiers_methods_usd <- DT::renderDataTable({
    categories_pp <- pp_dataset  %>% group_by(proc_methods, ifadpp_sheet, simplexstage_en) %>% summarize(montante = sum(basic_usd)) %>% pivot_wider(names_from = simplexstage_en, values_from = montante)
    ctots  <-   pp_dataset  %>% group_by(ifadpp_sheet, simplexstage_en) %>% summarize(montante = sum(basic_usd)) %>%
      pivot_wider(names_from = simplexstage_en, values_from = montante) %>% adorn_totals("row") %>% dplyr::filter(ifadpp_sheet== "Total")
    categories_pp  <- categories_pp  %>% dplyr::arrange(ifadpp_sheet) %>% split( .[,"ifadpp_sheet"] ) %>% purrr::map_df(., janitor::adorn_totals)
    categories_pp $proc_methods <- ifelse(categories_pp $ifadpp_sheet == "-", lag(categories_pp $ifadpp_sheet),  categories_pp $proc_methods)
    categories_pp $ifadpp_sheet <- ifelse(categories_pp $ifadpp_sheet == "-", categories_pp $proc_methods,  categories_pp $ifadpp_sheet)
    categories_pp$proc_methods[categories_pp$proc_methods == ""] <- "Outros"
    categories_pp  <- categories_pp  %>% arrange(ifadpp_sheet, proc_methods)
    ctots_pct <- ctots[, -1]/sum(pp_dataset$basic_usd)*100
    categories_pp  <- dplyr::bind_rows(categories_pp , ctots, ctots_pct) %>% adorn_totals("col")
    categories_pp $proc_methods[is.na(categories_pp $proc_methods)] <- "Total"
    categories_pp_value  <- categories_pp  %>% select(-ifadpp_sheet)
    setnames(categories_pp_value, "proc_methods", "Categorias e Métodos")

    datatable(categories_pp_value, rownames = FALSE, extensions = 'Buttons', options = list(paging = TRUE, searching = TRUE, fixedColumns = TRUE, autoWidth = TRUE, ordering = FALSE, dom = 'Blfrtip', buttons = c('copy', 'csv', 'excel')), class = "display") %>% 
      formatCurrency(2:ncol(categories_pp_activities), '')
  })
  
  
  # awpb_updates <- reactive({
  #   input$submit_edit
  #   dbReadTable(pool, SQL("procava.awpb_updates"))
  #   
  # })  
  
  #List of mandatory fields for submission
  fieldsMandatory <- c("situacao", "comentarios")
  observe({
    mandatoryFilled <-
      vapply(fieldsMandatory,
             function(x) {
               !is.null(input[[x]]) && input[[x]] != ""
             },
             logical(1))
    mandatoryFilled <- all(mandatoryFilled)
    shinyjs::toggleState(id = "update_awpb", condition = mandatoryFilled)
  })
  
  # Form for data entry
  entry_form <- function(button_id){
    showModal(
      modalDialog(
        div(id=("entry_form"),
            tags$head(tags$style(".modal-dialog{ width:500px}")),
            tags$head(tags$style(HTML(".shiny-split-layout > div {overflow: visible}"))),
            fluidPage(
              fluidRow(

                splitLayout(
                  cellWidths = c("500px"),
                  cellArgs = list(style = "vertical-align: top"),
                  # textInput("descricao_da_actividade", labelMandatory("Actividade"), placeholder = "")
                  textAreaInput("descricao_da_actividade", labelMandatory("Actividade"), placeholder = "", height = 55, width = "470px")),

                splitLayout(
                  cellWidths = c("165px", "365px"),
                  cellArgs = list(style = "vertical-align: top"),
                  selectInput("awpb_id", labelMandatory("Código"), multiple = FALSE, choices = as.character(unique(awpb_updated$awpb_id))),
                  selectInput("situacao", labelMandatory("Situação"), multiple = FALSE, choices = awpb_situacoes)),

                splitLayout(
                  cellWidths = c("75px", "75px", "75px", "75px", "75px", "75px"),
                  cellArgs = list(style = "vertical-align: top"),
                  numericInput('npmu_actuals',label='UNGP', value=0),
                  numericInput('mpc_actuals',label='MPC', value=0),
                  numericInput('gaza_actuals',label='Gaza', value=0),
                  numericInput('inhambane_actuals',label='Inhambane', value=0),
                  numericInput('maputo_actuals',label='Maputo', value=0),
                  numericInput('sofala_actuals',label='Sofala', value=0)),

                splitLayout(
                  cellWidths = c("75px", "75px", "75px", "75px", "75px", "75px"),
                  cellArgs = list(style = "vertical-align: top"),
                  numericInput('manica_actuals',label='Manica', value=0),
                  numericInput('tete_actuals',label='Tete', value=0),
                  numericInput('zambezia_actuals',label='zambézia', value=0),
                  numericInput('nampula_actuals',label='Nampula', value=0),
                  numericInput('cabo_delgado_actuals',label='C. Delgado', value=0),
                  numericInput('niassa_actuals',label='Niassa', value=0)),

                splitLayout(
                  cellWidths = c("75px", "75px", "75px", "75px", "75px", "75px"),
                  cellArgs = list(style = "vertical-align: top"),
                  numericInput('urgps_actuals',label='UNGPS', value=0),
                  numericInput('urgpc_actuals',label='UNGPC', value=0),
                  numericInput('urgpn_actuals',label='UNGPN', value=0),
                  numericInput('upgpn_actuals',label='UPGPN', value=0),
                  numericInput('npmu_actuals',label='UNGP', value=0),
                  numericInput('ungp',label='NACIONAL', value=0)),

                textAreaInput("comentarios", "Comentários", placeholder = "", height = 100, width = "500px"),
                helpText(labelMandatory(""), paste("Campo Obrigatório!")),
                actionButton("submeter_actividade", "SALVAR", class = "btn-success"),
              ),
              easyClose = TRUE
            )
        )
      )
    )
  }
  
  fieldsAll <- c("awpb_id", "descricao_da_actividade", "situacao", "ungp", "comentarios", 'npmu_actuals', 'mpc_actuals', 'gaza_actuals',
                 'inhambane_actuals', 'maputo_actuals', 'sofala_actuals', 'manica_actuals', 'tete_actuals', 'zambezia_actuals', 'nampula_actuals', 
                 'cabo_delgado_actuals', 'niassa_actuals', 'urgps_actuals', 'urgpc_actuals', 'urgpn_actuals', 'upgpn_actuals')
  #save form data into data_frame format
  # formData <- reactive({
  #   formData <- as.data.frame(awpb_id = input$awpb_id,
  #                          descricao_da_actividade = input$descricao_da_actividade,
  #                          situacao = input$situacao,
  #                          ungp = input$ungp, 
  #                          comentarios = input$comentarios,
  #                          npmu_actuals  =   input$npmu_actuals, 
  #                          mpc_actuals  =   input$mpc_actuals, 
  #                          gaza_actuals  =   input$gaza_actuals, 
  #                          inhambane_actuals  =   input$inhambane_actuals, 
  #                          maputo_actuals  =   input$maputo_actuals, 
  #                          sofala_actuals  =   input$sofala_actuals, 
  #                          manica_actuals  =   input$manica_actuals, 
  #                          tete_actuals  =   input$tete_actuals, 
  #                          zambezia_actuals  =   input$zambezia_actuals, 
  #                          nampula_actuals  =   input$nampula_actuals, 
  #                          cabo_delgado_actuals  =   input$cabo_delgado_actuals, 
  #                          niassa_actuals  =   input$niassa_actuals, 
  #                          urgps_actuals  =   input$urgps_actuals, 
  #                          urgpc_actuals  =   input$urgpc_actuals, 
  #                          urgpn_actuals  =   input$urgpn_actuals, 
  #                          upgpn_actuals  =   input$upgpn_actuals, 
  #                          
  #                          data_updated = as.character(format(Sys.Date(), format="%m-%d-%Y")),
  #                          stringsAsFactors = FALSE)
  #   return(formData)
  # })
  
  observeEvent(input$edit_button, priority = 20,{
    
    SQL_df <- filtered_awpb()
    
    showModal(
      if(length(input$responses_table_rows_selected) > 1 ){
        modalDialog(
          title = "Warning",
          paste("Por favor, selecione só uma linha." ),easyClose = TRUE)
      } else if(length(input$responses_table_rows_selected) < 1){
        modalDialog(
          title = "Warning",
          paste("Por favor, selecione só uma linha." ),easyClose = TRUE)
      })  
    
    if(length(input$responses_table_rows_selected) == 1 ){
      entry_form("entry_form")
      updateTextAreaInput(session, "descricao_da_actividade", value = SQL_df[input$responses_table_rows_selected, "descricao_da_actividade"])
      updateSelectInput(session, "situacao", selected = SQL_df[input$responses_table_rows_selected, "situacao"])
      updateSelectInput(session, "awpb_id", selected = SQL_df[input$responses_table_rows_selected, "awpb_id"])
      updateNumericInput(session, "ungp", value = SQL_df[input$responses_table_rows_selected, "ungp"])
      updateTextAreaInput(session, "comentarios", value = SQL_df[input$responses_table_rows_selected, "comentarios"])
      updateNumericInput(session, "npmu_actuals", value = SQL_df[input$responses_table_rows_selected, "npmu_actuals"])
      updateNumericInput(session, "mpc_actuals", value = SQL_df[input$responses_table_rows_selected, "mpc_actuals"])
      updateNumericInput(session, "gaza_actuals", value = SQL_df[input$responses_table_rows_selected, "gaza_actuals"])
      updateNumericInput(session, "inhambane_actuals", value = SQL_df[input$responses_table_rows_selected, "inhambane_actuals"])
      updateNumericInput(session, "maputo_actuals", value = SQL_df[input$responses_table_rows_selected, "maputo_actuals"])
      updateNumericInput(session, "sofala_actuals", value = SQL_df[input$responses_table_rows_selected, "sofala_actuals"])
      updateNumericInput(session, "manica_actuals", value = SQL_df[input$responses_table_rows_selected, "manica_actuals"])
      updateNumericInput(session, "tete_actuals", value = SQL_df[input$responses_table_rows_selected, "tete_actuals"])
      updateNumericInput(session, "zambezia_actuals", value = SQL_df[input$responses_table_rows_selected, "zambezia_actuals"])
      updateNumericInput(session, "nampula_actuals", value = SQL_df[input$responses_table_rows_selected, "nampula_actuals"])
      updateNumericInput(session, "cabo_delgado_actuals", value = SQL_df[input$responses_table_rows_selected, "cabo_delgado_actuals"])
      updateNumericInput(session, "niassa_actuals", value = SQL_df[input$responses_table_rows_selected, "niassa_actuals"])
      updateNumericInput(session, "urgps_actuals", value = SQL_df[input$responses_table_rows_selected, "urgps_actuals"])
      updateNumericInput(session, "urgpc_actuals", value = SQL_df[input$responses_table_rows_selected, "urgpc_actuals"])
      updateNumericInput(session, "urgpn_actuals", value = SQL_df[input$responses_table_rows_selected, "urgpn_actuals"])
      updateNumericInput(session, "upgpn_actuals", value = SQL_df[input$responses_table_rows_selected, "upgpn_actuals"])
    }
  })
  
  # observeEvent(input$update_awpb, priority = 20, {
  #   # SQL_df <- filtered_awpb()
  #   row_selection <- filtered_awpb()[input$responses_table_rows_selected, "awpb_id"]
  #   
  #   qry = paste0("UPDATE procava.awpb_updates SET  comentarios = '", paste(input$comentarios),"', ", "ungp = ", input$ungp,", ",
  #                
  #                "npmu_actuals =  ", input$npmu_actuals,", ",
  #                "mpc_actuals =  ", input$mpc_actuals,", ",
  #                "gaza_actuals =  ", input$gaza_actuals,", ",
  #                "inhambane_actuals =  ", input$inhambane_actuals,", ",
  #                "maputo_actuals =  ", input$maputo_actuals,", ",
  #                "sofala_actuals =  ", input$sofala_actuals,", ",
  #                "manica_actuals =  ", input$manica_actuals,", ",
  #                "tete_actuals =  ", input$tete_actuals,", ",
  #                "zambezia_actuals =  ", input$zambezia_actuals,", ",
  #                "nampula_actuals =  ", input$nampula_actuals,", ",
  #                "cabo_delgado_actuals =  ", input$cabo_delgado_actuals,", ",
  #                "niassa_actuals =  ", input$niassa_actuals,", ",
  #                "urgps_actuals =  ", input$urgps_actuals,", ",
  #                "urgpc_actuals =  ", input$urgpc_actuals,", ",
  #                "urgpn_actuals =  ", input$urgpn_actuals,", ",
  #                
  #                "upgpn_actuals =  ", input$upgpn_actuals,", ",
  #                
  #                "data_updated =  '", paste(as.character(format(Sys.Date(), format="%Y-%m-%d"))),"', ",
  #                
  #                "situacao = '", paste(input$situacao),"' ",
  #                " WHERE awpb_id = '", paste0(row_selection),"'")
  #   
  #   dbSendQuery(conn = conn, statement = qry)
  #   showModal(modalDialog(title=paste0("PARABÉNS! Activdade actualizada!"), br(), div(tags$b(paste0("SUCESSO!"), style = "color: green;"))))
  #   removeModal()
  #   
  # })


  
  output$responses_table <- DT::renderDataTable({
    table <- filtered_awpb()
    # %>% arrange(awpb_id)
    names(table) <- c('Código', 'Descrição', 'Unidades', 'Orçamento (US$)', 'Meta', 'Real Nacional', 'Q1', 'Q2', 'Q3', 'Q4', 'Localização', 'Situação', 'Comentários', 'Real UNGP', 'Real MPC', 'Real Gaza', 'Real Inhambane', 'Real Maputo', 'Real Sofala', 'Real Manica', 'Real Tete', 'Real Zambézia', 'Real Nampula', 'Real Cabo Delgado', 'Real Niassa', 'Real URGPS', 'Real URGPC', 'Real URGPN', 'Real UPGPN', 'Responsável', 'Instituição', 'Relevãncia', 'Caminho Crítico')
    datatable(table,  rownames=F, extensions = c('Buttons'), options = list(dom = 'Bfrtip', buttons = c('pageLength', 'copy', 'csv', 'excel', 'pdf', 'print'),  pagelength = 10, lengthMenu = list(c(10, 25, 100, -1), c('10', '25', '100','All')),
                                                                            columnDefs = list(list(targets = c(14:33), visible = FALSE))))%>%
      
      formatStyle('Situação', backgroundColor = styleEqual(c("Não iniciada (atrasada)", "Não iniciada (dentro do prazo)", "Iniciada (Execução < 50%)",
                                                             "Estado avançado (50% ≤ Execução < 75%)", "Quase concluída (75% ≤ Execução < situacao_pp_responsaveis)",
                                                             "Concluída  (Execução ≥ 100%)"), c("#ff5334", "#bfb6b2", "#fcc60e", "#eafc0e", "#93fc0e", "#10970b"))) %>% 
      
      formatRound(c('Meta','Real Nacional', 'Q1', 'Q2','Q3','Q4'), digits = 0) %>% 
      formatStyle(columns = c(7:10),
                  backgroundColor = styleInterval(cuts =c(0,1), values = c("#f5f7f7", "none","#00f0ff")),
                  fontWeight = styleInterval(cuts = 1,values = c("normal","bold"))) %>% 
      
      formatCurrency('Orçamento (US$)', '') %>% 
      formatStyle(c('Unidades' ,'Situação','Comentários', 'Localização'), `text-align` = 'center')
  })
  
  ############# FINANCE OUTPUTS #################
  output$expense_timeline <- renderEcharts4r({
    
    pagamentos <- dbGetQuery(conn, "SELECT * from fiduciary.full_approved_payments")
    pagamentos$payment_date <- if_else(pagamentos$payment_date == as.Date("1899-12-30"), as.Date("2020-12-24"), pagamentos$payment_date)
    pagamentos$month <- toupper(months(ymd(pagamentos$payment_date), abbreviate = TRUE))
    pagamentos$month_end <- ceiling_date(pagamentos$payment_date, "month") - 1
    
    pagamentos_mensais <- pagamentos %>% group_by(funding, payment_date) %>% summarize(despesas = round(sum(paid_ammount)/1000),2)
    ts_base <- pagamentos_mensais %>% 
      e_charts(x = payment_date) %>% 
      e_datazoom(
        type = "slider", 
        toolbox = FALSE,
        bottom = -5
      ) %>% 
      e_tooltip() %>% 
      e_scatter(payment_date, despesas) |>
      e_title("Despesas em '000 MZN") %>% 
      e_x_axis(payment_date, axisPointer = list(show = TRUE))
    ts_base %>% e_line(despesas)
  })

  output$despesas_trimestrais <- renderEcharts4r({
    budgeted <- read_feather('granular_awpb_2022.feather')
    executed <- dbGetQuery(conn, "SELECT * FROM fiduciary.full_approved_payments")
    executed$quarters <- quarters(executed$submission_date)
    budgeted_quarters <- budgeted %>% group_by(fiscal_quarters) %>% summarise(Plan = round(sum(granularusd)),2)
    executed_quarters <-   executed %>% group_by(quarters) %>% summarise(Actuals = round(sum(paid_ammount)),2)
    plan_vs_actuals <- merge(budgeted_quarters, executed_quarters, by.x = "fiscal_quarters", by.y = "quarters", all = TRUE)
    plan_vs_actuals[is.na(plan_vs_actuals)] <- 0

    plan_vs_actuals %>%
      e_charts(fiscal_quarters) %>%
      e_bar(`Plan`) %>%
      e_bar(`Actuals`) %>%
      e_tooltip(trigger = "item")
  })
  
  output$execucao_componente <- DT::renderDataTable({
    components_paid <- paid_set %>% group_by(component, subcomponent,quarters) %>% summarize(montante = sum(paid_ammount)) %>% pivot_wider(names_from = quarters, values_from = montante)
    components_paid <- components_paid %>% dplyr::arrange(component) %>% split( .[,"component"] ) %>% purrr::map_df(., janitor::adorn_totals)
    components_paid$subcomponent <- ifelse(components_paid$subcomponent == "-", lag(components_paid$component),components_paid$subcomponent)
    components_paid_TT <- paid_set %>% group_by(quarters) %>% summarize(montante = sum(paid_ammount)) %>% pivot_wider(names_from = quarters, values_from = montante)
    components_paid <- dplyr::bind_rows(components_paid, components_paid_TT)
    components_paid$subcomponent[is.na(components_paid$subcomponent)] <- "Total"
    components_paid <- components_paid %>% adorn_totals("col")
    components_planned <- granular_awpb_2022 %>% group_by(component, subcomponent,fiscal_quarters) %>% summarize(montante = sum(granularmzn)) %>% pivot_wider(names_from = fiscal_quarters, values_from = montante)
    components_planned <- components_planned %>% dplyr::arrange(component) %>% split( .[,"component"] ) %>% purrr::map_df(., janitor::adorn_totals)
    components_planned$subcomponent <- ifelse(components_planned$subcomponent == "-", lag(components_planned$component),components_planned$subcomponent)
    components_planned_TT <- granular_awpb_2022 %>% group_by(fiscal_quarters) %>% summarize(montante = sum(granularmzn)) %>% pivot_wider(names_from = fiscal_quarters, values_from = montante)
    components_planned <- dplyr::bind_rows(components_planned, components_planned_TT) %>% adorn_totals("col")
    components_planned$subcomponent[is.na(components_planned$subcomponent)] <- "Total"
    components_planned <- components_planned %>% select(subcomponent, AWPB = Total)
    component_quarters <- merge(components_paid, components_planned, by = "subcomponent", all= TRUE) %>% mutate(Execution = Total/AWPB*100)
    component_quarters <- merge(component_quarters, components_design, by = "subcomponent", all = TRUE) %>% mutate(Expenditure = Total/PDR/64.46*100)
    component_quarters$copy <- component_quarters$subcomponent
    component_quarters <- component_quarters %>% arrange(copy) %>% dplyr::select(-copy, -component)
    datatable(component_quarters, extensions = 'Buttons', options = list(paging = TRUE, searching = TRUE, fixedColumns = TRUE, autoWidth = TRUE, ordering = FALSE, dom = 'tB', buttons = c('copy', 'csv', 'excel')), class = "display")
    
  })
  
  output$execucao_categoria <- DT::renderDataTable({
    paid_categories <- paid_set
    paid_categories$pdr_category[paid_categories$pdr_category =="Operating costs"] <- "Operating Costs"
    
    paid_categories <- paid_categories %>% group_by(cost_centre, pdr_category) %>% summarise(soma_paga = sum(paid_ammount))  %>% 
      pivot_wider(names_from =  "cost_centre", values_from = "soma_paga") %>% 
      adorn_totals("col")
    
    paid_categories$pdr_category[paid_categories$pdr_category == "Works"] <- "Civil Works"
    paid_categories$pdr_category[paid_categories$pdr_category == "Salaries and allowances"] <- "Salaries and Allowances"
    
    planned_categories <- granular_awpb_2022 %>% group_by(pdr_category) %>% summarise(AWPB = sum(granularmzn))
    paid_categories <- merge(paid_categories, planned_categories, by = "pdr_category", all = TRUE)
    paid_categories <- merge(paid_categories, PDR_categories, by = "pdr_category", all = TRUE)
    paid_categories <- paid_categories %>% adorn_totals("row")
    paid_categories$AWPB_percent <- paid_categories$Total/paid_categories$AWPB*100
    paid_categories$PDR_percent <- paid_categories$Total/paid_categories$PDR*100/64.46
    datatable(paid_categories, extensions = 'Buttons', options = list(paging = TRUE, searching = TRUE, fixedColumns = TRUE, autoWidth = TRUE, ordering = FALSE, dom = 'Bfrtip', buttons = c('copy', 'csv', 'excel')), class = "display")
  })
  
  output$execucao_financiador <- DT::renderDataTable({
    paid_financiers <- paid_set
    paid_financiers <- paid_financiers %>% group_by(funding, quarters) %>% summarise(soma_paga = sum(paid_ammount))  %>% 
      pivot_wider(names_from =  "quarters", values_from = "soma_paga") %>% 
      adorn_totals("col")
    PDR_financiers <- cost_tabs_pdr %>% dplyr::filter(class == "financiers") %>% select(funding = Expenditure, PDR = total_cost)
    planned_financiers <- granular_awpb_2022 %>% group_by(financiers) %>% summarise(AWPB = sum(granularmzn))
    paid_financiers <- merge(paid_financiers, planned_financiers, by.x = "funding", by.y= "financiers", all = TRUE)
    paid_financiers <- merge(paid_financiers, PDR_financiers, by = "funding", all = TRUE)
    paid_financiers <- paid_financiers %>% adorn_totals("row")
    paid_financiers$AWPB_percent <- paid_financiers$Total/paid_financiers$AWPB*100
    paid_financiers$PDR_percent <- paid_financiers$Total/paid_financiers$PDR*100/64.46

    datatable(paid_financiers, extensions = 'Buttons', options = list(paging = TRUE, searching = TRUE, fixedColumns = TRUE, autoWidth = TRUE, ordering = FALSE, dom = 'Bfrtip', buttons = c('copy', 'csv', 'excel')), class = "display")
  })
  
  output$execucao_financiador <- DT::renderDataTable({
    paid_financiers <- paid_set
    paid_financiers <- paid_financiers %>% group_by(funding, quarters) %>% summarise(soma_paga = sum(paid_ammount))  %>% 
      pivot_wider(names_from =  "quarters", values_from = "soma_paga") %>% 
      adorn_totals("col")
    PDR_financiers <- cost_tabs_pdr %>% dplyr::filter(class == "financiers") %>% select(funding = Expenditure, PDR = total_cost)
    planned_financiers <- granular_awpb_2022 %>% group_by(financiers) %>% summarise(AWPB = sum(granularmzn))
    paid_financiers <- merge(paid_financiers, planned_financiers, by.x = "funding", by.y= "financiers", all = TRUE)
    paid_financiers <- merge(paid_financiers, PDR_financiers, by = "funding", all = TRUE)
    paid_financiers <- paid_financiers %>% adorn_totals("row")
    paid_financiers$AWPB_percent <- paid_financiers$Total/paid_financiers$AWPB*100
    paid_financiers$PDR_percent <- paid_financiers$Total/paid_financiers$PDR*100/64.46
    
    
    datatable(paid_financiers, extensions = 'Buttons', options = list(paging = TRUE, searching = TRUE, fixedColumns = TRUE, autoWidth = TRUE, ordering = FALSE, dom = 'Bfrtip', buttons = c('copy', 'csv', 'excel')), class = "display")
  })


  output$informacao_proposta <- DT::renderDataTable({
    req(input$info_proposta)
    inFile <- input$info_proposta
    ficheiro <- read_excel(inFile$datapath, sheet = "DATASET")
    ficheiro$paid_ammount <-  as.numeric(ficheiro$paid_ammount)
    payments_dataset <- ficheiro %>% dplyr::filter(!is.na(paid_ammount))
    ficheiro_visivel <- payments_dataset 
    ficheiro_visivel <- ficheiro_visivel %>% dplyr::select(beneficiary, awpb_id, processo_numero, detailed, payment_details, paid_ammount, contract,
                                                           e_sistafe_w_code, funding, applicant_name, everything())
    datatable(ficheiro_visivel, options=list(columnDefs = list(list(visible=FALSE, targets=c(13:ncol(ficheiro_visivel)))), rowsGroup = list(2)))
    
  })

  iv3 <- InputValidator$new()
  # iv3$add_rule("nuit", sv_between(100000000, 599999999, message_fmt = "NUIT correcto deve estar entre {left} e {right}."))
  # iv3$add_rule("processo_numero", sv_between(0, 999, message_fmt = "Numerador deve estar entre {left} e {right}."))
  iv3$add_rule("supplier_email", sv_email(message = "Email inválido", allow_multiple = FALSE, allow_na = FALSE))
  # iv3$add_rule("value", sv_between(0, 100000000, message_fmt = "Deve estar entre {left} e {right}."))
  # iv3$add_rule("contract_level", sv_between(0, 100, message_fmt = "Valor correcto deve estar entre {left} e {right}. Idealmente nunca deveria passar 100"))
  # iv3$add_rule("contract", sv_required(message = "É obrigatório indicar o contrato"))
  iv3$enable()

  
  output$payment_approvals <- renderDataTable({
    payment_requests <-  dbGetQuery(conn, "SELECT * FROM fiduciary.payment_requests")
    
    datatable(payment_requests, editable = TRUE, callback = JS(callback), options = list(columnDefs = list(
      list(targets = 13, className = "factor", createdCell = JS(createdCell(c("POSITIVO", "NEGATIVO", "PENDENTE")))), list(visible=FALSE, targets=c(14:ncol(payment_requests))))))
  })
  
  observeEvent(input$payment_approvals_cell_edit, {
    payment_requests[input$payment_approvals_cell_edit$row,input$payment_approvals_cell_edit$col] <<- input$payment_approvals_cell_edit$value
  })
  
  observeEvent(input$saveBtn,{
    payment_requests$payment_date <- Sys.Date()
    dbWriteTable(conn, SQL("fiduciary.full_approved_payments"), value = subset(payment_requests, finance_approval == "POSITIVO"), append = TRUE, overwrite = FALSE, row.names = FALSE)
    dbWriteTable(conn, "fiduciary.payment_requests", value = subset(payment_requests, finance_approval == "PENDENTE"), append = FALSE, overwrite = TRUE, row.names = FALSE)
    dbWriteTable(conn, "fiduciary.rejected_payments", value = subset(payment_requests, finance_approval == "NEGATIVO"), append = TRUE, overwrite = FALSE, row.names = FALSE)
    
    showModal(modalDialog(title = "PARABÉNS. Pagamentos Registados!"))
  })
 
   observeEvent(input$submeter_informacao_proposta, priority = 20, {
    req(input$info_proposta)
    inFile <- input$info_proposta
    ficheiro <- read_excel(inFile$datapath, sheet = "DATASET", col_types = c('text', 'text', 'numeric', 'text', 'text', 'text', 'text', 'numeric', 'numeric', 'numeric', 'numeric', 'numeric', 'numeric', 'numeric', 'text', 'text', 'text', 'text', 'date', 'date', 'numeric', 'text', 'text', 'text', 'text', 'text', 'text', 'text', 'text', 'text', 'text', 'numeric', 'text', 'text', 'text', 'text', 'text', 'numeric', 'numeric', 'text', 'date', 'date', 'text', 'numeric', 'numeric', 'numeric', 'text', 'numeric', 'numeric', 'numeric', 'numeric', 'numeric', 'text', 'text', 'text', 'text', 'text', 'text', 'text', 'text', 'text', 'text', 'text', 'text', 'text', 'text', 'numeric', 'text', 'text', 'text', 'text', 'date', 'text', 'text'))
    # ficheiro$paid_ammount <- as.numeric(ficheiro$paid_ammount)                      
    payments_dataset <- ficheiro %>% dplyr::filter(!is.na(paid_ammount))
    payments_dataset$ced <- parse_number(payments_dataset$e_sistafe_w_code)
    # payments_dataset <- payments_dataset %>% dplyr::filter(value !=)
    payments_dataset <- payments_dataset %>% dplyr::select(description, beneficiary, nuit, document, awpb_id, payment_details, units, quantity, price, paid_ammount,
                                                           contract, e_sistafe_w_code, detailed, relevance, started, ended, tompro_pay, ben_district, cost_centre,
                                                           process_type, process_status, funding, product_location, payment_order, submission_date, payment_date, pendings, 
                                                           government_contrib, private_contrib, third_party, processo_numero, expense_request_uid, planning_approval, 
                                                           procurement_approval, finance_approval, payment_ticket, overall_approval, comments, currency, quantity10, 
                                                           approval_comments, staff_id, is_contract, urgencia_processo, document_type, payment_checklist, update_date,
                                                           quantity30, quantity100, justificativo_tipo, subject, ifad_money, benef_contrib, ced)
   
    # dbWriteTable(conn, "paymentsrequested", value = payments_dataset, append = TRUE, overwrite = FALSE, row.names = FALSE)
    dbWriteTable(conn, SQL("fiduciary.payment_requests"), payments_dataset, overwrite = FALSE, append = TRUE)
    
    showModal(modalDialog(title=paste0("Pedido submetido"), br(), div(tags$b(paste0("SUCESSO!"), style = "color: green;"))))
  })
   
   renderSurvey()
   
   iv<- InputValidator$new()
   iv$add_rule("nuit", sv_between(100000000, 599999999, message_fmt = "NUIT correcto deve estar entre {left} e {right}."))
   iv$add_rule("processo_numero", sv_between(0, 999, message_fmt = "Numerador deve estar entre {left} e {right}."))
   # iv$add_rule("applicant_email", sv_email(message = "Email inválido", allow_multiple = FALSE, allow_na = FALSE))
   iv$add_rule("value", sv_between(0, 100000000, message_fmt = "Deve estar entre {left} e {right}."))
   iv$add_rule("contract_level", sv_between(0, 100, message_fmt = "Valor correcto deve estar entre {left} e {right}. Idealmente nunca deveria passar 100"))
   iv$add_rule("contract", sv_required(message = "É obrigatório indicar o contrato"))
   iv$enable()
   
   observeEvent(input$submit, {
     processo_payment <-  getSurveyData()
     
     processo_payment <-  processo_payment %>% dplyr::select(question_id, response)
     processo_payment$response[processo_payment$response== "HIDDEN-QUESTION"] <- NA
     
     processo_payment <- setNames(data.frame(t(processo_payment[-1])), processo_payment[,1])
     
     processo_payment <- processo_payment
     setnames(processo_payment, "rubricas", "e_sistafe_w_code")
     setnames(processo_payment, "status", "process_status")
     setnames(processo_payment, "document_type", "justificativo_tipo")
     processo_payment <- processo_payment %>% mutate(ced = readr::parse_number(as.character(e_sistafe_w_code))) %>% select(-contract_level)
     
     processo_payment$process_type <- "Pagamento de fundos cobrados por fornecedores"
     processo_payment$product_location <- processo_payment$ben_district
     processo_payment$subject <- processo_payment$description
     
     # strsplit(processo_payment$awpb_id, split = " -- ")
     
     processo_payment <- processo_payment %>% separate(awpb_coding, c('awpb_id','tompro_pay'), sep = " -- ", extra='drop')
     processo_payment <- processo_payment %>% separate(applicant, c('applicant','staff_id'), sep = " -- ", extra='drop')
     # processo_payment <- processo_payment %>% select(-awpb_coding)
     
     processo_payment$planning_approval <- "PENDENTE"
     processo_payment$overall_approval <- "PENDENTE"
     processo_payment$procurement_approval <- "PENDENTE"
     processo_payment$finance_approval <- "PENDENTE"
     # processo_payment$finance_approval <- "Duplo clique e digite a OP"
     processo_payment$approval_comments <- "Duplo clique para comentar"
     processo_payment$is_contract <- "YES"
     processo_payment$quantity100 <- processo_payment$quantity
     
     processo_payment$payment_ticket <- "Duplo clique e digite a OP"
     
     processo_payment$expense_request_uid <- toupper(str_trim(paste0(processo_payment$document, "-", processo_payment$nuit, "-", year(as.Date(processo_payment$submission_date))), side="both"))
     # dbWriteTable(pool, "fiduciary.contractpayments", value = processo_payment, append = FALSE, overwrite = TRUE, row.names = FALSE)
     dbWriteTable(pool, SQL("fiduciary.payment_requests"), value =  processo_payment, append = TRUE, overwrite = FALSE, row.names = FALSE)
     
     showModal(modalDialog(title=paste0("Pedido submetido. Aguarde pelo despacho!"), br(), div(tags$b(paste0("SUCESSO!"), style = "color: green;"))))
   })

   
   observeEvent(input$escrever_ip, {
     payments_data <- dbGetQuery(conn, "SELECT * FROM fiduciary.full_payment_dataset") %>% filter(processo_numero == "055/UNGP/PMA&GC/032.22/2022")
     
     sample_doc <- read_docx("Modelo_IP_RMarkdown_Final.docx")
     
     payments_data <- payments_data %>% mutate_if(is.numeric, ~replace_na(., 0))
     payments_summary <- payments_data %>% group_by(e_sistafe_w_code, e_sistafe_pt) %>% summarize(qty_10 = sum(quantity10), 
                                                                                    qty_30 = sum(quantity30), 
                                                                                    qty_100 = sum(quantity100),
                                                                                    valor = sum(paid_ammount))
     
     payments_summary$preco <- payments_summary$valor/(payments_summary$qty_10+payments_summary$qty_30+payments_summary$qty_100)
     payments_summary <-  modify_if(payments_summary, ~is.numeric(.), ~round(., 2))
     
     payments_summary[is.na(payments_summary)] <- 0
     payments_summary <- payments_summary %>% adorn_totals("row")

     payments_summary$ced<-gsub("[^0-9]", "", payments_summary$e_sistafe_w_code)
     
     payments_summary$ced <- ifelse(payments_summary$e_sistafe_w_code == "Total", "-",  payments_summary$ced)
     payments_summary <- payments_summary %>% select(ced, e_sistafe_pt, qty_10, qty_30, qty_100, preco, everything())
     payments_summary <- payments_summary %>% select(-e_sistafe_w_code)

     payments_summary$ced <- as.character(payments_summary$ced) 
     setnames(payments_summary, c("ced", "e_sistafe_pt", "qty_10", "qty_30", "qty_100", "preco", "valor"), 
              c("CED", "Designação do CED", "10%", "30%", "100%", "Preço", "Valor (MT)"))

     sample_doc <- cursor_reach(sample_doc, keyword = "seguintes rubricas.")
     set_flextable_defaults(font.size = 9, font.color = "black", table.layout = "fixed", digits = 1)
     ft <- flextable(head(payments_summary))
     ft <- theme_booktabs(ft)
     ft <- autofit(ft)
     ft <- colformat_double(x = ft, big.mark=",", digits = 2, na_str = "N/A")
     ft <- align_nottext_col(ft, align = "right")
     
     ft <- line_spacing(ft, space = 1.0, part = "all")
     ft <- bold(ft, bold = TRUE, part = "header")
     ft <- bold(ft, i = nrow(payments_summary), bold = TRUE)
     ft <- padding(ft, padding = 1)
     ft <- compose(ft, i = nrow(payments_summary), j = 3, as_paragraph(as_chunk('-')))
     ft <- compose(ft, i = nrow(payments_summary), j = 4, as_paragraph(as_chunk('-')))
     ft <- compose(ft, i = nrow(payments_summary), j = 5, as_paragraph(as_chunk('-')))
     ft <- compose(ft, i = nrow(payments_summary), j = 6, as_paragraph(as_chunk('-')))
     
     sample_doc <- body_add_flextable(sample_doc, value = ft, pos = "after")
     
     payments_data$document <- paste0(payments_data$justificativo_tipo, " n.º ", payments_data$document)
     
     document <- glue_collapse(payments_data$document, ", ", last = " e ")
     
     financiadores <- payments_data %>% group_by(details_contributors) %>% summarize(sum(paid_ammount)) %>% adorn_totals("col")
     financiadores$percent <-  financiadores$Total/sum(payments_data$paid_ammount)*100
     
     financiadores$contribs <- paste0(financiadores$details_contributors, " (", round(financiadores$percent,2), "%)")
     funding <- glue_collapse(financiadores$contribs, ", ", last = " e ")
     
     sample_doc <- body_replace_all_text(sample_doc, old_value = "FUNDO DE FOMENTO AGRÁRIO E EXTENSÃO RURAL, FUNDO PÚBLICO (FAR, FP)", new_value = toupper(paste0(payments_data$instituicao[1])), only_at_cursor = FALSE, fixed = TRUE)
     sample_doc <- body_replace_all_text(sample_doc, old_value = "MINISTÉRIO DA AGRICULTURA E DESENVOLVIMENTO RURAL", new_value = toupper(paste0(payments_data$entidade_governo[1])), only_at_cursor = FALSE, fixed = TRUE)
     
     sample_doc <- body_replace_all_text(sample_doc, old_value = "UNIDADE_GESTORA", new_value = toupper(paste0(payments_data$unidade_gestora[1])), only_at_cursor = FALSE, fixed = TRUE)
     sample_doc <- body_replace_all_text(sample_doc, old_value = "supplier_name", new_value = paste0(payments_data$supplier_name[1]), only_at_cursor = FALSE, fixed = TRUE)
     sample_doc <- body_replace_all_text(sample_doc, old_value = "submission_date", new_value = paste0(as.character(payments_data$submission_date[1], format = "%d/%m/%Y")), only_at_cursor = FALSE, fixed = TRUE)
     sample_doc <- body_replace_all_text(sample_doc, old_value = "subcomponente_pt", new_value = paste0(payments_data$componentnames_pt[1]), only_at_cursor = FALSE, fixed = TRUE)
     sample_doc <- body_replace_all_text(sample_doc, old_value = "subcompo_desc", new_value = paste0(payments_data$components_pt[1]), only_at_cursor = FALSE, fixed = TRUE)
     sample_doc <- body_replace_all_text(sample_doc, old_value = "sectores", new_value = paste0(payments_data$sectores[1]), only_at_cursor = FALSE, fixed = TRUE)
     sample_doc <- body_replace_all_text(sample_doc, old_value = "relevance", new_value = paste0(payments_data$relevance[1]), only_at_cursor = FALSE, fixed = TRUE)
     sample_doc <- body_replace_all_text(sample_doc, old_value = "process_type", new_value = paste0(payments_data$process_type[1]), only_at_cursor = FALSE, fixed = TRUE)
     sample_doc <- body_replace_all_text(sample_doc, old_value = "physical_compl", new_value = paste0(payments_data$physical_compl[1]), only_at_cursor = FALSE, fixed = TRUE)
     sample_doc <- body_replace_all_text(sample_doc, old_value = "paid_ammount", new_value = paste0(format(round(sum(payments_data$paid_ammount), 2), big.mark=",", nsmall = 2, scientific=FALSE)), only_at_cursor = FALSE, fixed = TRUE)
     sample_doc <- body_replace_all_text(sample_doc, old_value = "funding", new_value = funding, only_at_cursor = FALSE, fixed = TRUE)

     sample_doc <- body_replace_all_text(sample_doc, old_value = "new_salute", new_value = paste0(payments_data$new_salute[1]), only_at_cursor = FALSE, fixed = TRUE)
     sample_doc <- body_replace_all_text(sample_doc, old_value = "new_approver", new_value = paste0(payments_data$new_approver[1]), only_at_cursor = FALSE, fixed = TRUE)
     sample_doc <- body_replace_all_text(sample_doc, old_value = "name_applicant", new_value = paste0(payments_data$name[1]), only_at_cursor = FALSE, fixed = TRUE)
     sample_doc <- body_replace_all_text(sample_doc, old_value = "financial_compl", new_value = paste0(payments_data$financial_compl[1]), only_at_cursor = FALSE, fixed = TRUE)
     sample_doc <- body_replace_all_text(sample_doc, old_value = "e_sistafe_w_code", new_value = paste0(payments_data$e_sistafe_w_code[1]), only_at_cursor = FALSE, fixed = TRUE)
     sample_doc <- body_replace_all_text(sample_doc, old_value = "documents_numbers", new_value = document, only_at_cursor = FALSE, fixed = TRUE)
     sample_doc <- body_replace_all_text(sample_doc, old_value = "detailed", new_value = paste0(payments_data$detailed[1]), only_at_cursor = FALSE, fixed = TRUE)
     sample_doc <- body_replace_all_text(sample_doc, old_value = "awpb_description", new_value = paste0(payments_data$descricao_da_actividade[1]), only_at_cursor = FALSE, fixed = TRUE)
     sample_doc <- body_replace_all_text(sample_doc, old_value = "cost_centers", new_value = paste0(payments_data$cost_centers[1]), only_at_cursor = FALSE, fixed = TRUE)
     sample_doc <- body_replace_all_text(sample_doc, old_value = "contractdescription_pt", new_value = paste0(payments_data$contractdescription_pt[1]), only_at_cursor = FALSE, fixed = TRUE)
     sample_doc <- body_replace_all_text(sample_doc, old_value = "CONTRACT_NUMBER", new_value = paste0(payments_data$contract_number[1]), only_at_cursor = FALSE, fixed = TRUE)
     sample_doc <- body_replace_all_text(sample_doc, old_value = "contract", new_value = paste0(payments_data$contract_number[1]), only_at_cursor = FALSE, fixed = TRUE)

     sample_doc <- body_replace_all_text(sample_doc, old_value = "categoria_pdr", new_value = paste0(payments_data$categoria_pdr[1]), only_at_cursor = FALSE, fixed = TRUE)
     sample_doc <- body_replace_all_text(sample_doc, old_value = "carreira_proponente", new_value = paste0(payments_data$categoria[1]), only_at_cursor = FALSE, fixed = TRUE)
     sample_doc <- body_replace_all_text(sample_doc, old_value = "awpb_id", new_value = paste0(payments_data$awpb_id[1]), only_at_cursor = FALSE, fixed = TRUE)
     sample_doc <- footers_replace_all_text(sample_doc, old_value = "encedereco_alvo", new_value = paste0(payments_data$address[1]), only_at_cursor = FALSE, fixed = TRUE)
     sample_doc <- footers_replace_all_text(sample_doc, old_value = "cidade_alvo", new_value = paste0(payments_data$city[1]), only_at_cursor = FALSE, fixed = TRUE)
     sample_doc <- body_replace_all_text(sample_doc, old_value = "role", new_value = paste0(payments_data$short_vaccancy[1]), only_at_cursor = FALSE, fixed = TRUE)
     
     file.path(Sys.getenv("HOME"))

     print(sample_doc, target = file.path(Sys.getenv("HOME"), paste0(input$descricao_processo, ".docx")))

     showModal(modalDialog(title=paste0("Informação Gerada. Verifique no seu Computador!"), br(), div(tags$b(paste0("SUCESSO!"), style = "color: green;"))))
   })
   
  
   
   # procurement_dossiers <- reactive({
   #   input$submeter_dossier
   #   input$submit_dossier
   #   DBI::dbGetQuery(con, "SELECT * from fiduciary.procurement_dossiers")
   # })
   
   # cn <- filtered_dossiers()
   # dossier_numbers <- as.list(unique(cn$idpp))
   
   fieldsMandatory <- c("procurement_stage", "pp_comment")
   observe({
     mandatoryFilled <-
       vapply(fieldsMandatory,
              function(x) {
                !is.null(input[[x]]) && input[[x]] != ""
              },
              logical(1))
     mandatoryFilled <- all(mandatoryFilled)
     shinyjs::toggleState(id = "submeter_dossier", condition = mandatoryFilled)
   })
   
   entry_form_ppdossiers <- function(button_id){
     showModal(
       modalDialog(
         div(id=("entry_form_ppdossiers"),
             tags$head(tags$style(".modal-dialog{ width:650px}")),
             tags$head(tags$style(HTML(".shiny-split-layout > div {overflow: visible}"))),
             fluidPage(
               fluidRow(h4("ACTUALIZAR CONTRATAÇÕES"),
                        splitLayout(cellWidths = c("20%", "80%"), cellArgs = list(style = "vertical-align: top"),
                                    selectizeInput("idpp", "Ref.ª", multiple = FALSE, choices = as.character(unique(pp_dataset$idpp))),
                                    textInput("activity_description_pt", "Descrição da Actividade")),
                        
                        splitLayout(cellWidths = c("50%", "50%"), cellArgs = list(style = "vertical-align: top"),           
                                    selectizeInput("procurement_stage", labelMandatory("Ponto de Situação"), multiple = FALSE, choices = pp_stages_pt),
                                    selectizeInput("method_name_pt", "Método de contratação", multiple = FALSE, choices = c(pp_method_names_pt))),
                        
                        splitLayout(cellWidths = c("33.3333%", "33.3333%", "33.3333%"), cellArgs = list(style = "vertical-align: top"), 
                                    selectizeInput("ifad_review", "Revisão", multiple = FALSE, choices = c("Revisão Prévia" = "Prior Review", "Revisão Posterior" =  "Post Review")),            
                                    selectizeInput("non_consulting", "Não consultoria?", multiple = FALSE, choices = c("", "SIM"="Yes", "NÃO"="No")),
                                    selectizeInput("shortlisting_pp", "Lista-curta", multiple = FALSE, choices = c("SIM"="Yes","NÃO"="No"))),
                        
                        splitLayout(cellWidths = c("33.3333%", "33.3333%", "33.3333%"), cellArgs = list(style = "vertical-align: top"), 
                                    selectizeInput("qualification", "Qualificação", multiple = FALSE, choices = c("Pré-qualificação" = "Pre-Qual", "Pós-qualificação" =  "Post-Qual")),
                                    numericInput("envelopes", "Envelopes", 1, min = 1, max = 2),
                                    numericInput("contract_usd", "Valor da Adjudicação", 0, min = 0, max = 40000000)),
                        
                        splitLayout(cellWidths = c("33.3333%", "33.3333%", "33.3333%"), cellArgs = list(style = "vertical-align: top"), 
                                    dateInput("eo_i_submission", "Submissão de TDR", format = "yyyy-mm-dd"),
                                    dateInput("no_eo_i", "Não objecção TDR", format = "yyyy-mm-dd"),
                                    dateInput("inv_r_eo_i", "Convite para MDI", format = "yyyy-mm-dd")),
                        
                        splitLayout(cellWidths = c("33.3333%", "33.3333%", "33.3333%"), cellArgs = list(style = "vertical-align: top"), 
                                    dateInput("close_r_eo_i", "Recepção de MDIs", format = "yyyy-mm-dd"),
                                    dateInput("report_eo_i", "Relatório de MDI", format = "yyyy-mm-dd"),
                                    dateInput("no_eo_i_report", "NO Relatório de MDI", format = "yyyy-mm-dd")),
                        
                        splitLayout(cellWidths = c("33.3333%", "33.3333%", "33.3333%"), cellArgs = list(style = "vertical-align: top"), 
                                    dateInput("rfp_rno", "Documentos de Concurso", format = "yyyy-mm-dd"),
                                    dateInput("rfp_no", "NO Documentos de Concurso", format = "yyyy-mm-dd"),
                                    dateInput("invitation", "Anúncio de Concurso", format = "yyyy-mm-dd")),
                        
                        splitLayout(cellWidths = c("33.3333%", "33.3333%", "33.3333%"), cellArgs = list(style = "vertical-align: top"), 
                                    dateInput("closing", "Recepção de Propostas", format = "yyyy-mm-dd"),
                                    dateInput("rno_eva_r", "Relatório Técnico", format = "yyyy-mm-dd"),
                                    dateInput("no_eva_r", "NO Relatório Técnico", format = "yyyy-mm-dd")),
                        
                        splitLayout(cellWidths = c("33.3333%", "33.3333%", "33.3333%"), cellArgs = list(style = "vertical-align: top"), 
                                    dateInput("full_eva_rno", "Relatório Combinado", format = "yyyy-mm-dd"),
                                    dateInput("full_eva_no", "NO Relatório Combinado", format = "yyyy-mm-dd"),
                                    dateInput("noita", "Notificação de Adjudicação", format = "yyyy-mm-dd")),
                        
                        splitLayout(cellWidths = c("33.3333%", "33.3333%", "33.3333%"), cellArgs = list(style = "vertical-align: top"), 
                                    dateInput("contract_awards", "Adjudicação", format = "yyyy-mm-dd"),
                                    dateInput("negotiations", "Negociações", format = "yyyy-mm-dd"),
                                    dateInput("rno_contract", "Draft do Contrato", format = "yyyy-mm-dd")),
                        
                        splitLayout(cellWidths = c("33.3333%", "33.3333%", "33.3333%"), cellArgs = list(style = "vertical-align: top"), 
                                    dateInput("no_contract", "NO Draft do Contrato", format = "yyyy-mm-dd"),
                                    dateInput("signature", "Assinatura", format = "yyyy-mm-dd"),
                                    dateInput("contract_completion", "Conclusão", format = "yyyy-mm-dd")),
                        
                        splitLayout(cellWidths = c("33.3333%", "33.3333%", "33.3333%"), cellArgs = list(style = "vertical-align: top"), 
                                    textInput("contract_n", "Número n.º"),
                                    textInput("vendor_id", "Contratada"),
                                    selectizeInput("updated_by", "Actualizado por", multiple = FALSE, choices = staff_choices)),
                        
                        textAreaInput("pp_comment", "Comentários", placeholder = "", height = 100, width = "100%"),
                        
                        helpText(labelMandatory(""), paste("Campo Obrigatório!")),
                        actionButton("submeter_dossier", "SALVAR", class = "btn-success")),
               easyClose = TRUE
             )
         )
       )
     )
   }
   
   fieldsAll <- c('activ_id', 'idpp', 'activity', 'is_grant', 'non_consultancies', 'activity_description_pt', 'shortlisting_pp', 
                  'ifad_review', 'qualification', 'proc_methods', 'envelopes', 'basic_usd', 'eo_i_submission', 'no_eo_i', 'inv_r_eo_i', 
                  'close_r_eo_i', 'report_eo_i', 'no_eo_i_report', 'rfp_rno', 'rfp_no', 'invitation', 'closing', 'rno_eva_r', 'no_eva_r', 
                  'full_eva_rno', 'full_eva_no', 'noita', 'contract_awards', 'negotiations', 'rno_contract', 'no_contract', 'signature', 
                  'contract_n', 'vendor_id', 'contract_usd', 'contract_completion', 'pp_comment', 'lot', 'responsible', 'ifadpp_sheet', 
                  'method_name_pt', 'non_consulting', 'activity_pt', 'lotes', 'procurement_stage')
   
   
   
   
   # ppdossiers_formData <- reactive({
   #   ppdossiers_formData <- data.frame(
   #     idpp =  input$idpp,
   #     procurement_stage =  input$procurement_stage,
   #     contract_usd  =  as.numeric(input$contract_usd),
   #     supplier_vs_contract  =  input$supplier_vs_contract,
   #     
   #     eo_i_submission  =  iinput$eo_i_submission,
   #     
   #     shortlisting_pp = input$shortlisting_pp, 
   #     ifad_review = input$ifad_review, 
   #     qualification = input$qualification, 
   #     envelopes  = input$envelopes, 
   #     no_eo_i  = input$no_eo_i, 
   #     inv_r_eo_i  = input$inv_r_eo_i,
   #     close_r_eo_i = input$close_r_eo_i,
   #     report_eo_i  = input$report_eo_i, 
   #     
   #     no_eo_i_report  = input$no_eo_i_report,
   #     rfp_rno  = input$rfp_rno,
   #     rfp_no = input$rfp_no,
   #     invitation  = input$invitation,
   #     closing  = input$closing,
   #     rno_eva_r  = input$rno_eva_r, 
   #     no_eva_r  = input$no_eva_r, 
   #     full_eva_rno = input$full_eva_rno,
   #     full_eva_no = input$full_eva_no, 
   #     noita  = input$noita ,
   #     contract_awards  = input$contract_awards, 
   #     negotiations  = input$negotiations, 
   #     rno_contract  = input$rno_contract,
   #     no_contract  = input$no_contract, 
   #     signature  = input$signature,
   #     contract_n  = input$contract_n, 
   #     vendor_id  = input$vendor_id, 
   #     contract_completion  = input$contract_completion,
   #     non_consulting = input$non_consulting, 
   #     
   #     method_name_pt  =  input$method_name_pt,
   #     pp_comment  =  input$pp_comment,
   #     last_updated = as.character(format(Sys.Date(), format="yyyy-mm-dd")),
   #     stringsAsFactors = FALSE)
   #   
   #   return(ppdossiers_formData)
   # })
   # 
   # appendData <- function(data){
   #   quary <- filtered_dossiers()
   #   dbExecute(pool, quary)
   # }
   
   # observeEvent(input$add_dossier, priority = 20,{entry_form_ppdossiers("submeter_dossier")})
   # 
   # observeEvent(input$submit, priority = 20,{appendData(ppdossiers_formData())
   #   shinyjs::reset("entry_form_ppdossiers")
   #   removeModal()
   # })
   # 
   # observeEvent(input$delete_dossier, priority = 20,{if(length(input$ppdossiers_rows_selected)>=1 ){deleteData()}
   #   showModal(if(length(input$ppdossiers_rows_selected) < 1 ){modalDialog(title = "Warning", paste("Por favor selecione linhas." ), easyClose = TRUE)})
   # })
   
   observeEvent(input$edit_dossier, priority = 20,{
     SQL_ppdossiers <- filtered_dossiers()
     showModal(
       if(length(input$ppdossiers_rows_selected) > 1 ){
         modalDialog(
           title = "Warning",
           paste("Por favor, selecione só uma linha." ),easyClose = TRUE)
       } else if(length(input$ppdossiers_rows_selected) < 1){
         modalDialog(
           title = "Warning",
           paste("Por favor, selecione só uma linha." ),easyClose = TRUE)
       })  
     
     if(length(input$ppdossiers_rows_selected) == 1 ){
       entry_form_ppdossiers("submit_dossier")
       
       updateTextInput(session, "activity_description_pt", value = SQL_ppdossiers[input$ppdossiers_rows_selected, "activity_description_pt"])
       # updateSelectizeInput(session, "lotes", selected = SQL_ppdossiers[input$ppdossiers_rows_selected, "lotes"])
       updateSelectizeInput(session, "procurement_stage", selected = SQL_ppdossiers[input$ppdossiers_rows_selected, "procurement_stage"])
       updateSelectizeInput(session, "method_name_pt", selected = SQL_ppdossiers[input$ppdossiers_rows_selected, "method_name_pt"])
       updateSelectizeInput(session, "ifad_review", selected = SQL_ppdossiers[input$ppdossiers_rows_selected, "ifad_review"])
       updateSelectizeInput(session, "non_consulting", selected = SQL_ppdossiers[input$ppdossiers_rows_selected, "non_consulting"])
       updateSelectizeInput(session, "shortlisting_pp", selected = SQL_ppdossiers[input$ppdossiers_rows_selected, "shortlisting_pp"])
       updateSelectizeInput(session, "qualification", selected = SQL_ppdossiers[input$ppdossiers_rows_selected, "qualification"])
       updateSelectizeInput(session, "updated_by", selected = SQL_ppdossiers[input$ppdossiers_rows_selected, "updated_by"])
       
       updateSelectizeInput(session, "idpp", selected = SQL_ppdossiers[input$ppdossiers_rows_selected, "idpp"])
       
       updateNumericInput(session, "envelopes", value = SQL_ppdossiers[input$ppdossiers_rows_selected, "envelopes"])
       updateNumericInput(session, "basic_usd", value = SQL_ppdossiers[input$ppdossiers_rows_selected, "basic_usd"])
       updateNumericInput(session, "contract_usd", value = SQL_ppdossiers[input$ppdossiers_rows_selected, "contract_usd"])
       updateNumericInput(session, "lot", value = SQL_ppdossiers[input$ppdossiers_rows_selected, "lot"])
       
       updateDateInput(session, "no_eo_i", value = SQL_ppdossiers[input$ppdossiers_rows_selected, "no_eo_i"])
       updateDateInput(session, "eo_i_submission", value = SQL_ppdossiers[input$ppdossiers_rows_selected, "eo_i_submission"])
       updateDateInput(session, "no_eo_i", value = SQL_ppdossiers[input$ppdossiers_rows_selected, "no_eo_i"])
       updateDateInput(session, "inv_r_eo_i", value = SQL_ppdossiers[input$ppdossiers_rows_selected, "inv_r_eo_i"])
       updateDateInput(session, "close_r_eo_i", value = SQL_ppdossiers[input$ppdossiers_rows_selected, "close_r_eo_i"])
       updateDateInput(session, "report_eo_i", value = SQL_ppdossiers[input$ppdossiers_rows_selected, "report_eo_i"])
       updateDateInput(session, "no_eo_i_report", value = SQL_ppdossiers[input$ppdossiers_rows_selected, "no_eo_i_report"])
       updateDateInput(session, "rfp_rno", value = SQL_ppdossiers[input$ppdossiers_rows_selected, "rfp_rno"])
       updateDateInput(session, "rfp_no", value = SQL_ppdossiers[input$ppdossiers_rows_selected, "rfp_no"])
       updateDateInput(session, "invitation", value = SQL_ppdossiers[input$ppdossiers_rows_selected, "invitation"])
       updateDateInput(session, "closing", value = SQL_ppdossiers[input$ppdossiers_rows_selected, "closing"])
       updateDateInput(session, "rno_eva_r", value = SQL_ppdossiers[input$ppdossiers_rows_selected, "rno_eva_r"])
       updateDateInput(session, "no_eva_r", value = SQL_ppdossiers[input$ppdossiers_rows_selected, "no_eva_r"])
       updateDateInput(session, "full_eva_rno", value = SQL_ppdossiers[input$ppdossiers_rows_selected, "full_eva_rno"])
       updateDateInput(session, "full_eva_no", value = SQL_ppdossiers[input$ppdossiers_rows_selected, "full_eva_no"])
       updateDateInput(session, "noita", value = SQL_ppdossiers[input$ppdossiers_rows_selected, "noita"])
       updateDateInput(session, "contract_awards", value = SQL_ppdossiers[input$ppdossiers_rows_selected, "contract_awards"])
       updateDateInput(session, "negotiations", value = SQL_ppdossiers[input$ppdossiers_rows_selected, "negotiations"])
       updateDateInput(session, "rno_contract", value = SQL_ppdossiers[input$ppdossiers_rows_selected, "rno_contract"])
       updateDateInput(session, "no_contract", value = SQL_ppdossiers[input$ppdossiers_rows_selected, "no_contract"])
       updateDateInput(session, "signature", value = SQL_ppdossiers[input$ppdossiers_rows_selected, "signature"])
       updateDateInput(session, "contract_completion", value = SQL_ppdossiers[input$ppdossiers_rows_selected, "contract_completion"])

       updateTextAreaInput(session, "pp_comment", value = SQL_ppdossiers[input$ppdossiers_rows_selected, "pp_comment"])
     }
   })
   

   
   
   # pp_filtering <-reactive({
   #   pp_filtering <- DBI::dbGetQuery(conn, "SELECT idpp, activity_pt, lotes, shortlisting_pp, ifad_review, envelopes, basic_usd,  method_name_pt, procurement_stage, pp_comment, responsible, activ_id, activity, is_grant, non_consultancies, activity_description_pt, qualification, proc_methods, eo_i_submission, no_eo_i, inv_r_eo_i, close_r_eo_i, report_eo_i, no_eo_i_report, rfp_rno, rfp_no, invitation, closing, rno_eva_r, no_eva_r, full_eva_rno, full_eva_no, noita, contract_awards, negotiations, rno_contract, no_contract, signature, contract_n, vendor_id, contract_usd, contract_completion, lot, ifadpp_sheet, non_consulting FROM fiduciary.procurement_dossiers WHERE plan2 = 'Actual'")
   #   if(input$dossier_responsibles !="Todos"){pp_filtering <- pp_filtering[pp_filtering$responsible %in% input$dossier_responsibles,]}
   #   if(input$responsible_technician !="Todos"){pp_filtering <- pp_filtering[pp_filtering$responsible %in% input$responsible_technician,]}
   #   pp_filtering
   # })
   
   output$ppdossiers <- DT::renderDataTable({
     contratacoes <-   filtered_dossiers()
     names(contratacoes) <- c('Código',  'Ref.ª no PAAO',  'Actividade',  'Método',  'Orçamento (US$)',  'Responsável',  'Situação',  'Comentários',  'eo_i_submission',  'no_eo_i',  'inv_r_eo_i',  'close_r_eo_i',  'report_eo_i',  'no_eo_i_report',  'rfp_rno',  'rfp_no',  'invitation',  'closing',  'rno_eva_r',  'no_eva_r',  'full_eva_rno',  'full_eva_no',  'noita',  'contract_awards',  'negotiations',  'rno_contract',  'no_contract',  'signature',  'envelopes',  'contract_n',  'vendor_id',  'contract_usd',  'contract_completion',  'lot',  'ifadpp_sheet',  'ifad_review',  'qualification',  'proc_methods',  'non_consulting',  'activity_pt',  'lotes',  'is_grant',  'shortlisting_pp',  'non_consultancies',  'activity')
     datatable(contratacoes, rownames= FALSE, options = list(columnDefs = list(list(targets = c(8:44), visible = FALSE)))) %>% formatCurrency('Orçamento (US$)', '')
   })
   
   observeEvent(input$submeter_dossier, priority = 1, {
     SQL_ppdossiers <- filtered_dossiers()
     row_selection <- SQL_ppdossiers[input$ppdossiers_rows_selected, "activ_id"]
     
     qry = paste0("UPDATE fiduciary.procurement_dossiers SET  pp_comment = '", paste(input$pp_comment),"', ",
                  "last_updated =  '", paste(as.character(format(Sys.Date()))),"', ",
                  "procurement_stage = '", paste(input$procurement_stage),"', ",
                  "proc_methods = '", paste(input$proc_methods),"', ",
                  "shortlisting_pp =  '", paste(as.character(format(input$shortlisting_pp))),"', ",
                  "ifad_review =  '", paste(as.character(format(input$ifad_review))),"', ",
                  "qualification =  '", paste(as.character(format(input$qualification))),"', ",
                  "envelopes  =  ", input$envelopes,", ",
                  "eo_i_submission    =  '", lubridate::ymd(if_else(is.na(input$eo_i_submission), ymd(NA),input$eo_i_submission)),"', ",
                  "no_eo_i    =  '", lubridate::ymd(if_else(is.na(input$no_eo_i), ymd(NA),input$no_eo_i)),"', ",
                  "inv_r_eo_i   =  '", lubridate::ymd(if_else(is.na(input$inv_r_eo_i ), ymd(NA),input$inv_r_eo_i )),"', ",
                  "close_r_eo_i  =  '", lubridate::ymd(if_else(is.na(input$close_r_eo_i), ymd(NA),input$close_r_eo_i)),"', ",
                  "report_eo_i   =  '", lubridate::ymd(if_else(is.na(input$report_eo_i ), ymd(NA),input$report_eo_i )),"', ",
                  "no_eo_i_report   =  '", lubridate::ymd(if_else(is.na(input$no_eo_i_report ), ymd(NA),input$no_eo_i_report )),"', ",
                  "rfp_rno   =  '", lubridate::ymd(if_else(is.na(input$rfp_rno ), ymd(NA),input$rfp_rno )),"', ",
                  "rfp_no  =  '", lubridate::ymd(if_else(is.na(input$rfp_no), ymd(NA),input$rfp_no)),"', ",
                  "invitation   =  '", lubridate::ymd(if_else(is.na(input$invitation ), ymd(NA),input$invitation )),"', ",
                  "closing   =  '", lubridate::ymd(if_else(is.na(input$closing ), ymd(NA),input$closing )),"', ",
                  "rno_eva_r   =  '", lubridate::ymd(if_else(is.na(input$rno_eva_r ), ymd(NA),input$rno_eva_r )),"', ",
                  "no_eva_r   =  '", lubridate::ymd(if_else(is.na(input$no_eva_r ), ymd(NA),input$no_eva_r )),"', ",
                  "full_eva_rno  =  '", lubridate::ymd(if_else(is.na(input$full_eva_rno), ymd(NA),input$full_eva_rno)),"', ",
                  "full_eva_no  =  '", lubridate::ymd(if_else(is.na(input$full_eva_no), ymd(NA),input$full_eva_no)),"', ",
                  "noita   =  '", lubridate::ymd(if_else(is.na(input$noita ), ymd(NA),input$noita )),"', ",
                  "non_consulting =  '", paste(as.character(format(input$non_consulting))),"', ",
                  "updated_by = '", paste(input$updated_by),"'",
                  " WHERE idpp = '", paste(input$idpp),"'")
     
     dbExecute(pool, qry)
     removeModal()
     showModal(modalDialog(title=paste0("Parabéns. Dossier actualizado!"), br(), div(tags$b(paste0("SUCESSO!"), style = "color: green;"))))
   })
   
   observeEvent(input$submeter_dossier, priority = 1, {
     SQL_ppdossiers <- filtered_dossiers()
     row_selection <- SQL_ppdossiers[input$ppdossiers_rows_selected, "activ_id"]
     
     qry = paste0("UPDATE fiduciary.procurement_dossiers SET  pp_comment = '", paste(input$pp_comment),"', ",
                  "contract_usd = ", as.numeric(ifelse(is.na(input$contract_usd),0,input$contract_usd)),", ",
                  
                  "contract_awards   =  '", lubridate::ymd(if_else(is.na(input$contract_awards ), ymd(NA),input$contract_awards )),"', ",
                  "negotiations   =  '", lubridate::ymd(if_else(is.na(input$negotiations ), ymd(NA),input$negotiations )),"', ",
                  "rno_contract   =  '", lubridate::ymd(if_else(is.na(input$rno_contract ), ymd(NA),input$rno_contract )),"', ",
                  "no_contract   =  '", lubridate::ymd(if_else(is.na(input$no_contract ), ymd(NA),input$no_contract )),"', ",
                  "signature   =  '", lubridate::ymd(if_else(is.na(input$signature ), ymd(NA),input$signature )),"', ",
                  "contract_n  =  '", paste(as.character(format(input$contract_n ))),"', ",
                  "vendor_id  =  '", paste(as.character(format(input$vendor_id))),"', ",
                  "contract_completion  =  '", lubridate::ymd(if_else(is.na(input$no_eo_i), ymd(NA),input$no_eo_i)),"' ",
                  "WHERE lotes = '", paste(input$lotes),"'")
     
     dbExecute(pool, qry)
     removeModal()
     showModal(modalDialog(title=paste0("Parabéns. Dossier actualizado!"), br(), div(tags$b(paste0("SUCESSO!"), style = "color: green;"))))
   })
   
   
   aggreements <-  data.frame(dbGetQuery(conn, SQL("SELECT * from fiduciary.aggreements")))
   
   fieldsMandatory <- c("contract_status", "comments")
   observe({
     mandatoryFilled <-
       vapply(fieldsMandatory,
              function(x) {
                !is.null(input[[x]]) && input[[x]] != ""
              },
              logical(1))
     mandatoryFilled <- all(mandatoryFilled)
     shinyjs::toggleState(id = "submeter_aggreement", condition = mandatoryFilled)
   })
   
   entry_form_contractdossiers <- function(button_id){
     showModal(
       modalDialog(
         div(id=("entry_form_contractdossiers"),
             tags$head(tags$style(".modal-dialog{ width:800px}")),
             tags$head(tags$style(HTML(".shiny-split-layout > div {overflow: visible}"))),
             fluidPage(
               fluidRow(h3("ACTUALIZAR CONTRATAÇÕES"),
                        
                        splitLayout(cellWidths = c("100%"), cellArgs = list(style = "vertical-align: top"),
                        h4(HTML("<b>DADOS DE BÁSICOS</b>"))),
                        
                        splitLayout(cellWidths = c("100%"), cellArgs = list(style = "vertical-align: top"),
                                    textInput("contractdescription_en", "Objecto (Inglês)")),
                        br(),
                        hr(style="border-color: purple"),
                        h4(HTML("<b>NUMERAÇÃO DO CONTRATO</b>")),
                        splitLayout(cellWidths = c("50%", "50%"), cellArgs = list(style = "vertical-align: top"),
                        textInput("no_number", "Não objecção n.º"),
                        selectizeInput("contract_number", "Contrato n.º", multiple = FALSE, choices = c("",unique(aggreements$contract_number)))),
                        
                        h4(HTML("<b>ENDEREÇO DA CONTRATADA</b>")),
                        splitLayout(cellWidths = c("33.3333%", "33.3333%", "33.3333%"), cellArgs = list(style = "vertical-align: top"),
                                    textInput("supplier_address", "Endereço"),
                                    selectizeInput("supplier_country", "País", multiple = FALSE, choices = c("", countries)),
                                    textInput("supplier_city", "Cidade")),
                        
                        splitLayout(cellWidths = c("50%", "50%"), cellArgs = list(style = "vertical-align: top"),
                                    textInput("supplier_email", labelMandatory("Email")),
                                    textInput("supplier_phone", labelMandatory("Telefone"))),
                        
                        h4(HTML("<b>GESTOR DO CONTRATO</b>")),
                        splitLayout(cellWidths = c("100%"), cellArgs = list(style = "vertical-align: top"),
                                    selectizeInput("contract_manager", labelMandatory("Nome do Gestor do Contrato"), multiple = FALSE, choices =c("",staff_choices))),
                        
                        h4(HTML("<b>DATAS DO CONTRATO</b>")),
                        splitLayout(cellWidths = c("33.33333%", "33.33333%", "33.33333%", "33.33333%"), cellArgs = list(style = "vertical-align: top"),
                                    dateInput("sign_date", "Assinatura", format = "yyyy-mm-dd"),
                                    dateInput("start_date", "Início", format = "yyyy-mm-dd"),
                                    dateInput("end_date", "Fim", format = "yyyy-mm-dd")),
                        
                        h4(HTML("<b>PREÇO DO CONTRATO</b>")),
                        splitLayout(cellWidths = c("33.3333%", "33.3333%", "33.3333%", "33.3333%"), cellArgs = list(style = "vertical-align: top"),
                                    selectizeInput("currency", labelMandatory("Moeda"), multiple = FALSE, choices = c("",moeda)),
                                    numericInput("ammount", "Montante inicial", 0, min = 0, max = 100000000),
                                    numericInput("exchange_rate", "Câmbio para USD", 64.46, min = 0, max = 10000)),
                        
                        h4(HTML("<b>ADENDA AO CONTRATO</b>")),
                        splitLayout(cellWidths = c("33.3333%", "33.3333%", "33.3333%"), cellArgs = list(style = "vertical-align: top"),
                                    dateInput("revision_date", "Data de revisão", format = "yyyy-mm-dd"),
                                    numericInput("revised_ammount", "Montante revisto", 0, min = 0, max = 100000000),
                                    dateInput("revised_end", "Prazo revisto", format = "yyyy-mm-dd")),
                        
                        h4(HTML("<b>FONTES DE FINANCIAMENTO</b>")),
                        splitLayout(cellWidths = c("25%", "25%", "25%", "25%"), cellArgs = list(style = "vertical-align: top"),
                                    numericInput("ifadgrant_percent", "Donativo FIDA (%)", 0, min = 0, max = 100),
                                    numericInput("ifadloan_percent", "Crédito FIDA (%)", 0, min = 0, max = 100),
                                    numericInput("rpsf1_percent", "RPSF 1 (%)", 0, min = 0, max = 100),
                                    numericInput("rpsf2_percent", "RPSF 2 (%)", 0, min = 0, max = 100)),
                        
                        splitLayout(cellWidths = c("25%", "25%", "25%", "25%"), cellArgs = list(style = "vertical-align: top"),
                                    numericInput("government_percent", "Governo (%)", 0, min = 0, max = 100),
                                    numericInput("gcf_percent", "GCF (%)", 0, min = 0, max = 100),
                                    numericInput("beneficiary_percent", "Beneficiários (%)", 0, min = 0, max = 100),
                                    numericInput("others_percent", "Outros (%)", 0, min = 0, max = 100)),
                        
                        h4(HTML("<b>DESEMPENHO DO CONTRATO</b>")),
                        splitLayout(cellWidths = c("30%", "22.5%", "22.5%", "25%"), cellArgs = list(style = "vertical-align: top"),
                                    selectizeInput("contract_status", labelMandatory("Situação"), multiple = FALSE, choices = c("",contract_status)),
                                    numericInput("contract_perform", labelMandatory("Desempenho"), 0, min = 0, max = 4),
                                    numericInput("physical_compl", labelMandatory("Execução física"), 0, min = 0, max = 100),
                                    selectizeInput("risk_flag", labelMandatory("Risco"), multiple = FALSE, choices = c("",risk_level))),
                        
                        h4(HTML("<b>GARANTIA DEFINITIVA</b>")),
                        splitLayout(cellWidths = c("50%", "25%", "25%"), cellArgs = list(style = "vertical-align: top"),
                                    textInput("perfguar_numb", "Garantia n.º"),
                                    dateInput("perfguar_issuedate", "Emissão", format = "yyyy-mm-dd"),
                                    dateInput("perfguar_expdate", "Validade", format = "yyyy-mm-dd")),
                        
                        splitLayout(cellWidths = c("60%", "25%", "15%"), cellArgs = list(style = "vertical-align: top"),
                                    selectizeInput("perfguar_issuer", "Emissor", multiple = FALSE, choices = c("",guarantee_issuers)),
                                    selectizeInput("perfguar_type", "Tipo", multiple = FALSE, choices = c("",garantias)),
                                    numericInput("perfguar_value", "Valor", 0, min = 0, max = 6000000)),
                        
                        h4(HTML("<b>ADIANTAMENTOS AO CONTRATO</b>")),
                        splitLayout(cellWidths = c("25%", "30%", "45%"), cellArgs = list(style = "vertical-align: top"),
                                    numericInput("ammount_advanced", "Valor adiantado", 0, min = 0, max = 60000000),
                                    dateInput("advance_date", "Data do adiantamento", format = "yyyy-mm-dd"),
                                    textInput("advguar_numb", "Garantia n.º")),
                        
                        splitLayout(cellWidths = c("60%", "20%", "20%"), cellArgs = list(style = "vertical-align: top"),
                                    selectizeInput("advguar_issuer", "Emissor", multiple = FALSE, choices = c("",guarantee_issuers)),
                                    dateInput("advguar_issuedate", "Emissão", format = "yyyy-mm-dd"),
                                    dateInput("advguar_expdate", "Validade", format = "yyyy-mm-dd")),
                        
                        splitLayout(cellWidths = c("70%", "30%"), cellArgs = list(style = "vertical-align: top"),
                                    selectizeInput("advguar_type", "Tipo", multiple = FALSE, choices = c("",garantias)),
                                    numericInput("advguar_value", "Valor", 0, min = 0, max = 60000000)),
                        # dateInput("update_date", "Actualização", format = "yyyy-mm-dd"),
                        textAreaInput("comments", labelMandatory("COMENTÁRIOS"), placeholder = "", height = 100, width = "100%"),
                        
                        helpText(labelMandatory(""), paste("Campo Obrigatório!")),
                        actionButton("submeter_aggreement", "SALVAR", class = "btn-success")),
               easyClose = TRUE
             )
         )
       )
     )
   }
   
   filtered_aggreements <- reactive({
     procava_aggreements <- DBI::dbGetQuery(conn, "SELECT supplier_name, contract_number, contractdescription_pt, currency, revised_ammount, contract_status, contract_perform, physical_compl, risk_flag, ammount_advanced, comments, revised_end, contract_manager FROM fiduciary.aggreements")
     
     if(input$gestores_contractuais !="Todos"){procava_aggreements <- procava_aggreements[procava_aggreements$contract_manager %in% input$gestores_contractuais,]}
     procava_aggreements
   })
   
   output$contractdossiers <- DT::renderDataTable({
     contrato <-   filtered_aggreements() %>% select(-contract_manager)
     names(contrato) <- c('Contratada', 'Contrato n.º', 'Objecto', 'Moeda', 'Montante revisto', 'Situação', 'Desempenho', 'Execução física', 'Risco', 'Valor adiantado', 'Comentários', 'Prazo revisto')
     datatable(contrato, rownames= FALSE)
   })
   
   observeEvent(input$edit_aggreements, priority = 20,{
     SQL_aggreements <- aggreements
     showModal(
       if(length(input$contractdossiers_rows_selected) > 1 ){
         modalDialog(
           title = "ALERTA",
           paste("Por favor, selecione só uma linha." ),easyClose = TRUE)
       } else if(length(input$contractdossiers_rows_selected) < 1){
         modalDialog(
           title = "ALERTA",
           paste("Por favor, selecione só uma linha." ),easyClose = TRUE)
       })  
     
     if(length(input$contractdossiers_rows_selected) == 1 ){
       entry_form_contractdossiers("submit_aggreement")
       
       updateDateInput(session, "sign_date", value = SQL_aggreements[input$contractdossiers_rows_selected, "sign_date"])
       updateDateInput(session, "start_date", value = SQL_aggreements[input$contractdossiers_rows_selected, "start_date"])
       updateDateInput(session, "end_date", value = SQL_aggreements[input$contractdossiers_rows_selected, "end_date"])
       updateDateInput(session, "revision_date", value = SQL_aggreements[input$contractdossiers_rows_selected, "revision_date"])
       updateDateInput(session, "contract_number", value = SQL_aggreements[input$contractdossiers_rows_selected, "contract_number"])
       updateDateInput(session, "perfguar_issuedate", value = SQL_aggreements[input$contractdossiers_rows_selected, "perfguar_issuedate"])
       updateDateInput(session, "perfguar_expdate", value = SQL_aggreements[input$contractdossiers_rows_selected, "perfguar_expdate"])
       updateDateInput(session, "advance_date", value = SQL_aggreements[input$contractdossiers_rows_selected, "advance_date"])
       updateDateInput(session, "advguar_issuedate", value = SQL_aggreements[input$contractdossiers_rows_selected, "advguar_issuedate"])
       updateDateInput(session, "advguar_expdate", value = SQL_aggreements[input$contractdossiers_rows_selected, "advguar_expdate"])
       updateDateInput(session, "revised_end", value = SQL_aggreements[input$contractdossiers_rows_selected, "revised_end"])
       updateNumericInput(session, "revised_ammount", value = SQL_aggreements[input$contractdossiers_rows_selected, "revised_ammount"])
       updateNumericInput(session, "exchange_rate", value = SQL_aggreements[input$contractdossiers_rows_selected, "exchange_rate"])
       updateNumericInput(session, "ifadgrant_percent", value = SQL_aggreements[input$contractdossiers_rows_selected, "ifadgrant_percent"])
       updateNumericInput(session, "ifadloan_percent", value = SQL_aggreements[input$contractdossiers_rows_selected, "ifadloan_percent"])
       updateNumericInput(session, "currency", value = SQL_aggreements[input$contractdossiers_rows_selected, "currency"])
       updateNumericInput(session, "ammount", value = SQL_aggreements[input$contractdossiers_rows_selected, "ammount"])
       updateNumericInput(session, "rpsf1_percent", value = SQL_aggreements[input$contractdossiers_rows_selected, "rpsf1_percent"])
       updateNumericInput(session, "rpsf2_percent", value = SQL_aggreements[input$contractdossiers_rows_selected, "rpsf2_percent"])
       updateNumericInput(session, "government_percent", value = SQL_aggreements[input$contractdossiers_rows_selected, "government_percent"])
       updateNumericInput(session, "gcf_percent", value = SQL_aggreements[input$contractdossiers_rows_selected, "gcf_percent"])
       updateNumericInput(session, "beneficiary_percent", value = SQL_aggreements[input$contractdossiers_rows_selected, "beneficiary_percent"])
       updateNumericInput(session, "others_percent", value = SQL_aggreements[input$contractdossiers_rows_selected, "others_percent"])
       updateNumericInput(session, "contract_perform", value = SQL_aggreements[input$contractdossiers_rows_selected, "contract_perform"])
       updateNumericInput(session, "physical_compl", value = SQL_aggreements[input$contractdossiers_rows_selected, "physical_compl"])
       updateNumericInput(session, "perfguar_value", value = SQL_aggreements[input$contractdossiers_rows_selected, "perfguar_value"])
       updateNumericInput(session, "ammount_advanced", value = SQL_aggreements[input$contractdossiers_rows_selected, "ammount_advanced"])
       updateNumericInput(session, "advguar_value", value = SQL_aggreements[input$contractdossiers_rows_selected, "advguar_value"])
       updateSelectizeInput(session, "supplier_country", selected = SQL_aggreements[input$contractdossiers_rows_selected, "supplier_country"])
       updateSelectizeInput(session, "contract_manager", selected = SQL_aggreements[input$contractdossiers_rows_selected, "contract_manager"])
       updateSelectizeInput(session, "contract_status", selected = SQL_aggreements[input$contractdossiers_rows_selected, "contract_status"])
       updateSelectizeInput(session, "risk_flag", selected = SQL_aggreements[input$contractdossiers_rows_selected, "risk_flag"])
       updateSelectizeInput(session, "perfguar_issuer", selected = SQL_aggreements[input$contractdossiers_rows_selected, "perfguar_issuer"])
       updateSelectizeInput(session, "perfguar_type", selected = SQL_aggreements[input$contractdossiers_rows_selected, "perfguar_type"])
       updateSelectizeInput(session, "advguar_issuer", selected = SQL_aggreements[input$contractdossiers_rows_selected, "advguar_issuer"])
       updateSelectizeInput(session, "advguar_type", selected = SQL_aggreements[input$contractdossiers_rows_selected, "advguar_type"])
       updateTextInput(session, "supplier_address", value = SQL_aggreements[input$contractdossiers_rows_selected, "supplier_address"])
       updateTextInput(session, "supplier_city", value= SQL_aggreements[input$contractdossiers_rows_selected, "supplier_city"])
       updateTextInput(session, "supplier_email", value= SQL_aggreements[input$contractdossiers_rows_selected, "supplier_email"])
       updateTextInput(session, "supplier_phone", value= SQL_aggreements[input$contractdossiers_rows_selected, "supplier_phone"])
       updateTextInput(session, "no_number", value = SQL_aggreements[input$contractdossiers_rows_selected, "no_number"])
       updateTextInput(session, "perfguar_numb", value = SQL_aggreements[input$contractdossiers_rows_selected, "perfguar_numb"])
       updateTextInput(session, "advguar_numb", value = SQL_aggreements[input$contractdossiers_rows_selected, "advguar_numb"])
       updateTextInput(session, "supplier_name", value = SQL_aggreements[input$contractdossiers_rows_selected, "supplier_name"])
       updateTextInput(session, "contractdescription_en", value = SQL_aggreements[input$contractdossiers_rows_selected, "contractdescription_en"])
       updateTextAreaInput(session, "comments", value = SQL_aggreements[input$contractdossiers_rows_selected, "comments"])
     }
   })
   
   observeEvent(input$submeter_aggreement, priority = 1, {
     SQL_aggreements <- aggreements
     row_selection <- SQL_aggreements[input$contractdossiers_rows_selected, "contract_number"]
     
     qry = paste0("UPDATE fiduciary.aggreements SET  comments = '", paste(input$comments),"', ",
                  
                  "update_date =  '", paste(as.character(format(Sys.Date()))),"', ",
                  "contractdescription_en = '", paste(input$contractdescription_en),"', ",
                  "supplier_address = '", paste(input$supplier_address),"', ",
                  "supplier_country = '", paste(input$supplier_country),"', ",
                  "supplier_city = '", paste(input$supplier_city),"', ",
                  "supplier_email = '", paste(input$supplier_email),"', ",
                  "supplier_phone = '", paste(input$supplier_phone),"', ",
                  "contract_manager = '", paste(input$contract_manager),"', ",
                  "no_number = '", paste(input$no_number),"', ",
                  "sign_date = '", paste(input$sign_date),"', ",
                  "start_date = '", paste(input$start_date),"', ",
                  "end_date = '", paste(input$end_date),"', ",
                  "revised_ammount = '", paste(input$revised_ammount),"', ",
                  "exchange_rate = '", paste(input$exchange_rate),"', ",
                  "ifadgrant_percent = '", paste(input$ifadgrant_percent),"', ",
                  "ifadloan_percent = '", paste(input$ifadloan_percent),"', ",
                  "rpsf1_percent = '", paste(input$rpsf1_percent),"', ",
                  "rpsf2_percent = '", paste(input$rpsf2_percent),"', ",
                  "government_percent = '", paste(input$government_percent),"', ",
                  "gcf_percent = '", paste(input$gcf_percent),"', ",
                  "beneficiary_percent = '", paste(input$beneficiary_percent),"', ",
                  "others_percent = '", paste(input$others_percent),"', ",
                  "revision_date = '", paste(input$revision_date),"', ",
                  "contract_status = '", paste(input$contract_status),"', ",
                  "contract_perform = '", paste(input$contract_perform),"', ",
                  "physical_compl = '", paste(input$physical_compl),"', ",
                  "risk_flag = '", paste(input$risk_flag),"', ",
                  "perfguar_numb = '", paste(input$perfguar_numb),"', ",
                  "perfguar_issuedate = '", paste(input$perfguar_issuedate),"', ",
                  "perfguar_expdate = '", paste(input$perfguar_expdate),"', ",
                  "perfguar_issuer = '", paste(input$perfguar_issuer),"', ",
                  "perfguar_type = '", paste(input$perfguar_type),"', ",
                  "perfguar_value = '", paste(input$perfguar_value),"', ",
                  "ammount_advanced = '", paste(input$ammount_advanced),"', ",
                  "advance_date = '", paste(input$advance_date),"', ",
                  "advguar_numb = '", paste(input$advguar_numb),"', ",
                  "advguar_issuedate = '", paste(input$advguar_issuedate),"', ",
                  "advguar_expdate = '", paste(input$advguar_expdate),"', ",
                  "advguar_issuer = '", paste(input$advguar_issuer),"', ",
                  "advguar_type = '", paste(input$advguar_type),"', ",
                  "advguar_value = '", paste(input$advguar_value),"', ",
                  "revised_end = '", paste(input$revised_end),"' ",
                  
                  " WHERE contract_number = '", paste(input$contract_number),"'")
     
     dbExecute(pool, qry)
     removeModal()
     showModal(modalDialog(title=paste0("Parabéns. Contrato actualizado!"), br(), div(tags$b(paste0("SUCESSO!"), style = "color: green;"))))
   })
   
   
   linhas_contratos <- reactive({input$linhas_contratos})
   colunas_contratos <- reactive({input$colunas_contratos})
   numerica_contratos<- reactive({input$numerica_contratos})
   
   
   output$contract_categories <- DT::renderDataTable({
     
     dataset_selecionado <- long_aggreements
     sumario <- dataset_selecionado %>% group_by_(input$linhas_contratos, input$colunas_contratos) %>% summarise(valor_calculado = sum(revised_ammount))
     sumario <- sumario %>% pivot_wider(values_from = "valor_calculado", names_from = input$colunas_contratos)
     sumario[is.na(sumario)] <- 0
     sumario <- sumario %>% adorn_totals("col") %>% adorn_totals("row")
     
     datatable(sumario, extensions = 'Buttons', options = list(dom = "Blfrtip", 
                                                                          buttons = list("copy", list(extend = "collection", buttons = c("csv", "excel", "pdf"), text = "Download")), 
                                                                          lengthMenu = list( c(10, 20, -1), c(10, 20, "All")), pageLength = 10
     )) %>% formatCurrency(2:ncol(sumario), '')
   })
   
   output$component_contracts <- DT::renderDataTable({
     
     ##################  CONTRACTS COMPONENTS #######################
     totais <- long_aggreements %>% group_by(contract_status) %>% summarize(montante = sum(revised_ammount))%>%
       pivot_wider(names_from = "contract_status", values_from = "montante", values_fill = 0)
     tt_pct <- totais
     tt_pct <- tt_pct[,1:ncol(tt_pct)]/sum(long_aggreements$revised_ammount)*100
     tt_pct$components_pt <- "(%)"
     contract_components <- long_aggreements %>% group_by(components_pt, componentnum_pt, contract_status) %>% summarize(montante = sum(revised_ammount)) %>%
       pivot_wider(names_from = "contract_status", values_from = "montante", values_fill = 0)
     subcom_awpb <- read_feather("granular_awpb_2022.feather", columns = c("subcomp", "granularusd"))
     C1 <- contract_components %>% dplyr::filter(componentnum_pt == "Componente 1") %>% adorn_totals("row") %>% mutate(components_pt = replace(components_pt, components_pt == "Total", "Componente 1")) %>% arrange(components_pt)
     C2 <- contract_components %>% dplyr::filter(componentnum_pt == "Componente 2") %>% adorn_totals("row") %>% mutate(components_pt = replace(components_pt, components_pt == "Total", "Componente 2"))%>% arrange(components_pt)
     C3 <- contract_components %>% dplyr::filter(componentnum_pt == "Componente 3") %>% adorn_totals("row") %>% mutate(components_pt = replace(components_pt, components_pt == "Total", "Componente 3"))%>% arrange(components_pt)
     C4 <- contract_components %>% dplyr::filter(componentnum_pt == "Componente 4") %>% adorn_totals("row") %>% mutate(components_pt = replace(components_pt, components_pt == "Total", "Componente 4"))%>% arrange(components_pt)
     
     aggreement_components <- dplyr::bind_rows(C1, C2, C3, C4, totais, tt_pct)
     aggreement_components <- aggreement_components %>% dplyr::select(-2) %>% adorn_totals("col")
     aggreement_components$components_pt[is.na(aggreement_components$components_pt)] <- "Total"
     aggreement_components$col_share <- round(aggreement_components$Total/sum(long_aggreements$revised_ammount)*100,2)
     
     datatable(aggreement_components, extensions = 'Buttons', options = list(dom = "Blfrtip", 
                                                                          buttons = list("copy", list(extend = "collection", buttons = c("csv", "excel", "pdf"), text = "Download")), 
                                                                          lengthMenu = list( c(10, 20, -1), c(10, 20, "All")), pageLength = 10
     )) %>% 
       formatCurrency(2:ncol(aggreement_components), '')
   })
   
   
   output$ammount_contracted <- renderPlot({
     ballance <- long_aggreements %>% group_by(contract_manager, contract_status) %>% summarize(sum(revised_ammount)/1000)
     ballance$contract_status[ballance$contract_status == "Under implementation"] <- "Implementation" 
     ballance$contract_manager[ballance$contract_manager == ""] <- "Unknown" 
     ballance <- ballance %>% spread(contract_status, -contract_manager) %>% adorn_totals("col")
     ballance[is.na(ballance)] <- 0
     ballance$resolved <- ballance$Closed + ballance$Completed
     ballance <- ballance %>% arrange(-Total)
     ballance$contract_manager <- fct_reorder(ballance$contract_manager, ballance$Total, min)
     ballance <- ballance %>% mutate(place = if_else(row_number() == 1, 1.5, -1))
     ggplot(ballance, aes(x= contract_manager, y = Total)) + geom_bar(stat= "identity", col = "#c4fb83",fill="#c4fb83") +
       geom_bar(aes(x= contract_manager, y = resolved), stat= "identity", width = 0.5,col = "#10b005",fill="#10b005") + theme_void() +
       labs(x = "", y = "US$ nos contratos", caption = "Fonte: Actualizações dos Contratos @ FAR,FP - PROCAVA")+
       theme(axis.text.y = element_text(size = 10, hjust = 1), plot.margin = margin(rep(15, 4)))+
       geom_text(aes(label=round(Total), hjust = place), colour="#1f5502", size=3, position=position_dodge(width=0.1))+
       coord_flip()
     
   })
   
   
   output$signed_contracts <- renderPlot({
     ballance <- long_aggreements %>% group_by(contract_manager, contract_status) %>% summarize(n_distinct(contract_number))
     ballance$contract_status[ballance$contract_status == "Under implementation"] <- "Implementation" 
     ballance$contract_manager[ballance$contract_manager == ""] <- "Unknown" 
     ballance <- ballance %>% spread(contract_status, -contract_manager) %>% adorn_totals("col")
     ballance[is.na(ballance)] <- 0
     ballance$resolved <- ballance$Closed + ballance$Completed
     ballance <- ballance %>% arrange(-Total)
     ballance$contract_manager <- fct_reorder(ballance$contract_manager, ballance$Total, min)
     ballance <- ballance %>% mutate(place = if_else(row_number() == 1, 1.5, -1))
     ggplot(ballance, aes(x= contract_manager, y = Total)) + geom_bar(stat= "identity", col = "#c4fb83",fill="#c4fb83") +
       geom_bar(aes(x= contract_manager, y = resolved), stat= "identity", width = 0.5,col = "#10b005",fill="#10b005") + theme_void() +
       labs(x = "", y = "# de Contratos", caption = "Fonte: Actualizações dos Contratos @ FAR,FP - PROCAVA")+
       theme(axis.text.y = element_text(size = 10, hjust = 1), plot.margin = margin(rep(15, 4)))+
       geom_text(aes(label=round(Total), hjust = place), colour="#1f5502", size=3, position=position_dodge(width=0.1))+
       coord_flip()
     
   })

}
