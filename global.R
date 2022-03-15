library(bs4Dash, quietly = TRUE)
library(DT, quietly = TRUE)
library(pool, quietly = TRUE)
library(dplyr, quietly = TRUE)
library(tidyverse, quietly = TRUE)
library(DBI, quietly = TRUE)
library(RPostgres, quietly = TRUE)
library(RPostgreSQL, quietly = TRUE)
library(ggcharts, quietly = TRUE)
library(ggplot2, quietly = TRUE)
library(echarts4r)
library(janitor)
library(lubridate)
library(feather)
library(fst)
library(shinysurveys)
library(shinyvalidate)
library(extrafont, warn.conflicts = FALSE, quietly = T)
library(showtext, warn.conflicts = FALSE, quietly = T)
library(shinyjs, warn.conflicts = FALSE, quietly = T)
library(sodium, warn.conflicts = FALSE, quietly = T)
library(httr, warn.conflicts = FALSE, quietly = T)
library(bslib)
library(fresh)
library(splitstackshape, warn.conflicts = FALSE, quietly = T)
library(zoo)
library(data.table)
library(flextable)
library(glue)
library(shinyFiles)

bs4dash_font(size_base = "1.5rem", weight_bold = 900)
thematic::thematic_shiny(font = "auto")
options(scipen = 9999)
options(digits=15)
options(warn = 0)
e_rate = 64.46

title <- tags$a(href='https://www.google.com',
                tags$img(src="PROCAVA_LOGO.png", height = '92.5', width = '220'),
                '', target="_blank")

db <- 'mzprocava'  
host_db <- "mzprocava.c3n7ltv3naza.eu-west-1.rds.amazonaws.com"
db_port <- '5432'  
db_user <- "mzprocava"
db_password <- "GoMPROCAVA;2030"
conn <- dbConnect(RPostgres::Postgres(), dbname = db, host=host_db, port=db_port, user=db_user, password=db_password)
pool <- dbPool(RPostgres::Postgres(), dbname = db, host=host_db, port=db_port, user=db_user, password=db_password)

onStop(function() {poolClose(pool)})


staff_choices <-  data.frame(dbGetQuery(conn, SQL("SELECT staff_name_codes from fiduciary.procava_staff")))
risk_level <- c('Muito baixo'  =  'Very Low', 'Baixo'  =  'Low', 'Médio'  =  'Medium', 'Alto'  =  'High', 'Muito alto'  =  'Very High')
garantias <- c('Garantia bancária'  =  'Bank Guarantee', 'Seguro garantia'  =  'Insurance Guarantee', 'Numerário'  =  'Cash', 'Cheque visado'  =  'Certified Cheque')
contract_status <- c('Negociação'  =  'Negotiation', 'Assinado'  =  'Signed', 'VTA ou anotação'  =  'Clearance', 'Implementação'  =  'Under implementation', 'Concluído'  =  'Completed', 'Conflito'  =  'Pending, conflict', 'Cancelado'  =  'Cancelled', 'Fechado'  =  'Closed')
moeda <- c('USD', 'ZAR', 'GBP', 'EUR', 'MZN')
countries <-  data.frame(dbGetQuery(conn, SQL("SELECT country from fiduciary.countries")))
countries <-  as.character(unique(countries$country))

staff_choices <- as.character(unique(staff_choices$staff_name_codes))
aggreements <-  data.frame(dbGetQuery(conn, SQL("SELECT * from fiduciary.aggreements")))
guarantee_issuers <-  data.frame(dbGetQuery(conn, SQL("SELECT issuer_name from fiduciary.guarantee_issuers")))
guarantee_issuers <- as.character(unique(guarantee_issuers$issuer_name))

# staff_choices <-  data.frame(dbGetQuery(conn, SQL("SELECT staff_name_codes from fiduciary.procava_staff")))
pp_stages_pt <-  data.frame(dbGetQuery(conn, SQL("SELECT detailedstage_pt from fiduciary.pp_stages")))
pp_dataset <-  DBI::dbGetQuery(conn, "SELECT * from fiduciary.procurement_dossiers WHERE plan2 = 'Actual'")
pp_methods <- read_feather('procurement_deadlines.feather') %>% select(method_name_pt)
pp_method_names_pt <- as.list(unique(pp_methods$method_name_pt))
codes <- as.list(unique(pp_stages_pt$detailedstage_pt))
pp_responsibles <- as.list(unique(pp_dataset$responsible))  

labelMandatory <- function(label) {tagList(label,span("*", class = "mandatory_star"))}
appCSS <- ".mandatory_star { color: red; }"

disbursed <- dbGetQuery(conn, "SELECT contravalor_mzn, disbursed_usd from fiduciary.withdrawal_applications")
procurement_view <- dbGetQuery(conn, "SELECT * FROM fiduciary.procurement_view WHERE plan2 = 'Actual'")

awpb_for_summary <- dbGetQuery(conn, "SELECT * from procava.awpb_for_summary")

components_design <- read_feather("component_years_project.feather")  %>% select(subcomponent = components, total)
awpb_updated <-  dbGetQuery(conn, "SELECT * from procava.awpb_updates")
pagamentos_aprovados <- dbGetQuery(conn, "SELECT * from fiduciary.full_approved_payments")

paao_granulado <-  read_feather('granular_awpb_2022.feather')
procava_staff <-  dbGetQuery(conn, "SELECT * from fiduciary.procava_staff")

cost_tab <- read_feather('cost_tab.feather')
procava_cost_tabs <-  cost_tab
ced_codes <- read_feather("e_sistafe.feather") %>% select(ced = esistafe_key, e_sistafe_w_code)
paid_set <- pagamentos_aprovados
e_sistafe  <-  ced_codes %>% dplyr::select(esistafe_key=ced, description_e_sistafe=e_sistafe_w_code)
paid_set$quarters <- quarters(as.Date(paid_set$payment_date))

contractrecords_df <- read_feather('contractrecords_df.feather')
esistafe <- read_feather('esistafe.feather')
costtabs <- read_feather('costtabs.feather')
awpb_and_tompro <- read_feather('awpb_and_tompro.feather')

granular_awpb_2022  <- paao_granulado
components_design <- read_feather("component_years_project.feather") %>% select(subcomponent = components, PDR=total)
cost_tabs_pdr <- read_fst("summarycosttables.fst")

PDR_categories <- cost_tabs_pdr %>% dplyr::filter(class == "categories") %>% select(pdr_category = Expenditure, PDR = total_cost)
PDR_categories$pdr_category[PDR_categories$pdr_category == "Credit, Guarantee Funds"] <- "Credit Guarantee Funds"
PDR_categories$pdr_category[PDR_categories$pdr_category == "Training"] <- "Trainings"
PDR_categories$pdr_category[PDR_categories$pdr_category == "Workshop"] <- "Workshops"
PDR_categories$pdr_category[PDR_categories$pdr_category == "Works"] <- "Civil Works"

detailed_pp_stages <- as.list(unique(pp_stages_pt$detailedstage_pt))
pp_responsibles <- as.list(unique(pp_dataset$responsible)) 

codes <- as.list(unique(awpb_updated$awpb_id))
responsaveis <- as.list(unique(awpb_updated$internal_responsible))
management_units <- sort(c("URGPS", "UNGP", "URGPC", "URGN", "UPGPN"))

labelMandatory <- function(label) {tagList(label,span("*", class = "mandatory_star"))}

appCSS <- ".mandatory_star { color: red; ,}"
awpb_situacoes <- c("Não iniciada (atrasada)", "Não iniciada (dentro do prazo)", "Iniciada (Execução < 50%)", "Estado avançado (50% < Execução < 75%)", 
                    "Quase concluída (75% < Execução < 100%)", "Concluída  (Execução >= 100%)", "Cancelada")

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

awpb_updates <- dbGetQuery(conn, "SELECT awpb_id, descricao_da_actividade, unidades, current_year_budget_us, target, ungp, q1, q2, q3, q4, area,
                               situacao, comentarios , npmu_actuals, mpc_actuals, gaza_actuals, inhambane_actuals, maputo_actuals, sofala_actuals,
                               manica_actuals, tete_actuals, zambezia_actuals, nampula_actuals, cabo_delgado_actuals, niassa_actuals, urgps_actuals,
                               urgpc_actuals, urgpn_actuals, upgpn_actuals, internal_responsible, institution, relevance, critical_path, mader FROM procava.awpb_updates")

payment_requests  <- dbGetQuery(conn, "SELECT * FROM fiduciary.payment_requests")
approved_paymemts  <- dbGetQuery(conn, "SELECT * FROM fiduciary.approved_payments")
full_payments <- dbGetQuery(conn, "SELECT * FROM fiduciary.full_approved_payments")

procava_payments <- full_payments

procava_payments$quarters_paid <- as.yearqtr(as.Date(procava_payments$submission_date, "%m/%d/%Y"))
procava_payments <- concat.split(data = procava_payments, split.col = which(colnames(procava_payments) == "quarters_paid"), sep = " ", drop = FALSE)
setnames(procava_payments, c("quarters_paid_1", "quarters_paid_2"), c("year_paid", "quarter_paid"))

df <- read_feather('payments_survey.feather')
extendInputType("date",{shiny::dateInput(inputId = surveyID(),value = Sys.Date(), label = surveyLabel(), min = Sys.Date()-1100, max = Sys.Date()+1900)})
extendInputType("inlineRadioButtons", {shiny::radioButtons(inputId = surveyID(), label = surveyLabel(),selected = character(0),choices = surveyOptions(),inline = TRUE)})
extendInputType("commentsInputs", {shiny::textAreaInput("caption", "Caption", "Observações", width = "100%")})
extendInputType("exchange_rate", {shiny::numberInput(inputId = "exchange_rate", label = "Câmbio para o Dólar", value = 64.46,
                                                     min = 0, max = 1000000, step = 0.01, placeholder = NULL, width = NULL)})
extendInputType("checkboxinput", {
  shiny::checkboxGroupInput(inputId = surveyID(),label = surveyLabel(),choices = surveyOptions(),selected = NULL,
                            inline = FALSE,width = NULL,choiceNames = NULL,choiceValues = NULL)})

extendInputType("contract_choices", {
  db <- 'mzprocava'  
  host_db <- "mzprocava.c3n7ltv3naza.eu-west-1.rds.amazonaws.com"
  db_port <- '5432'  
  db_user <- "mzprocava"
  db_password <- "GoMPROCAVA;2030"
  pool <- dbPool(RPostgres::Postgres(), dbname = db, host=host_db, port=db_port, user=db_user, password=db_password)
  contratos <- dbGetQuery(pool, "SELECT supplier_vs_contract FROM fiduciary.procava_contracts")
  contract_choices <- unique(as.character(contratos$supplier_vs_contract))
  shiny::selectizeInput(inputId = surveyID(),label = surveyLabel(), choices = c("", contract_choices))
})

extendInputType("staff_choices", {
  db <- 'mzprocava'  
  host_db <- "mzprocava.c3n7ltv3naza.eu-west-1.rds.amazonaws.com"
  db_port <- '5432'  
  db_user <- "mzprocava"
  db_password <- "GoMPROCAVA;2030"
  pool <- dbPool(RPostgres::Postgres(), dbname = db, host=host_db, port=db_port, user=db_user, password=db_password)
  staff_codes <- dbGetQuery(pool, "SELECT staff_name_codes FROM fiduciary.procava_staff")
  staff_codes <- unique(as.character(staff_codes$staff_name_codes))
  shiny::selectizeInput(inputId = surveyID(),label = surveyLabel(), choices = c("", staff_codes))
})

extendInputType("awpb_codes_choices", {
  awpb_choices <- read_feather("awpb_and_costab_codes.feather")
  awpb_choices <- unique(as.character(awpb_choices$activity_choices))
  shiny::selectizeInput(inputId = surveyID(),label = surveyLabel(), choices = c("", awpb_choices))
})

extendInputType("unit_choices", {
  units <- read_fst("procava_units.fst")
  units_choices <- unique(as.character(units$unit_pt))
  shiny::selectizeInput(inputId = surveyID(),label = surveyLabel(), choices = c("", units_choices))
})

extendInputType("district_choices", {
  district_choices <- read_feather('odk_options.feather') %>% filter(list_name == "distrito_new")
  district_choices <- unique(as.character(district_choices$label_portuguese))
  shiny::selectizeInput(inputId = surveyID(),label = surveyLabel(), choices = c("", district_choices), multiple = TRUE)
})

variaveis_contratos <- c('Método de procurement' = "proc_method",
                         'Unidade Gestora (UGB)' = "cost_center",
                         'País da contratada' = "supplier_country",
                         'Gestor do Contrato' = "contract_manager",
                         'Infraestrutura' = "infrastructure",
                         'Categoria de procurement' = "procurement_type",
                         'Revisão' = "review",
                         'Situação do contrato' = "contract_status",
                         'Nível de risco' = "risk_flag")

valores_contratos <- c("Valor revisto" = "revised_ammount", 
                       "Valor pago" = "ammount_paid")

aggreements_dataset <-  DBI::dbGetQuery(conn, "SELECT awpb_role, componentnum_pt, components_pt, supplier_country, contract_manager, infrastructure, review, risk_flag, contract_manager, cost_center, revised_ammount, value_paid, contract_number, proc_method,
                                     contract_status, procurement_type, ifadgrant_percent, ifadloan_percent, rpsf1_percent, rpsf2_percent,
                                     government_percent, beneficiary_percent, gcf_percent, private_percent, others_percent FROM fiduciary.contracts_table")

valores_contratos <- c("Valor revisto" = "revised_ammount", 
                       "Valor pago" = "value_paid")


long_aggreements <- aggreements_dataset %>% pivot_longer(ifadgrant_percent:others_percent, names_to = "financier", values_to = "contribution") %>% filter(contribution>0)

long_aggreements$financier <- ifelse(long_aggreements$financier=="ifadgrant_percent", "IFAD Grant",
                                     ifelse(long_aggreements$financier=="ifadloan_percent", "IFAD Loan",
                                            ifelse(long_aggreements$financier=="rpsf1_percent", "RPSF 1st Allocation",
                                                   ifelse(long_aggreements$financier=="rpsf2_percent", "RPSF 2nd Allocation",
                                                          ifelse(long_aggreements$financier=="government_percent", "Government",
                                                                 ifelse(long_aggreements$financier=="beneficiary_percent", "Beneficiaries",
                                                                        ifelse(long_aggreements$financier=="gcf_percent", "GCF",
                                                                               ifelse(long_aggreements$financier=="private_percent", "Private Sector",
                                                                                      ifelse(long_aggreements$financier=="others_percent", "","Others")))))))))

long_aggreements$revised_ammount <-  long_aggreements$revised_ammount*long_aggreements$contribution/100
long_aggreements$value_paid <-  long_aggreements$value_paid*long_aggreements$contribution/100

