library(officedown)
library(officer)
library(tidyverse)
library(tidyverse)
library(readxl)
library(splitstackshape)
library(data.table)
library(readr)
library(feather)
library(stringr)
library(lubridate)
library(bizdays)
library(dplyr)
library(tibbletime)
library(openxlsx)
library(pivottabler)
library(janitor)
library(lubridate)
library(bizdays)
library(xlsx)
library(DBI) ## Connection to database
library(RPostgres)
library(xlsx)
library(openxlsx)
library(shinyjs)
library(sodium)
library(powerjoin)
library(dbplyr)
library(lubridate)
library(flextable)
library(glue)

options(scipen = 999)
options(digits=15)
exchangerate <- 64.46

db <- 'mzprocava'  #provide the name of your db
host_db <- "localhost" #i.e. # i.e. 'ec2-54-83-201-96.compute-1.amazonaws.com'
db_port <- '5432'  # or any other port specified by the DBA
db_user <- "postgres"
db_password <- "20122014"
con <- dbConnect(RPostgres::Postgres(), dbname = db, host=host_db, port=db_port, user=db_user, password=db_password)

# payments_data <- tbl(con, in_schema("fiduciary", "full_payment_dataset")) 
# payments_data <- dbGetQuery(con, "SELECT * FROM fiduciary.payment_requests")

# write.csv(colnames(payments_data), "C:\\Users\\Administrator\\Desktop\\payment_columns.csv")

payments_data <- dbGetQuery(con, "SELECT * FROM fiduciary.full_payment_dataset") %>% filter(processo_numero == "055/UNGP/PMA&GC/032.22/2022")

sample_doc <- read_docx("C:\\Users\\Administrator\\Dropbox\\2022_apps\\pmu\\Modelo_IP_RMarkdown_Final.docx")

payments_data <- payments_data %>% mutate_if(is.numeric, ~replace_na(., 0))
payments_summary <- payments_data %>% group_by(e_sistafe_w_code) %>% summarize(qty_10 = sum(quantity10), 
                                                                               qty_30 = sum(quantity30), 
                                                                               qty_100 = sum(quantity100),
                                                                               valor = sum(paid_ammount))

payments_summary$preco <- payments_summary$valor/(payments_summary$qty_10+payments_summary$qty_30+payments_summary$qty_100)
payments_summary <-  modify_if(payments_summary, ~is.numeric(.), ~round(., 2))

payments_summary[is.na(payments_summary)] <- 0
payments_summary <- payments_summary %>% adorn_totals("row")
# payments_summary <- payments_summary %>% mutate(ced = as.numeric(str_extract_all(payments_summary$e_sistafe_w_code, "[0-9]+")[[1]]))

payments_summary$ced<-gsub("[^0-9]", "", payments_summary$e_sistafe_w_code)

payments_summary$ced <- ifelse(payments_summary$e_sistafe_w_code == "Total", "-",  payments_summary$ced)
payments_summary <- payments_summary %>% select(ced, e_sistafe_w_code, qty_10, qty_30, qty_100, preco, everything())



payments_summary$ced <- as.character(payments_summary$ced) 
setnames(payments_summary, c("ced", "e_sistafe_w_code", "qty_10", "qty_30", "qty_100", "preco", "valor"), 
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
print(sample_doc, target = file.path(Sys.getenv("HOME"), "IP_RMarkdown_Final.docx"))
