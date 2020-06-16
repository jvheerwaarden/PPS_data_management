##### Minimal Reproducible Analysis Example
##### 2 - data cleaning

library(tidyverse)
library(readxl)
library(openxlsx)

# Load project functions
source("./scripts/MiRAE_funcs.R")


##### Inputs ---------------------------------------------------------------------------------------

output_worbook_name <- "WUR_ferlilizer_trials"
output_sheet_name <- "data"

##### Load data ------------------------------------------------------------------------------------

# Raw (initial processed) data set
d <- read.xlsx("./data/processed/Meststof proef WUR_metadata.xlsx",sheet="data")
metadata<- read.xlsx("./data/processed/Meststof proef WUR_metadata.xlsx",sheet="meta data")
variable_definition <- read.xlsx("./data/processed/Meststof proef WUR_metadata.xlsx",sheet="variable definitions")

##### Clean data -----------------------------------------------------------------------------------

## Inspect data
str(d)
# Here we can see that:
# - Opbrengst.Maïs: has multiple place holders for missing values
# - Boederij: some farm names are not properly capitalized
lapply(d, table)


## Set proper English variable names
names(d) <- c("field_id", "farm", "fertilizer", "yield")

## Correct values
d$farm[grep("de jong", d$farm)] <- "de Jong"
d$farm[grep("van de boer", d$farm)] <- "van de Boer"

## Make missing value placeholder uniform for yield
id_NA <- grep("^[^-]?\\d+([.]\\d+)?$", d$yield, invert = TRUE)
d$yield[id_NA] <- NA

## Transform variables

## Delete accent or special characters in fertilizer names
# here I simply overwrite an existing variable

# escaping the weird character to unicode representation
d$fertilizer <- stringi::stri_escape_unicode(d$fertilizer)

# replace the unicode representation by "e" yielding "Efficiencie"
d$fertilizer <- str_replace(d$fertilizer, "\\\\u00c3\\\\u0192\\\\u00c2\\\\u00ab", "e")

## Convert yield in ton per ha (tha),
# here I use dplyr::mutate as an example
d <- d %>%
  mutate(`yield$tha` = as.numeric(yield) / 10) %>%
  select(-yield) # get rid of initial yield column


##### Setup final workbook (data + metadata + variable definition) ---------------------------------


## Get variable names and units from data.frame column names
raw_variables <- strsplit(names(d), split = "\\$")
variables_names <- sapply(raw_variables, `[`, 1)
variables_unit <- sapply(raw_variables, `[`, 2)

## Create data.frame
variable_definition <- data.frame(workbook = rep(output_worbook_name,
                                                 length(variables_names)),
                                  sheet = rep(output_sheet_name,
                                              length(variables_names)),
                                  variable = variables_names,
                                  units = variables_unit)

## Manually define variables
definition <- c(field_id = "unique field identifier",
                yield = "maize yield",
                fertilizer = "type of fertilizer used",
                farm = "name of the farm where the experiment was performed")
## Turn definition into a proper data.frame
definition <- definition %>%
  data.frame(definition = .) %>%
  rownames_to_column("variable")

## Update variable definition data.frame
variable_definition <- inner_join(variable_definition,
                                  definition,
                                  by = "variable")
variable_definition <- variable_definition %>%
  mutate(
    # Use custom function to spot unique identifiers
    unique.id = detect_unique_ID(d),
    personal.information = ifelse(names(d) == "farm", 1, 0)
    )


##### Save data workbook ---------------------------------------------------------------------------

wb <- createWorkbook()

sheet_names <- c("data","meta data","variable definitions")
sheet_data <- list(d, metadata, variable_definition)

walk(sheet_names, ~ addWorksheet(wb, sheetName = .x))
walk2(sheet_names, sheet_data, ~ writeData(wb, sheet = .x, x = .y))

saveWorkbook(wb = wb,
             file = "./data/processed/fertilizer_trial_WUR_cleaned.xlsx",
             overwrite = TRUE)


