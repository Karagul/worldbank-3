# Install
Clone this repo, open with RStudio and build package.

# Functions

 - GetWorldBankList 
   - example: `wb.tables <- GetWorldBankList()`
   - data frame(indicator, id, table.name)
 - GetRawTableFromID 
   - example: `wb.economy.central.gov.finances <- GetRawTableFromID("4.12")`
   - data.frame with with same columns and rows as [worldbank data](http://wdi.worldbank.org/table/4.12).

# References
http://wdi.worldbank.org/tables

