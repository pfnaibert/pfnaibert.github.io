# packages
library(readxl)

# ?read_excel()
# ?read_xls()
# ?read_xlsx()

# Import
PIB <- read_excel("PIB_BASE_2000.xlsx")

# number of ROWS and COLS
NCOL(PIB); NROW(PIB)

# quick visualizations
head(PIB)
tail(PIB)

# Names of cols and rows
colnames(PIB)
rownames(PIB)

# lets trim the last two lines
PIB <- head(PIB, -2)

# lets give the rows names
rownames(PIB) <- PIB[,1]

# lets delete the first column
PIB <-PIB[,-1]

# number of ROWS and COLS
NCOL(PIB); NROW(PIB)





