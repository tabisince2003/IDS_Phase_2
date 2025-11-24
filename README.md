# Pakistan Nutrition Analysis (2000-2024)

# Project Overview
Comprehensive analysis of malnutrition trends in Pakistan using UNICEF and FAOSTAT data.

##  Key Findings
- **46.7% reduction** in malnutrition (28.5% → 15.2%)
- **Strong correlation** with child stunting & women anemia
- **Infrastructure improvements** linked to better nutrition

##  Files Included
- `UNICEF_malnutrition.csv` - Malnutrition data
- `FAOSTAT_data_en_10-12-2025.csv` - Food security indicators
- `analysis_code.R` - Complete analysis code
- `Nutrition_Report.html` - Interactive report

##  Quick Start
1. Place CSV files in working directory
2. Run `analysis_code.R` in RStudio
3. Open `Nutrition_Report.html` for results

##  Outputs Generated
- 5 Visualization graphs (PNG)
- Interactive HTML report
- Statistical analysis results
- Progress metrics (2000-2024)

##  Requirements
```r
install.packages(c("tidyverse", "ggplot2", "corrplot", "gridExtra"))
```

