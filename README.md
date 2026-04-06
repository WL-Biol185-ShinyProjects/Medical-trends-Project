# Parkinson's Disease & Environmental Factors Dashboard

## Project Summary

This is an interactive R Shiny web application that explores potential relationships between agricultural pesticide exposure, farm density, and neurological health outcomes across the United States. Using state- and county-level data, the dashboard visualizes geographic patterns in Parkinson's disease mortality, runs correlation and regression analyses between pesticide use and health outcomes, and applies ANOVA with Tukey post-hoc tests to compare health outcomes across low, medium, and high pesticide exposure groups. The goal of this project is to surface potential environmental risk patterns associated with Parkinson's disease while acknowledging the observational nature of the data and the many confounding factors at play.

The app is publicly deployed and accessible at:
**https://positconnect.wlu.edu/content/0c6f6fd5-759d-43ef-ba42-0739756bd983**

---

## About the Creators

This app was created as a final project for BIOL-185: Exploring Big Data at Washington & Lee University.

**Ashley Ellis**
I am a current senior at Washington & Lee University studying Engineering Integrated with Biology. My career and research interests center on neuroengineering and hardware-based medical device development, so exploring the intersection of environmental risk factors and neurological disease for this project was a great way to tie in my interests while learning new skills.
**Robert Bernote**
I am a current student at Washington & Lee University. This project gave me the opportunity to explore how large-scale agricultural and public health datasets can be combined to investigate environmental contributors to disease. I enjoyed the challenge of wrangling messy real-world data and turning it into something visually accessible and analytically meaningful.

**Georgia Busbee**
I am a current student at Washington & Lee University. I was drawn to this project because Parkinson's disease has a personal significance to me, and I wanted to understand whether the data could tell us anything meaningful about its geographic distribution and potential environmental drivers. Building this dashboard reinforced how important it is to communicate statistical findings carefully and honestly, especially when the results are mixed.

---

## AI Tools Used

Claude (Anthropic) was used throughout the development of this project as a coding assistant. We used it as a tool to translate our analytical ideas into functional R and Shiny code, debug errors, refine the dashboard UI, and think through the appropriate statistical methods for our research questions. All analytical decisions, interpretations, and conclusions are our own.

---

## About the Data

This dashboard integrates five primary datasets covering Parkinson's disease mortality, pesticide usage, farm statistics, life expectancy, and county geographic coordinates across the United States.

| Dataset | Description |
|---|---|
| `Parkinsons_mortality_rates_clean.csv` | State-level Parkinson's disease average death rates |
| `county_pesticides_data_clean.csv` | County-level pesticide usage estimates (LOW, AVG, HIGH) for four compounds |
| `state_pesticide_data_clean.csv` | State-level pesticide usage estimates aggregated by compound |
| `ExpectancyData_clean.csv` | County-level average life expectancy |
| `LifeExpectancyStateData_clean.csv` | State-level average life expectancy |
| `Farm_Data_2024.csv` | State-level farm counts and acreage |
| `cfips_location.csv` | County FIPS codes with latitude and longitude coordinates |

---

## Statistical Methods

The following analyses are implemented in the dashboard:

- **Pearson correlation** between pesticide use and health outcomes at the state and county level
- **Linear regression** modeling health outcomes as a function of pesticide exposure, with log transformation applied at the county level due to skewness in pesticide distribution
- **One-way ANOVA** comparing health outcomes across Low, Medium, and High pesticide exposure tertiles (defined by 33rd and 66th percentile cutoffs within each compound)
- **Tukey HSD post-hoc tests** identifying which specific exposure group pairs drive significant ANOVA results, with adjusted p-values correcting for multiple comparisons

---

## Tech Stack

- **R / RStudio**
- **Shiny** — web application framework
- **plotly** — interactive scatter plots and box plots
- **leaflet** — interactive choropleth maps
- **sf / maps** — spatial polygon rendering for state and county boundaries
- **DT** — interactive data tables
- **dplyr / tidyr** — data wrangling and transformation

---

## How to Run Locally

1. Clone this repository
2. Open the project in RStudio
3. Ensure the following R packages are installed:

```r
install.packages(c("shiny", "ggplot2", "plotly", "dplyr", "DT",
                   "leaflet", "sf", "maps", "tibble", "htmltools"))
```

4. Place all CSV data files in the same directory as `ui.R` and `server.R`
5. Run the app with:

```r
shiny::runApp("Clean Datasets and website code")
```

---

## Acknowledgements & Bibliography

We would like to acknowledge our use of Claude.ai (Anthropic) during the coding and development of this app. We would also like to thank and acknowledge the help of our professor, Professor Whitworth, who guided the development of this dashboard.

**Data Sources:**

Centers for Disease Control and Prevention. (n.d.). *Parkinson's disease mortality data by state.* CDC Wonder. https://wonder.cdc.gov/

Centers for Disease Control and Prevention. (n.d.). *United States life expectancy data by state and county.* CDC Wonder. https://wonder.cdc.gov/

U.S. Department of Agriculture, National Agricultural Statistics Service. (n.d.). *Pesticide use estimates and farm statistics by county and state.* USDA NASS. https://www.nass.usda.gov/

U.S. Census Bureau. (n.d.). *County FIPS codes and geographic coordinates.* United States Census Bureau. https://www.census.gov/

---

## Important Limitations

- All findings represent **observational associations** and do not establish causation
- Pesticide use estimates carry inherent uncertainty (LOW, AVG, and HIGH estimate ranges)
- State-level ANOVA analyses are underpowered due to small group sizes (~15-17 states per tertile)
- Geographic confounding is present throughout — agricultural regions differ systematically in demographics, healthcare access, income, and other factors that independently affect health outcomes
- Aggregated county and state data may mask important local variation