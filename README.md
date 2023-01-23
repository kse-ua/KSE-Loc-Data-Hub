# Decentralization Reform in Ukraine (2014 -2022)

The project investigates the effects of restructurization of local councils (i.e. "rada", N = 11,250) into amalgamated territorial communities (i.e. "hromada", N = 1,469) and the effect of this tranformation on key macroeconomic and social indicators. Of specific interest is hromadas' resiliance to full-scale russian invasion launched on 2022-02-24.

The project is funded by GIZ within its project "Support to the Decentralisation Reform in Ukraine (UDU U-LEAD with Europe. Phase II) (#`81281025`)

![](https://www.nationsonline.org/maps/Ukraine-Administrative-Map.jpg)

| Name              | Role                                                            | \@         |
|------------------------|------------------------|------------------------|
| Tymofii Brik      | Lead Researcher                                                 |            |
| Andriy Darkovich  | Research Assistant                                              |            |
| Valentyn Hatsko   | Data Analyst                                                    | valgat29   |
| Andriy Koval      | Statistical modeling, data visualization, reproducible research | andkov     |
| Maryna Rabinovych | Senior Researcher                                               |            |
| Myroslava Savisko | Project Manager                                                 | splanetina |
| Serhii Tytiuk     | Data Analyst                                                    | tytser     |
| Igor Piddubniy    | Data Analyst                                                    | ipiddubnyi |

# Reports

-   [Resilience Survey overview](https://raw.githack.com/kse-ua/ua-de-center-serve/main/analysis/survey-overview/survey-overview.html) - Results and Key findings of the Resiliance Survey

# Data Products

| Data Product Name                      | Description                                                                                                              | Current State / Left To Do                                                                               | Script                                        |
|----------------|------------------|---------------------|------------------|
| Admininstrative Units                  | Relationship among multiple administrative levels (settlement, rada, hromada, raion, oblast, region)                     | ready for analysis at `./data-public/derived/ua-admin-map-2020.csv`                                      | `./manipulation/ellis-ua-admin.R`             |
| Admin History                          | Composition of hromadas (what radas comprise it) at every point in time when such composition changed, from 2014 to 2020 | push to ./data-public/derived/                                                                           | `./manipulation/ellis-rada-hromada.R`         |
| Population                             | Population counts at the level of hromada                                                                                | Add more years (currently only 2021), push to ./data-public/derived/                                     | `./manipulation/ellis-demography.R`           |
| Open Budget                            | Tax revenues of admin units                                                                                              | No clarity yet. Classify hromadas based on complexity of their admin history                             | `./manipulation/ellis-budget.R`               |
| Open Budget                            | Tax revenues of hromadas 2020-2022                                                                                       | ready for analysis at `./data-public/derived/hromada_budget_2020_2022.xlsx`                              | `./manipulation/ellis-budget-2020-2022.R`     |
| Geographic                             | Main spatial features of hromadas: area, coordinates of hromada center, travel time to oblast center                     | ready for analysis at `./data-public/derived/geography.csv`                                              | `./manipulation/ellis-geography.R`            |
| Geographic                             | Spatial poligons of hromadas                                                                                             | Demonstate graphing maps (at hromada-raion-oblast level) using a replacable quantifier (e.g. population) |                                               |
| Community Competence                   | Number of youth centers, youth councils and centers for entrepreneurial support                                          | ready for analysis at `./data-public/derived/community-competence-hromada.csv`                           | `./manipulation/ellis-community-competence.R` |
| Health Declarations                    | Number of declarations with health facilities as of February 2022                                                        | ready for analysis at `./data-public/derived/declarations-hromada.csv`                                   | `./manipulation/ellis-health.R`               |
| OSBB (Homeowners Associations)         | Number of homeowners associations in 2015-2020                                                                           | ready for analysis at `./data-public/derived/osbb-hromada.csv`                                           | `./manipulation/ellis-health.R`               |
| Passengers                             | Number of passengers in 2021                                                                                             | push to ./data-public/derived/                                                                           | `./manipulation/ellis-osbb.R`                 |
| ZNO (External Independent Examination) | Mean scores of ZNO (standardized test) for main subjects                                                                 | Add more years (currently only 2021), push to ./data-public/derived/                                     | `./manipulation/ellis-zno.R`                  |
| War zones                              | Statuses of the war zone/occupation according to the Ministry of Regional Development                                    | push to ./data-public/derived                                                                            | `./manipulation/ellis-war-status.R`           |
| E-dem                                  | Form of electronic participation in hromadas                                                                             | push to ./data-public/derived                                                                            | `./manipulation/ellis-edem.R`                 |
| DFRR                                   | Data on cost of the projects financed by the State Regional Development Fund                                             | ready for analysis                                                                                       | `./manipulation/ellis-dfrr.R`                 |
| Hromada Mayors Bio                     | Information on mayor of hromadas that were elected in 2020 local elections                                               | ready for analysis at `./data-public/raw/hromada_heads.xlsx`                                             | `./manipulation/ellis-mayors.R`               |
