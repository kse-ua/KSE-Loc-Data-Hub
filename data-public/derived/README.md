# `data-public/derived/` Directory

This directory should contain only data files that can be derived from the raw data files (*i.e.*, those in [`data-public/raw/`](../../data-public/raw/))) **using code contained in this repository**. Unlike the raw data files, proprietary & binary formats are acceptable, since the repository's code should be able to reproduce them.

When using `R`, the \*.rds files are well-suited here, since they are smaller than CSV (thus quicker to load) and persist the metadata (such as factor labels).

the processed raw, unmodified files that serve as an input to the project. In theory the schema of these data files shouldn't change when new data arrive. But of course this is frequently violated, so at minimum, our code should assert that the required columns are present, and contain reasonable values. More thorough checking can be warranted.

For the sake of long-term reproducibility, these files are ideally in a nonproprietary format that is human readable. Plain text files (*e.g.*, CSVs & XML) are preferred. Binary & proprietary formats (*e.g.*, Excel & SAS) may not be readable if certain software is missing from the user's computer; or they might be able to be read by only old versions of software (*e.g.*, Excel 97).

## No PHI

Files with PHI should **not** be stored in a GitHub repository, even a private GitHub repository. We recommend using an enterprise database (such as MySQL or SQL Server) to store the data, and read & write the information to/from the software right before and after it's used. If a database isn't feasible, consider storing the files in [`data-private/`](../../data-private/), whose contents are not committed to the repository; a line in [`.gitignore`](../../.gitignore) keeps the files uncommitted/unstaged. However, there could be some information that is sensitive enough that it shouldn't even be stored locally without encryption (such as PHI).

# Datasets

| Type of data   | Dataset | File name | Description       | Script | Source |
|--------------|--------------|:--|--------------------------------------------|--------------|---------|
| Administrative  | Administrative Units                   | ua-admin-map-2020.csv | Relationship among multiple administrative levels (settlement, rada, hromada, raion, oblast, region) | `./manipulation/ellis-ua-admin.R` | Old and new State classifier of objects of the administrative and territorial system of Ukraine  |
| Administrative | Admin History                          |   time-rada.csv | Composition of hromadas (what radas comprise it) at every point in time when such composition changed, from 2014 to 2020 | `./manipulation/ellis-rada-hromada.R` | |
| Demography     | Population                             |     ua-pop-2022.csv | Population counts at the level of hromada (total and urban)  | `./manipulation/ellis-demography.R`           | State Statistics Service of Ukraine |
| Geographic | Geographic | geography.csv | Main spatial features of hromadas: area, coordinates of hromada center, travel time to oblast center, mountain hromadas, distance from hromada centers to the nearest point of the border with Russia, Russia or Belarus, or the EU; hromadas within 30 km of the sea/30 km of the border/30 km of the border with Russia and Belarus; hromadas within 15 km of international roads and national roads | `./manipulation/ellis-geography.R` |    |
| Geographic | Polygons | terhromad_fin.geojson | Spatial poligons of hromadas | | |
| Economic       | Taxes                            |   hromada_budget_2020_2022.csv | Grouped taxes at the hromada level, their share in own revenue, change for ---, and year-on-year change for different periods of 2020-2022 | `./manipulation/ellis-budget.R` | OpenBudget |
| Economic | Taxes |  hromada_budget_2020_2022_taxes.xlsx | Individual tax revenues for hromadas from Jan 2020 to Aug 2022 | `./manipulation/ellis-budget-2020-2022.R` | OpenBudget |
|Economic|Budget Expenses|hromada_expenses_2021_2022.xlsx|Hromadas budget expenses for 2021-2022 |ellis-expenses-2020-2022.R  |OpenBudget
| Economic               | SRDF |dfrr_hromadas.csv | Data on cost of the projects financed by the State Regional Development Fund | `./manipulation/ellis-dfrr.R` | Request to Ministry of Regional Development |
| Social Capital               | Community Competence | community-competence-hromada.csv | Number of youth centers, youth councils and centers for entrepreneurial support | `./manipulation/ellis-community-competence.R` |   |
| Health               | Health Declarations | declarations-hromada.csv | Number of declarations with health facilities as of February 2022 | `./manipulation/ellis-health.R` |     |
| Social Capital | OSBB (Homeowners Associations)| osbb-hromada.csv | Number of homeowners associations in 2015-2020 | `./manipulation/ellis-osbb.R`|  |
| Infrastructure | Passengers                             |                       | Number of passengers arriving at hromada railway stations in 2021 | `./manipulation/ellis-uz.R`                 |
|  Education              | ZNO (External Independent Examination) |                       | Mean scores of ZNO (standardized test) for main subjects                                                                                                                                                                                                                                                                                                                                               | `./manipulation/ellis-zno.R`                  |
| War-related | War zones |minregion-war-status.csv| Statuses of the war zone/occupation according to the Ministry of Reintegration  | `./manipulation/ellis-war-status.R` |Ministry of Reintegration |
| Citizen Participation | E-dem |edem-data.csv | Form of electronic participation in hromadas | `./manipulation/ellis-edem.R` | Scrapped from e-dem.ua |
| Political               | Hromada Mayors Bio |                       | Information on mayor of hromadas that were elected in 2020 local elections | `./manipulation/ellis-mayors.R` |
