---
title: "Survey Deoccupied Hromadas"
author:
- Valentyn Hatsko
date: 'Last updated: 2023-02-15'
always_allow_html: true
output:
  html_document:
    keep_md: yes
    toc: yes
    toc_float: yes
    code_folding: hide
    theme: cerulean
    highlight: zenburn
  pdf_document:
    toc: yes
    latex_engine: xelatex
  word_document:
    toc: yes
editor_options:
  chunk_output_type: console
---

> This report visualizes key information about Resilience Survey

***Important Definitions***

> Research Sample: Hromadas who responded to the survey.

<!--  Set the working directory to the repository's base directory; this assumes the report is nested inside of two directories.-->







# Environment

> Reviews the components of the working environment of the report. Non-technical readers are welcomed to skip. Come back if you need to understand the origins of custom functions, scripts, or data objects.

<details>

<summary>Packages used </summary>

Packages used in current report



</details>

<details>

<summary>External scripts </summary>

Collection of custom functions used in current repository (`sda-information-requests`)



</details>

<details>

<summary>Global values </summary>

Values used throughout the report.



</details>

<details>

<summary>Functions </summary>

Custom functions defined for use in this report.



</details>

# Data

## Input



<details>

<summary>click to glimpse </summary>


```{.r .fold-show}
ds_survey %>% glimpse()
```

```
Rows: 138
Columns: 260
$ index                                              <dbl> 12, 82, 85, 93, 102, 23, 61, 133, 140, ~
$ today                                              <dttm> 2022-10-19, 2022-11-01, 2022-11-01, 20~
$ `_id`                                              <dbl> 194017568, 196954519, 196999969, 197222~
$ hromada_code                                       <chr> "UA05100110000070795", "UA0510017000007~
$ hromada_name                                       <chr> "Студенянська", "Шпиківська", "Калинівс~
$ hromada_full_name                                  <chr> "Студенянська сільська громада", "Шпикі~
$ raion_code                                         <chr> "UA05100000000022396", "UA0510000000002~
$ raion_name                                         <chr> "Тульчинський", "Тульчинський", "Хмільн~
$ oblast_code                                        <chr> "UA05000000000010236", "UA0500000000001~
$ oblast_name                                        <chr> "Вінницька", "Вінницька", "Вінницька", ~
$ region_en                                          <chr> "Center", "Center", "Center", "Center",~
$ region_en.y                                        <chr> "Center", "Center", "Center", "Center",~
$ deoccupied_at_feb_2023                             <dbl> 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, ~
$ type                                               <chr> "сільська", "селищна", "міська", "сільс~
$ occupation                                         <chr> "not_occupied", "not_occupied", "not_oc~
$ military_action                                    <chr> "no_combat", "no_combat", "no_combat", ~
$ population_text                                    <dbl> 6422, 18000, 65000, 7362, 21900, 3240, ~
$ partners_text                                      <dbl> 0, 0, 0, 0, 1, 0, 3, 1, 0, 2, 0, 0, 1, ~
$ friends_text                                       <dbl> 0, 0, 0, 0, 1, 0, 17, 10, 0, 1, 0, 1, 0~
$ state_communication                                <chr> "yes", "yes", "yes", "no", "no", "no", ~
$ prep_first_aid_water                               <dbl> 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, ~
$ prep_first_aid_fuel                                <dbl> 1, 1, 1, 0, 1, 1, 1, 1, 1, 1, 2, 1, 1, ~
$ prep_reaction_plan                                 <dbl> 1, 1, 2, 2, 2, 1, 2, 2, 1, 0, 2, 1, 1, ~
$ prep_evacuation_plan                               <dbl> 1, 0, 2, 2, 2, 1, 1, 1, 1, 0, 2, 1, 2, ~
$ prep_reaction_plan_oth_hromadas                    <dbl> 1, 0, 1, 0, 0, 1, 1, 0, 1, 0, 1, 0, 2, ~
$ prep_reaction_plan_oda                             <dbl> 1, 1, 1, 1, 0, 1, 1, 0, 1, 1, 2, 1, 2, ~
$ prep_dftg_creation                                 <dbl> 1, 1, 1, 1, 1, 2, 2, 1, 1, 1, NA, 1, 1,~
$ prep_national_resistance                           <dbl> 1, 0, 1, 1, 1, 0, 1, 1, 1, 0, 1, 1, 0, ~
$ prep_starosta_meeting                              <dbl> 1, 1, 2, 1, 1, 2, 1, 1, 2, 0, 2, 1, 1, ~
$ prep_communal_meetiing                             <dbl> 1, 1, 2, 1, 1, 2, 1, 1, 2, 1, 2, 1, 1, ~
$ prep_online_map                                    <dbl> 0, 0, 1, 0, 1, 1, 1, 1, 1, 0, 2, 0, 1, ~
$ prep_shelter_list                                  <dbl> 0, 0, 1, 1, 1, 1, 1, 1, 1, 0, 2, 1, 1, ~
$ prep_notification_check                            <dbl> 1, 1, 2, 1, 2, 1, 1, 1, 1, 0, 2, 1, 2, ~
$ prep_backup                                        <dbl> 0, 0, NA, 0, 0, 1, 2, 0, 1, 1, 2, 0, 2,~
$ prep_partly_backup                                 <dbl> 1, 1, NA, 1, 0, 1, 2, 0, 1, 1, NA, 1, 2~
$ shelter_capacity_before_text                       <chr> "200", "948", "13500", "280", "2,4 тис.~
$ shelter_capacity_before_coded                      <dbl> 200, 948, 13500, 280, 2400, 1, 31500, 5~
$ shelter_capacity_now_text                          <chr> "565", "1408", "10000", "675", "3,0 тис~
$ shelter_capacity_now_coded                         <dbl> 565, 1408, 10000, 675, 3000, 50, 216000~
$ telegram                                           <dbl> 0, 1, 0, 0, 1, 0, 2, 1, 0, 2, 2, 1, 1, ~
$ viber                                              <dbl> 2, 1, 2, 1, 0, 2, 0, 0, 1, 0, 2, 0, 1, ~
$ facebook                                           <dbl> 2, 2, 2, 2, 2, 2, 2, 2, 1, 2, 2, 2, 2, ~
$ chat_help                                          <dbl> 1, 1, 0, 0, 0, 0, 2, 1, 0, 2, 0, 0, 0, ~
$ hotline                                            <dbl> 2, 1, 2, 0, 0, 1, 2, 1, 1, 0, 0, 2, 1, ~
$ telegram_link                                      <chr> NA, "Група для оповіщення в Шпиківській~
$ facebook_link                                      <chr> "https://www.facebook.com/studenyanska"~
$ head_hromada_communication                         <chr> "none", "few_times_a_week", "once_a_day~
$ dftg_creation                                      <chr> "yes", "yes", "yes", "yes", "yes", "yes~
$ dftg_creation_date                                 <dttm> 2022-08-30, 2022-07-07, 2022-03-01, 20~
$ help_for_military                                  <chr> "rooms transport money products", "room~
$ `help_for_military/rooms`                          <dbl> 1, 1, 1, 1, 1, 0, 0, 0, 1, 0, 1, 1, 0, ~
$ `help_for_military/transport`                      <dbl> 1, 1, 0, 0, 1, 1, 1, 0, 1, 0, 1, 1, 0, ~
$ `help_for_military/money`                          <dbl> 1, 1, 1, 1, 0, 1, 1, 1, 0, 1, 1, 0, 1, ~
$ `help_for_military/products`                       <dbl> 1, 1, 1, 1, 1, 1, 1, 0, 1, 1, 1, 1, 1, ~
$ `help_for_military/other`                          <dbl> 0, 0, 0, 0, 1, 0, 1, 0, 1, 0, 1, 0, 0, ~
$ `help_for_military/none`                           <dbl> 0, 0, 0, 0, 0, 0, 0, 0, 1, 0, 0, 0, 0, ~
$ help_for_military_text                             <chr> NA, NA, NA, NA, "Генератори, прилади ні~
$ transport_help_communal                            <chr> "1", "0", NA, NA, "2", "2", "-", NA, "1~
$ transport_help_bought                              <chr> "2", "20", NA, NA, "1", "2", "5", NA, "~
$ transport_help_communal_coded                      <dbl> 1, 0, NA, NA, 2, 2, 0, NA, 1, NA, 5, 10~
$ transport_help_bought_coded                        <dbl> 2, 20, NA, NA, 1, 2, 5, NA, 0, NA, 0, 0~
$ percent_working_march                              <dbl> 100, 100, 80, 100, 100, 100, 50, 95, 10~
$ percent_working_now                                <dbl> 100, 100, 100, 100, 100, 100, 95, 98, 9~
$ commun_between_hromadas                            <chr> "Once a month and less", "Several times~
$ evacuation                                         <chr> "no", "no", "no", "no", "no", "no", "no~
$ idp_accept                                         <chr> "yes", "yes", "yes", "yes", "yes", "yes~
$ idp_registration_date                              <dttm> 2022-02-28, 2022-02-25, 2022-03-24, 20~
$ idp_registration_number                            <dbl> 565, 1765, 3428, 467, 1200, 23, 20000, ~
$ idp_real_number                                    <dbl> 362, 1765, 3428, 500, 1200, 23, 60000, ~
$ idp_help                                           <chr> "communal_placement private_placement h~
$ `idp_help/communal_placement`                      <dbl> 1, 1, 1, 0, 1, 1, 1, 1, 0, 1, 1, 1, 0, ~
$ `idp_help/private_placement`                       <dbl> 1, 1, 1, 1, 1, 1, 0, 0, 0, 0, 0, 1, 1, ~
$ `idp_help/regular_meal`                            <dbl> 0, 0, 1, 0, 1, 1, 1, 1, 0, 0, 0, 1, 0, ~
$ `idp_help/humanitar_help`                          <dbl> 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, ~
$ `idp_help/fundraising`                             <dbl> 1, 0, 1, 0, 1, 0, 0, 0, 0, 0, 0, 0, 0, ~
$ `idp_help/employ`                                  <dbl> 0, 0, 0, 0, 0, 0, 1, 1, 0, 0, 1, 0, 0, ~
$ `idp_help/psych_help`                              <dbl> 1, 1, 1, 0, 0, 0, 1, 1, 0, 1, 1, 1, 1, ~
$ `idp_help/law_help`                                <dbl> 1, 1, 1, 0, 0, 0, 1, 1, 1, 0, 1, 1, 0, ~
$ `idp_help/transit_center`                          <dbl> 0, 0, 0, 0, 0, 0, 0, 1, 0, 0, 1, 0, 0, ~
$ idp_place_rooms                                    <chr> "0_100_beds", "0_100_beds", "0_100_beds~
$ idp_room_number                                    <chr> NA, NA, NA, NA, NA, NA, "більше 1000", ~
$ idp_child_education                                <dbl> 15, 9, 179, 36, 30, 0, 800, 369, 2, 10,~
$ special_fund_relocation                            <chr> "yes", "no", "no", "no", "yes", "yes", ~
$ special_fund_relocation_needs                      <chr> "utilities", NA, NA, NA, "public_order ~
$ `special_fund_relocation_needs/state_functions`    <dbl> 0, NA, NA, NA, 0, 0, 0, NA, NA, 0, 0, N~
$ `special_fund_relocation_needs/defense`            <dbl> 0, NA, NA, NA, 0, 1, 1, NA, NA, 0, 1, N~
$ `special_fund_relocation_needs/public_order`       <dbl> 0, NA, NA, NA, 1, 0, 0, NA, NA, 1, 1, N~
$ `special_fund_relocation_needs/economic_activity`  <dbl> 0, NA, NA, NA, 0, 0, 0, NA, NA, 0, 0, N~
$ `special_fund_relocation_needs/environment`        <dbl> 0, NA, NA, NA, 0, 0, 0, NA, NA, 0, 0, N~
$ `special_fund_relocation_needs/utilities`          <dbl> 1, NA, NA, NA, 1, 0, 1, NA, NA, 0, 0, N~
$ `special_fund_relocation_needs/spirit_development` <dbl> 0, NA, NA, NA, 0, 0, 0, NA, NA, 0, 0, N~
$ `special_fund_relocation_needs/education`          <dbl> 0, NA, NA, NA, 1, 0, 1, NA, NA, 0, 0, N~
$ `special_fund_relocation_needs/social_protection`  <dbl> 0, NA, NA, NA, 0, 0, 1, NA, NA, 0, 0, N~
$ `special_fund_relocation_needs/healthcare`         <dbl> 0, NA, NA, NA, 0, 0, 1, NA, NA, 0, 0, N~
$ relocated_companies_text                           <chr> "0", "0", "5", "0", "0", "0", "60", "10~
$ created_jobs                                       <chr> "0_50_jobs", "dk", "dk", "dk", "0_50_jo~
$ bussiness_stimules                                 <chr> "other", "free_rooms", "other", "educat~
$ `bussiness_stimules/tax_benefits`                  <dbl> 0, 0, 0, 0, 0, 0, 0, 1, 0, 0, 1, 0, 0, ~
$ `bussiness_stimules/free_rooms`                    <dbl> 0, 1, 0, 0, 0, 1, 0, 0, 1, 0, 0, 0, 1, ~
$ `bussiness_stimules/education`                     <dbl> 0, 0, 0, 1, 0, 0, 1, 1, 0, 1, 0, 1, 0, ~
$ `bussiness_stimules/other`                         <dbl> 1, 0, 1, 0, 1, 0, 0, 0, 0, 0, 0, 0, 0, ~
$ bussiness_stimules_none                            <dbl> 1, 0, 1, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, ~
$ bussiness_stimules_other                           <chr> "не надавались", NA, "Не створювались",~
$ humanitarian_hub                                   <chr> NA, NA, NA, NA, NA, NA, NA, NA, NA, NA,~
$ hromada_cooperation                                <chr> NA, NA, NA, NA, NA, NA, NA, NA, NA, NA,~
$ `hromada_cooperation/medicine`                     <dbl> NA, NA, NA, NA, NA, NA, NA, NA, NA, NA,~
$ `hromada_cooperation/food`                         <dbl> NA, NA, NA, NA, NA, NA, NA, NA, NA, NA,~
$ `hromada_cooperation/pensions`                     <dbl> NA, NA, NA, NA, NA, NA, NA, NA, NA, NA,~
$ `hromada_cooperation/evacuation`                   <dbl> NA, NA, NA, NA, NA, NA, NA, NA, NA, NA,~
$ `hromada_cooperation/other`                        <dbl> NA, NA, NA, NA, NA, NA, NA, NA, NA, NA,~
$ `hromada_cooperation/none`                         <dbl> NA, NA, NA, NA, NA, NA, NA, NA, NA, NA,~
$ hromada_cooperation_text                           <chr> NA, NA, NA, NA, NA, NA, NA, NA, NA, NA,~
$ is_damaged                                         <chr> "no", "no", "yes", "no", "no", "no", "y~
$ percent_damaged                                    <chr> NA, NA, "0_10_percent", NA, NA, NA, "0_~
$ damage_evaluation_persons                          <chr> NA, NA, "yes", NA, NA, NA, "yes", NA, N~
$ damage_evaluation_communal                         <chr> NA, NA, "no", NA, NA, NA, "yes", NA, NA~
$ damage_evaluation_bussiness                        <chr> NA, NA, "no", NA, NA, NA, "yes", NA, NA~
$ reconstruction_plan                                <chr> NA, NA, "yes", NA, NA, NA, "no", NA, NA~
$ reconstruction_financing                           <chr> NA, NA, "no", NA, NA, NA, "no", NA, NA,~
$ reconstruction_financing_text                      <chr> NA, NA, NA, NA, NA, NA, NA, NA, NA, NA,~
$ international_projects                             <chr> "0", "1", "0", "1", "0", "0", "3", "3",~
$ percent_reconstructed                              <chr> NA, NA, "76_100_percent", NA, NA, NA, "~
$ finance_school_shelters                            <chr> "50 тис. грн.", "0", "2200000", "120000~
$ finance_school_shelters_coded                      <dbl> 50000, 0, 2200000, 120000, 10000, 75000~
$ info_campaign                                      <dbl> 1, 1, 1, 1, 1, 0, 1, 1, 1, 1, 1, 1, 1, ~
$ reserves                                           <dbl> 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, NA,~
$ count_power_sources                                <dbl> 1, 1, 0, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, ~
$ count_heaters_need                                 <dbl> 0, 1, 0, 1, 0, 0, 1, 1, 1, 0, 0, 0, 0, ~
$ solid_fuel_boiler                                  <dbl> NA, 1, 0, 0, 0, 0, 1, 1, NA, 1, 0, 0, 0~
$ no_school_days                                     <chr> "0", "В громаді не було прогалин з навч~
$ no_school_days_coded                               <chr> "0", "0", "0", "75", "0", "0", NA, "0",~
$ hromada_exp                                        <chr> "yes", "yes", "yes", "yes", "yes", "yes~
$ hromada_problem_info                               <chr> "idp", "idp citizens bussiness", "bussi~
$ `hromada_problem_info/idp`                         <dbl> 1, 1, 0, 0, 0, 0, 1, 0, NA, 1, 1, 0, 0,~
$ `hromada_problem_info/citizens`                    <dbl> 0, 1, 0, 0, 1, 1, 1, 1, NA, 1, 0, 1, 1,~
$ `hromada_problem_info/bussiness`                   <dbl> 0, 1, 1, 1, 1, 0, 1, 1, NA, 0, 1, 1, 1,~
$ `hromada_problem_info/experts`                     <dbl> 0, 0, 0, 0, 0, 0, 0, 1, NA, 0, 0, 0, 0,~
$ `hromada_problem_info/ngo`                         <dbl> 0, 0, 0, 0, 1, 0, 1, 1, NA, 0, 0, 1, 1,~
$ `hromada_problem_info/nobody`                      <dbl> 0, 0, 0, 0, 0, 0, 0, 0, NA, 0, 0, 0, 0,~
$ hromada_problem_consultation                       <chr> "nobody", "idp citizens bussiness", "ci~
$ `hromada_problem_consultation/idp`                 <dbl> 0, 1, 0, 1, 0, 1, 1, 0, NA, 1, 1, 0, 0,~
$ `hromada_problem_consultation/citizens`            <dbl> 0, 1, 1, 1, 0, 0, 1, 0, NA, 1, 0, 0, 0,~
$ `hromada_problem_consultation/bussiness`           <dbl> 0, 1, 1, 0, 0, 0, 1, 1, NA, 0, 0, 1, 0,~
$ `hromada_problem_consultation/experts`             <dbl> 0, 0, 0, 0, 0, 0, 0, 0, NA, 0, 0, 0, 0,~
$ `hromada_problem_consultation/ngo`                 <dbl> 0, 0, 0, 0, 0, 0, 1, 0, NA, 0, 0, 1, 1,~
$ `hromada_problem_consultation/nobody`              <dbl> 1, 0, 0, 0, 1, 0, 0, 0, NA, 0, 0, 0, 0,~
$ hromada_problem_proposition                        <chr> "bussiness", "bussiness", "citizens bus~
$ `hromada_problem_proposition/idp`                  <dbl> 0, 0, 0, 1, 0, 0, 1, 0, NA, 1, 0, 0, 0,~
$ `hromada_problem_proposition/citizens`             <dbl> 0, 0, 1, 1, 0, 1, 1, 0, NA, 0, 1, 0, 0,~
$ `hromada_problem_proposition/bussiness`            <dbl> 1, 1, 1, 1, 0, 0, 1, 1, NA, 0, 0, 1, 1,~
$ `hromada_problem_proposition/experts`              <dbl> 0, 0, 0, 0, 0, 0, 1, 0, NA, 0, 0, 0, 0,~
$ `hromada_problem_proposition/ngo`                  <dbl> 0, 0, 0, 0, 0, 0, 1, 0, NA, 0, 0, 1, 1,~
$ `hromada_problem_proposition/nobody`               <dbl> 0, 0, 0, 0, 1, 0, 0, 0, NA, 0, 0, 0, 0,~
$ hromada_problem_system                             <chr> "bussiness", "bussiness", "bussiness ex~
$ `hromada_problem_system/idp`                       <dbl> 0, 0, 0, 0, 0, 0, 1, 0, NA, 1, 1, 0, 0,~
$ `hromada_problem_system/citizens`                  <dbl> 0, 0, 0, 1, 1, 1, 1, 0, NA, 1, 0, 0, 0,~
$ `hromada_problem_system/bussiness`                 <dbl> 1, 1, 1, 1, 1, 0, 1, 1, NA, 0, 0, 1, 1,~
$ `hromada_problem_system/experts`                   <dbl> 0, 0, 1, 0, 0, 0, 0, 0, NA, 1, 0, 0, 0,~
$ `hromada_problem_system/ngo`                       <dbl> 0, 0, 0, 0, 1, 0, 0, 0, NA, 1, 0, 1, 1,~
$ `hromada_problem_system/nobody`                    <dbl> 0, 0, 0, 0, 0, 0, 0, 0, NA, 0, 0, 0, 0,~
$ hromada_problem_feedback                           <chr> "nobody", "bussiness", "citizens bussin~
$ `hromada_problem_feedback/idp`                     <dbl> 0, 0, 0, 0, 0, 0, 1, 0, NA, 1, 1, 0, 0,~
$ `hromada_problem_feedback/citizens`                <dbl> 0, 0, 1, 1, 1, 1, 1, 0, NA, 0, 0, 1, 0,~
$ `hromada_problem_feedback/bussiness`               <dbl> 0, 1, 1, 1, 1, 0, 1, 1, NA, 0, 0, 1, 1,~
$ `hromada_problem_feedback/experts`                 <dbl> 0, 0, 0, 0, 0, 0, 0, 0, NA, 0, 0, 0, 0,~
$ `hromada_problem_feedback/ngo`                     <dbl> 0, 0, 0, 0, 0, 0, 0, 0, NA, 1, 0, 1, 1,~
$ `hromada_problem_feedback/nobody`                  <dbl> 1, 0, 0, 0, 0, 0, 0, 0, NA, 0, 0, 0, 0,~
$ hromada_problem_execution                          <chr> "ngo", "bussiness", "bussiness", "citiz~
$ `hromada_problem_execution/idp`                    <dbl> 0, 0, 0, 0, 0, 0, 1, 0, NA, 0, 1, 0, 0,~
$ `hromada_problem_execution/citizens`               <dbl> 0, 0, 0, 1, 1, 1, 1, 0, NA, 0, 1, 0, 0,~
$ `hromada_problem_execution/bussiness`              <dbl> 0, 1, 1, 1, 1, 0, 0, 0, NA, 0, 0, 1, 1,~
$ `hromada_problem_execution/experts`                <dbl> 0, 0, 0, 0, 0, 0, 0, 1, NA, 0, 0, 0, 0,~
$ `hromada_problem_execution/ngo`                    <dbl> 1, 0, 0, 0, 1, 0, 1, 0, NA, 1, 0, 1, 1,~
$ `hromada_problem_execution/nobody`                 <dbl> 0, 0, 0, 0, 0, 0, 0, 0, NA, 0, 0, 0, 0,~
$ skills_needed                                      <chr> "human_resourse", "fundraising", "fundr~
$ `skills_needed/fundraising`                        <dbl> 0, 1, 1, 1, 0, 1, 0, 1, 1, 1, 1, 1, 1, ~
$ `skills_needed/project_management`                 <dbl> 0, 0, 0, 1, 0, 1, 0, 0, 0, 1, 1, 1, 1, ~
$ `skills_needed/longterm_planning`                  <dbl> 0, 0, 0, 1, 0, 1, 0, 0, 0, 1, 0, 0, 0, ~
$ `skills_needed/crisis_planning`                    <dbl> 0, 0, 0, 1, 0, 1, 0, 0, 0, 1, 0, 0, 0, ~
$ `skills_needed/data_analysis`                      <dbl> 0, 0, 1, 1, 0, 1, 0, 0, 0, 0, 0, 0, 0, ~
$ `skills_needed/human_resourse`                     <dbl> 1, 0, 1, 0, 1, 0, 1, 1, 1, 0, 0, 1, 1, ~
$ `skills_needed/other`                              <dbl> 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, ~
$ skills_needed_text                                 <chr> NA, NA, NA, NA, NA, NA, NA, NA, NA, NA,~
$ contact_text                                       <chr> "Ружицька Валентина 0966195704", "Синьк~
$ prep_count                                         <dbl> 12, 9, 18, 13, 14, 17, 19, 12, 17, 7, 2~
$ comm_channels_count                                <dbl> 7, 6, 6, 3, 3, 5, 8, 5, 3, 6, 6, 5, 5, ~
$ help_military_count                                <dbl> 4, 4, 3, 3, 4, 3, 4, 1, 4, 2, 5, 3, 2, ~
$ hromada_cooperation_count                          <dbl> 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, ~
$ idp_registration_time                              <chr> "187", "133", "5", "19", "154", "17", "~
$ prep_winter_count                                  <dbl> 3, 5, 2, 4, 3, 2, 5, 5, 4, 4, 3, 3, 2, ~
$ oblast_center                                      <dbl> 0, 0, 0, 0, 0, 0, 1, 0, 0, 0, 0, 0, 0, ~
$ hromada_center_code                                <chr> "UA05100110010049992", "UA0510017001003~
$ hromada_center                                     <chr> "Студена", "Шпиків", "Калинівка", "Стан~
$ lat_center                                         <dbl> 48.14573, 48.78607, 49.45837, 48.97121,~
$ lon_center                                         <dbl> 28.85272, 28.56475, 28.52626, 28.10989,~
$ travel_time                                        <dbl> 163.6, 58.1, 34.1, 59.1, 47.9, 113.0, 0~
$ n_settlements                                      <dbl> 15, 27, 49, 16, 39, 11, 36, 8, 11, 10, ~
$ square                                             <dbl> 259.4, 478.9, 844.1, 301.0, 614.8, 167.~
$ occipied_before_2022                               <dbl> 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, ~
$ total_population_2022                              <dbl> 5778, 15803, 44542, 7436, 20302, 3364, ~
$ urban_population_2022                              <dbl> 0, 2917, 18492, 0, 7958, 0, 217449, 543~
$ urban_pct                                          <dbl> 0.00000000, 0.18458521, 0.41515873, 0.0~
$ budget_code                                        <chr> "02502000000", "02513000000", "02501000~
$ budget_name                                        <chr> "Бюджет Студенянської сільської територ~
$ oblast_name_en                                     <chr> "Vinnytsia", "Vinnytsia", "Vinnytsia", ~
$ region_en.x                                        <chr> "Center", "Center", "Center", "Center",~
$ region_code_en                                     <chr> "C", "C", "C", "C", "C", "W", "W", "W",~
$ income_total_2021                                  <dbl> 20730049, 48691797, 178804836, 26343066~
$ income_transfert_2021                              <dbl> 10462622, 29977942, 62006814, 14318223,~
$ income_military_2021                               <dbl> 1045251.0, 0.0, 11114908.8, 0.0, 782324~
$ income_pdfo_2021                                   <dbl> 5462135, 10093919, 74352398, 7178588, 2~
$ income_unified_tax_2021                            <dbl> 1458530.3, 2621993.3, 12220444.9, 98515~
$ income_property_tax_2021                           <dbl> 3029384.2, 4151579.8, 18346558.8, 24329~
$ income_excise_duty_2021                            <dbl> 9618.00, 841932.38, 6514591.36, 29836.2~
$ income_own_2021                                    <dbl> 10267427, 18713856, 116798022, 12024843~
$ own_income_prop_2021                               <dbl> 0.50, 0.38, 0.65, 0.46, 0.58, 0.30, 0.7~
$ transfert_prop_2021                                <dbl> 0.50, 0.62, 0.35, 0.54, 0.42, 0.70, 0.2~
$ military_tax_prop_2021                             <dbl> 0.05, 0.00, 0.06, 0.00, 0.01, 0.00, 0.0~
$ pdfo_prop_2021                                     <dbl> 0.26, 0.21, 0.42, 0.27, 0.34, 0.10, 0.5~
$ unified_tax_prop_2021                              <dbl> 0.07, 0.05, 0.07, 0.04, 0.07, 0.03, 0.0~
$ property_tax_prop_2021                             <dbl> 0.15, 0.09, 0.10, 0.09, 0.12, 0.13, 0.0~
$ excise_duty_prop_2021                              <dbl> 0.00, 0.02, 0.04, 0.00, 0.03, 0.00, 0.0~
$ own_income_change                                  <dbl> 0.71, -0.03, 0.53, -0.31, 0.04, 0.06, 0~
$ own_prop_change                                    <dbl> 0.19, 0.00, 0.07, -0.08, 0.02, 0.02, 0.~
$ total_income_change                                <dbl> 0.22, -0.02, 0.38, -0.16, 0.01, 0.03, 0~
$ income_own                                         <dbl> 17528225, 18205722, 178548531, 8349266,~
$ income_total                                       <dbl> 25355340, 47898128, 247407289, 22130386~
$ income_transfert                                   <dbl> 7827115, 29692406, 68858758, 13781120, ~
$ dfrr_executed                                      <dbl> NA, 8330.759, 10963.622, NA, 4646.000, ~
$ turnout_2020                                       <dbl> 0.5034738, 0.4260486, 0.4086520, 0.4721~
$ sex_head                                           <chr> "female", "female", "female", "female",~
$ age_head                                           <dbl> 61, 47, 64, 48, 61, 55, 34, 39, 48, 35,~
$ education_head                                     <chr> "higher", "non-higher", "higher", "high~
$ incumbent                                          <dbl> 1, 0, 1, 1, 0, 1, 1, 0, 0, 1, 0, 0, 1, ~
$ rda                                                <dbl> 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 1, 1, 0, ~
$ not_from_here                                      <dbl> 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, ~
$ party                                              <chr> "Самовисування", "Батьківщина", "Україн~
$ enterpreuner                                       <dbl> 0, 0, 0, 0, 0, 0, 0, 1, 0, 0, 0, 0, 0, ~
$ unemployed                                         <dbl> 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, ~
$ priv_work                                          <dbl> 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, ~
$ polit_work                                         <dbl> 1, 1, 1, 1, 1, 1, 1, 0, 1, 1, 1, 1, 1, ~
$ communal_work                                      <dbl> 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, ~
$ ngo_work                                           <dbl> 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, ~
$ party_national_winner                              <dbl> 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 1, 1, 0, ~
$ no_party                                           <dbl> 1, 0, 0, 1, 0, 0, 0, 0, 1, 0, 0, 0, 1, ~
$ male                                               <dbl> 0, 0, 0, 0, 0, 0, 0, 0, 1, 1, 0, 0, 0, ~
$ high_educ                                          <dbl> 1, 0, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, ~
$ sum_osbb_2020                                      <dbl> NA, NA, 12, NA, 6, NA, 471, 200, NA, NA~
$ edem_total                                         <dbl> 0, 0, 1, 0, 1, 0, 4, 2, 0, 2, 1, 2, 0, ~
$ edem_petitions                                     <dbl> 0, 0, 1, 0, 1, 0, 1, 1, 0, 1, 0, 0, 0, ~
$ edem_consultations                                 <dbl> 0, 0, 0, 0, 0, 0, 1, 0, 0, 1, 1, 1, 0, ~
$ edem_participatory_budget                          <dbl> 0, 0, 0, 0, 0, 0, 1, 1, 0, 0, 0, 0, 0, ~
$ edem_open_hromada                                  <dbl> 0, 0, 0, 0, 0, 0, 1, 0, 0, 0, 0, 1, 0, ~
$ youth_councils                                     <dbl> 0, 0, 0, 0, 0, 0, 2, 0, 0, 1, 0, 0, 0, ~
$ youth_centers                                      <dbl> 0, 0, 0, 0, 0, 0, 4, 1, 0, 1, 0, 0, 0, ~
$ business_support_centers                           <dbl> 1, 0, 0, 0, 0, 0, 13, 2, 0, 0, 0, 0, 0,~
$ creation_date                                      <dttm> 2015-08-16, 2016-10-02, 2015-08-16, 20~
$ creation_year                                      <dbl> 2015, 2016, 2015, 2020, 2020, 2017, 201~
$ time_before_24th                                   <dbl> 2383.7917, 1970.7917, 2383.7917, 556.79~
$ voluntary                                          <dbl> 1, 1, 1, 0, 0, 1, 1, 0, 1, 1, 0, 1, 1, ~
```

```{.r .fold-show}
meta_survey %>% filter(group == 'preamble')
```

```
# A tibble: 1 x 14
  group    item_~1 label~2 label~3 type  name  label requi~4 appea~5 media~6 relev~7 const~8 const~9
  <chr>      <dbl> <chr>   <chr>   <chr> <chr> <chr> <list>  <chr>   <chr>   <chr>   <chr>   <chr>  
1 preamble      NA <NA>    <NA>    note  <NA>  "Суп~ <NULL>  <NA>    kse_in~ <NA>    <NA>    <NA>   
# ... with 1 more variable: hint <chr>, and abbreviated variable names 1: item_number, 2: label_en,
#   3: label_ua, 4: required, 5: appearance, 6: `media::image`, 7: relevant, 8: constraint,
#   9: constraint_message
```

```{.r .fold-show}
ds_survey %>% pull(hromada_code) %>% unique()
```

```
  [1] "UA05100110000070795" "UA05100170000071290" "UA05120070000075759" "UA05060130000030729"
  [5] "UA05020110000052014" "UA07060370000022360" "UA07080170000013585" "UA07020130000036300"
  [9] "UA07060290000054842" "UA07060390000098670" "UA12060190000043514" "UA12040170000019083"
 [13] "UA12100030000084605" "UA12140150000054570" "UA12040210000039759" "UA12100050000045992"
 [17] "UA12040030000066040" "UA12080070000077032" "UA12140110000060935" "UA12140250000015858"
 [21] "UA12140270000072109" "UA12020130000022909" "UA18020050000058053" "UA18060170000069581"
 [25] "UA18040410000025491" "UA21120190000082169" "UA21020070000015036" "UA21080190000094580"
 [29] "UA21080150000014443" "UA21040250000010914" "UA21100130000055002" "UA21080070000025254"
 [33] "UA21060070000049340" "UA21020130000047547" "UA21040110000099623" "UA21120130000025618"
 [37] "UA23060230000071243" "UA23060130000058533" "UA23060150000085288" "UA26060250000064599"
 [41] "UA26060090000054411" "UA26040230000035526" "UA26040130000010870" "UA26060170000091466"
 [45] "UA26120050000087602" "UA26040110000023512" "UA26100050000019570" "UA26060110000025739"
 [49] "UA26040290000025886" "UA26040270000047749" "UA26060030000011364" "UA26060130000047466"
 [53] "UA26120070000067596" "UA26040350000024417" "UA26080070000092582" "UA32120090000034281"
 [57] "UA32140070000012102" "UA32080210000074136" "UA32020230000012716" "UA32060030000048241"
 [61] "UA35060190000079777" "UA35020130000045875" "UA35020090000039429" "UA35040030000074104"
 [65] "UA35040190000012514" "UA46060370000065608" "UA46060230000093092" "UA46080130000077112"
 [69] "UA46100070000076013" "UA46080090000029798" "UA46020010000073886" "UA46140010000081849"
 [73] "UA46100150000087495" "UA46140030000023506" "UA48040150000011861" "UA51040110000040346"
 [77] "UA51100250000055079" "UA51020170000041393" "UA51120070000097161" "UA51120210000054996"
 [81] "UA51120230000084853" "UA51020110000041005" "UA51060030000044366" "UA51100070000063635"
 [85] "UA51140110000053825" "UA51020030000095942" "UA51120050000071748" "UA53060250000043118"
 [89] "UA53060070000077527" "UA53060230000098362" "UA53040130000097690" "UA53040110000034949"
 [93] "UA53040030000088898" "UA53060290000047345" "UA53040010000091190" "UA56080150000069525"
 [97] "UA56060050000010769" "UA56060310000066585" "UA56060290000044465" "UA59080230000084731"
[101] "UA59100010000064812" "UA59020050000012539" "UA59020110000066430" "UA59080030000075526"
[105] "UA59040010000075530" "UA59060110000049734" "UA59040130000041676" "UA61060070000098188"
[109] "UA61060130000045755" "UA61020030000060484" "UA61060270000084790" "UA63020090000096294"
[113] "UA63100010000016136" "UA63120210000075842" "UA63100030000050119" "UA65060250000073379"
[117] "UA65040010000061104" "UA65100150000057191" "UA65020190000013272" "UA65100130000084882"
[121] "UA65080070000011930" "UA65020150000047137" "UA68040430000052699" "UA71080450000083423"
[125] "UA71040130000029175" "UA71040090000047664" "UA71020030000059581" "UA71080070000050993"
[129] "UA73060250000049790" "UA73060410000077092" "UA73040170000011490" "UA74040130000094076"
[133] "UA74040050000013413" "UA74040330000024949" "UA74080150000033167" "UA74100090000064336"
[137] "UA74080130000060606" "UA74080010000063011"
```

</details>

Next, we define useful sets of variable names to be used throughout the report

<details>

<summary>click to see the groups </summary>


```
 [1] "help_for_military"             "evacuation_actions"            "idp_help"                     
 [4] "special_fund_relocation_needs" "bussiness_stimules"            "hromada_cooperation"          
 [7] "hromada_problem_inv_labels"    "hromada_problem_info"          "hromada_problem_consultation" 
[10] "hromada_problem_proposition"   "hromada_problem_system"        "hromada_problem_feedback"     
[13] "hromada_problem_execution"     "skills_needed"                
```

```
 [1] "prep_first_aid_water"            "prep_first_aid_fuel"            
 [3] "prep_reaction_plan"              "prep_evacuation_plan"           
 [5] "prep_reaction_plan_oth_hromadas" "prep_reaction_plan_oda"         
 [7] "prep_dftg_creation"              "prep_national_resistance"       
 [9] "prep_starosta_meeting"           "prep_communal_meetiing"         
[11] "prep_online_map"                 "prep_shelter_list"              
[13] "prep_notification_check"         "prep_backup"                    
[15] "prep_partly_backup"             
```

```
[1] "telegram"  "viber"     "facebook"  "chat_help" "hotline"  
```

```
[1] "idp_help/communal_placement" "idp_help/private_placement"  "idp_help/regular_meal"      
[4] "idp_help/humanitar_help"     "idp_help/fundraising"        "idp_help/employ"            
[7] "idp_help/psych_help"         "idp_help/law_help"           "idp_help/transit_center"    
```

```
[1] "help_for_military/rooms"     "help_for_military/transport" "help_for_military/money"    
[4] "help_for_military/products"  "help_for_military/other"     "help_for_military/none"     
```

```
[1] "hromada_cooperation/medicine"   "hromada_cooperation/food"      
[3] "hromada_cooperation/pensions"   "hromada_cooperation/evacuation"
[5] "hromada_cooperation/other"      "hromada_cooperation/none"      
```

```
[1] "skills_needed/fundraising"        "skills_needed/project_management"
[3] "skills_needed/longterm_planning"  "skills_needed/crisis_planning"   
[5] "skills_needed/data_analysis"      "skills_needed/human_resourse"    
[7] "skills_needed/other"             
```

```
[1] "own_income_prop_2021"   "transfert_prop_2021"    "military_tax_prop_2021"
[4] "pdfo_prop_2021"         "unified_tax_prop_2021"  "property_tax_prop_2021"
[7] "excise_duty_prop_2021" 
```

</details>

<details>

<summary>meta data </summary>


```{.r .fold-show}
meta_survey %>% glimpse()
```

```
Rows: 132
Columns: 14
$ group              <chr> NA, NA, NA, NA, NA, NA, "preamble", "general_information", "general_inf~
$ item_number        <dbl> NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA,~
$ label_en           <chr> NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA,~
$ label_ua           <chr> NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA,~
$ type               <chr> "start", "end", "today", "deviceid", "begin_group", "begin_group", "not~
$ name               <chr> "start", "end", "today", "deviceid", "general_information", "general_in~
$ label              <chr> NA, NA, NA, NA, NA, NA, "Супротив України у загарбницькій війні Російсь~
$ required           <list> <NULL>, <NULL>, <NULL>, <NULL>, <NULL>, "field-list", <NULL>, TRUE, TR~
$ appearance         <chr> NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA,~
$ `media::image`     <chr> NA, NA, NA, NA, NA, NA, "kse_institute.png", NA, NA, NA, NA, NA, NA, NA~
$ relevant           <chr> NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA,~
$ constraint         <chr> NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA,~
$ constraint_message <chr> NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA,~
$ hint               <chr> NA, NA, NA, NA, NA, NA, "Заповнення опитувальника займе орієнтовно 25 х~
```

```{.r .fold-show}
meta_survey %>% 
  filter(type %in% c("begin_group","end_group")) %>% 
  select(1:5) %>% 
  print_all()
```

```
# A tibble: 32 x 5
   group item_number label_en label_ua type       
   <chr>       <dbl> <chr>    <chr>    <chr>      
 1 <NA>           NA <NA>     <NA>     begin_group
 2 <NA>           NA <NA>     <NA>     begin_group
 3 <NA>           NA <NA>     <NA>     end_group  
 4 <NA>           NA <NA>     <NA>     end_group  
 5 <NA>           NA <NA>     <NA>     begin_group
 6 <NA>           NA <NA>     <NA>     end_group  
 7 <NA>           NA <NA>     <NA>     begin_group
 8 <NA>           NA <NA>     <NA>     end_group  
 9 <NA>           NA <NA>     <NA>     begin_group
10 <NA>           NA <NA>     <NA>     begin_group
11 <NA>           NA <NA>     <NA>     end_group  
12 <NA>           NA <NA>     <NA>     end_group  
13 <NA>           NA <NA>     <NA>     begin_group
14 <NA>           NA <NA>     <NA>     end_group  
15 <NA>           NA <NA>     <NA>     begin_group
16 <NA>           NA <NA>     <NA>     end_group  
17 <NA>           NA <NA>     <NA>     begin_group
18 <NA>           NA <NA>     <NA>     end_group  
19 <NA>           NA <NA>     <NA>     begin_group
20 <NA>           NA <NA>     <NA>     end_group  
21 <NA>           NA <NA>     <NA>     begin_group
22 <NA>           NA <NA>     <NA>     end_group  
23 <NA>           NA <NA>     <NA>     begin_group
24 <NA>           NA <NA>     <NA>     begin_group
25 <NA>           NA <NA>     <NA>     end_group  
26 <NA>           NA <NA>     <NA>     end_group  
27 <NA>           NA <NA>     <NA>     begin_group
28 <NA>           NA <NA>     <NA>     begin_group
29 <NA>           NA <NA>     <NA>     end_group  
30 <NA>           NA <NA>     <NA>     begin_group
31 <NA>           NA <NA>     <NA>     end_group  
32 <NA>           NA <NA>     <NA>     end_group  
```

```{.r .fold-show}
meta_survey %>% glimpse()
```

```
Rows: 132
Columns: 14
$ group              <chr> NA, NA, NA, NA, NA, NA, "preamble", "general_information", "general_inf~
$ item_number        <dbl> NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA,~
$ label_en           <chr> NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA,~
$ label_ua           <chr> NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA,~
$ type               <chr> "start", "end", "today", "deviceid", "begin_group", "begin_group", "not~
$ name               <chr> "start", "end", "today", "deviceid", "general_information", "general_in~
$ label              <chr> NA, NA, NA, NA, NA, NA, "Супротив України у загарбницькій війні Російсь~
$ required           <list> <NULL>, <NULL>, <NULL>, <NULL>, <NULL>, "field-list", <NULL>, TRUE, TR~
$ appearance         <chr> NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA,~
$ `media::image`     <chr> NA, NA, NA, NA, NA, NA, "kse_institute.png", NA, NA, NA, NA, NA, NA, NA~
$ relevant           <chr> NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA,~
$ constraint         <chr> NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA,~
$ constraint_message <chr> NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA,~
$ hint               <chr> NA, NA, NA, NA, NA, NA, "Заповнення опитувальника займе орієнтовно 25 х~
```

</details>

## Transformations

For the state `ds0`, we augment the focal table of the report with additional columns and transform existing variable to better fit visualization/modeling needs


```{.r .fold-hide}
ds_general0 <- 
  ds_general %>% 
  mutate(
    survey_response = case_when(
      hromada_code %in% (ds_survey %>% pull(hromada_code) %>% unique()) ~ TRUE
      ,TRUE ~ FALSE
    )
  )
# ds_general0 %>% group_by(survey_response) %>% count()

ds_general1 <- ds_general %>% 
  mutate(oblast_name_en = case_when(oblast_name_en == 'Vonyn' ~ "Volyn",
                                    oblast_name_en == 'Driproptrovska' ~ "Dnipropetrovska",
                                    TRUE ~ oblast_name_en)
  )

ds0 <- 
  ds_survey %>% 
  mutate(
    income_own_per_capita       = income_own_2021         / total_population_2022,
    income_total_per_capita     = income_total_2021       / total_population_2022,
    income_tranfert_per_capita  = income_transfert_2021   / total_population_2022,
    idp_registration_share      = idp_registration_number / total_population_2022,
    idp_real_share              = idp_real_number         / total_population_2022 * 100,
    idp_child_share             = idp_child_education     / idp_registration_number * 100,
    type                        = case_when(type == 'сільська' ~ 'village',
                                            type == 'селищна' ~ 'urban village',
                                            type == 'міська' ~ 'urban'),
    type = factor(type, levels  = c("village", "urban village", "urban")),
    help_military_count         = rowSums(across(all_of(military_help_short))),
    idp_help_count              = rowSums(across(all_of(idp_help))),
    occupation_and_combat       = case_when(military_action == 'no_combat' & occupation == 'not_occupied' ~ 0,
                                            TRUE ~ 1),
    occupation_and_combat_fct   = factor(occupation_and_combat, 
                                         labels = c('Rear communities', 
                                                    'Communities exposed to war (n = 22)')),
    occupation_fct              = factor(deoccupied_at_feb_2023,
                                         labels = c('Rear communities', 
                                                    'Deoccupied communities (n = 16)')),
    voluntary_fct               = factor(voluntary,
                                         labels = c('Top-down amalgamated', 'Voluntary amalgamated')),
    oblast_name_en              = case_when(oblast_name_en == 'Vonyn' ~ "Volyn",
                                            oblast_name_en == 'Driproptrovska' ~ "Dnipropetrovska",
                                            TRUE ~ oblast_name_en)
     )

ds1_winter_prep <- ds0 %>% 
  mutate(
    winter_prep_count = rowSums(across(info_campaign:solid_fuel_boiler), na.rm = T)
    ,winter_prep_count = case_when(
      occupation =='not_occupied' | occupation == 'occupied_april' ~ winter_prep_count
      ,TRUE ~ NA_real_
    )
  )

ds1_problem <- ds0 %>% 
  mutate(
    hromada_exp = ifelse(hromada_exp == "yes", 1, 0)
    ,problem_info_index         =           ifelse(`hromada_problem_info/nobody`==1, 0, 
                                           rowSums(across(contains("hromada_problem_info/"))))
    ,problem_consultation_index =   ifelse(`hromada_problem_consultation/nobody`==1, 0, 
                                           rowSums(across(contains("hromada_problem_consultation/"))))
    ,problem_proposition_index  =   ifelse(`hromada_problem_proposition/nobody`==1, 0,
                                           rowSums(across(contains("hromada_problem_proposition/"))))
    ,problem_system_index       =   ifelse(`hromada_problem_system/nobody`==1, 0,
                                           rowSums(across(contains("hromada_problem_system/"))))
    ,problem_feedback_index     =   ifelse(`hromada_problem_feedback/nobody`==1, 0,
                                           rowSums(across(contains("hromada_problem_feedback/"))))
    ,problem_execution_index    =   ifelse(`hromada_problem_execution/nobody`==1, 0,
                                           rowSums(across(contains("hromada_problem_execution/"))))
    ,problem_additive_index     =   .4*problem_info_index + .6*problem_consultation_index + 
      .6*problem_proposition_index + .8*problem_system_index + .8*problem_feedback_index +
      problem_execution_index
  )
```

To make our analysis more nimble we create four alternative versions of `ds1` with Invasion Preparedness questions

<details>

<summary>show transformations </summary>


```{.r .fold-show}
# compute total binary score (preparations are made at all, regardless of timing)
d_meta_prep <- 
  meta_survey %>% 
  filter(group=="preparation") %>% 
  select(item_name = name,label_en,label)

ds1_prep <-
  ds0 %>% 
  mutate(
    # sum of 0|1|2 where larger numbers indicate more preparedness
    prep_score_combo = rowSums(across(all_of(preparation)),na.rm = T) 
    ,prep_score_feb = rowSums(
      across(
        .cols = preparation
        ,.fns = ~case_when(
          .  == 0 ~ 0 #"No"
          ,. == 1 ~ 0 #"After Feb 24"
          ,. == 2 ~ 1 #"Before Feb 24"
        )
      )
      ,na.rm = T
    )
    ,prep_score_oct = rowSums(
      across(
        .cols = preparation
        ,.fns = ~case_when(
          .  == 0 ~ 0 #"No"
          ,. == 1 ~ 1 #"After Feb 24"
          ,. == 2 ~ 1 #"Before Feb 24"
        )
      )
      ,na.rm = T
    )
  )  %>% 
  select(hromada_code, starts_with("prep_score"),preparation)  
ds1_prep %>% select(1:4)
```

```
# A tibble: 138 x 4
   hromada_code        prep_score_combo prep_score_feb prep_score_oct
   <chr>                          <dbl>          <dbl>          <dbl>
 1 UA05100110000070795               12              0             12
 2 UA05100170000071290                9              0              9
 3 UA05120070000075759               18              5             13
 4 UA05060130000030729               13              2             11
 5 UA05020110000052014               14              3             11
 6 UA07060370000022360               17              3             14
 7 UA07080170000013585               19              4             15
 8 UA07020130000036300               12              1             11
 9 UA07060290000054842               17              2             15
10 UA07060390000098670                7              0              7
# ... with 128 more rows
```

```{.r .fold-show}
## Some handy datasets for quick visualization
# Raw scale (0,1,2) with factors
ds1_prep_ordinal_factors <- 
  ds1_prep %>% 
  mutate(
    across(
      .cols = preparation
      ,.fns = ~case_when(
        . == 0  ~ "No"
        ,. == 1 ~ "As of Oct"
        ,. == 2 ~ "As of Feb"
        ,TRUE   ~ "Not Applicable"
      ) %>% factor(levels=c("No","As of Oct","As of Feb",  "Not Applicable"))
    )
  ) %>% 
  select(hromada_code, starts_with("prep_score"),preparation)

# Binary scale (0,1) with factors
ds1_prep_binary_factors <- 
  ds1_prep %>% 
  mutate(
    across(
      .cols = preparation
      ,.fns = ~case_when(
        .  == 0  ~ "No"
        ,. == 1 ~ "Yes"
        ,. == 2 ~ "Yes"
        ,TRUE   ~ "Not Applicable"
      ) %>% factor(levels=c("No","Yes","Not Applicable"))
    )
  ) %>% 
  select(hromada_code, starts_with("prep_score"),preparation)

# Binary scale (0,1) with factors
ds1_prep_binary_factors_feb <- 
  ds1_prep %>% 
  mutate(
    across(
      .cols = preparation
      ,.fns = ~case_when(
        .  == 0  ~ "No"
        ,. == 1 ~ "No"
        ,. == 2 ~ "Yes"
        ,TRUE   ~ "No"
      ) %>% factor(levels=c("No","Yes"))
    )
  ) %>% 
  select(hromada_code, starts_with("prep_score"),preparation)
```


```{.r .fold-show}
d_meta_info <- 
  meta_survey %>% 
  filter(group== "information") %>%
  select(item_name = name,label_en,item_number)
meta_choices %>% filter(list_name=="commun_prep")
```

```
# A tibble: 3 x 4
  list_name   name      label                         label_en
  <chr>       <chr>     <chr>                         <chr>   
1 commun_prep before_24 Було створено до 24 лютого    <NA>    
2 commun_prep after_24  Було створено після 24 лютого <NA>    
3 commun_prep none      Немає                         <NA>    
```

```{.r .fold-show}
item_information <- d_meta_info %>% pull(item_name)

ds1_info <- 
  ds0 %>% 
  mutate(
    across(
      .cols = item_information
      ,.fns = ~case_when(
        . == 0  ~ "No"
        ,. == 1 ~ "After Feb 24"
        ,. == 2 ~ "Before Feb 24"
        ,TRUE   ~ "Not Applicable"
      ) %>% factor(levels=c("No","Before Feb 24","After Feb 24",  "Not Applicable"))
    )
  ) %>% 
  select(hromada_code,item_information)
```


</details>

<details>

<summary>examine the versions </summary>



</details>

# Variable List

The following variables are present in the processed data table of survey responses:


```{.r .fold-hide}
ds0 %>% explore::describe_all() %>%neat_DT()
```

```{=html}
<div id="htmlwidget-153fbb359552a4a20a36" style="width:100%;height:auto;" class="datatables html-widget"></div>
<script type="application/json" data-for="htmlwidget-153fbb359552a4a20a36">{"x":{"filter":"top","vertical":false,"filterHTML":"<tr>\n  <td><\/td>\n  <td data-type=\"character\" style=\"vertical-align: top;\">\n    <div class=\"form-group has-feedback\" style=\"margin-bottom: auto;\">\n      <input type=\"search\" placeholder=\"All\" class=\"form-control\" style=\"width: 100%;\"/>\n      <span class=\"glyphicon glyphicon-remove-circle form-control-feedback\"><\/span>\n    <\/div>\n  <\/td>\n  <td data-type=\"character\" style=\"vertical-align: top;\">\n    <div class=\"form-group has-feedback\" style=\"margin-bottom: auto;\">\n      <input type=\"search\" placeholder=\"All\" class=\"form-control\" style=\"width: 100%;\"/>\n      <span class=\"glyphicon glyphicon-remove-circle form-control-feedback\"><\/span>\n    <\/div>\n  <\/td>\n  <td data-type=\"integer\" style=\"vertical-align: top;\">\n    <div class=\"form-group has-feedback\" style=\"margin-bottom: auto;\">\n      <input type=\"search\" placeholder=\"All\" class=\"form-control\" style=\"width: 100%;\"/>\n      <span class=\"glyphicon glyphicon-remove-circle form-control-feedback\"><\/span>\n    <\/div>\n    <div style=\"display: none;position: absolute;width: 200px;opacity: 1\">\n      <div data-min=\"0\" data-max=\"136\"><\/div>\n      <span style=\"float: left;\"><\/span>\n      <span style=\"float: right;\"><\/span>\n    <\/div>\n  <\/td>\n  <td data-type=\"number\" style=\"vertical-align: top;\">\n    <div class=\"form-group has-feedback\" style=\"margin-bottom: auto;\">\n      <input type=\"search\" placeholder=\"All\" class=\"form-control\" style=\"width: 100%;\"/>\n      <span class=\"glyphicon glyphicon-remove-circle form-control-feedback\"><\/span>\n    <\/div>\n    <div style=\"display: none;position: absolute;width: 200px;opacity: 1\">\n      <div data-min=\"0\" data-max=\"98.6\" data-scale=\"1\"><\/div>\n      <span style=\"float: left;\"><\/span>\n      <span style=\"float: right;\"><\/span>\n    <\/div>\n  <\/td>\n  <td data-type=\"integer\" style=\"vertical-align: top;\">\n    <div class=\"form-group has-feedback\" style=\"margin-bottom: auto;\">\n      <input type=\"search\" placeholder=\"All\" class=\"form-control\" style=\"width: 100%;\"/>\n      <span class=\"glyphicon glyphicon-remove-circle form-control-feedback\"><\/span>\n    <\/div>\n    <div style=\"display: none;position: absolute;width: 200px;opacity: 1\">\n      <div data-min=\"1\" data-max=\"138\"><\/div>\n      <span style=\"float: left;\"><\/span>\n      <span style=\"float: right;\"><\/span>\n    <\/div>\n  <\/td>\n  <td data-type=\"number\" style=\"vertical-align: top;\">\n    <div class=\"form-group has-feedback\" style=\"margin-bottom: auto;\">\n      <input type=\"search\" placeholder=\"All\" class=\"form-control\" style=\"width: 100%;\"/>\n      <span class=\"glyphicon glyphicon-remove-circle form-control-feedback\"><\/span>\n    <\/div>\n    <div style=\"display: none;position: absolute;width: 200px;opacity: 1\">\n      <div data-min=\"-0.83\" data-max=\"191541757\" data-scale=\"2\"><\/div>\n      <span style=\"float: left;\"><\/span>\n      <span style=\"float: right;\"><\/span>\n    <\/div>\n  <\/td>\n  <td data-type=\"number\" style=\"vertical-align: top;\">\n    <div class=\"form-group has-feedback\" style=\"margin-bottom: auto;\">\n      <input type=\"search\" placeholder=\"All\" class=\"form-control\" style=\"width: 100%;\"/>\n      <span class=\"glyphicon glyphicon-remove-circle form-control-feedback\"><\/span>\n    <\/div>\n    <div style=\"display: none;position: absolute;width: 200px;opacity: 1\">\n      <div data-min=\"-0.01\" data-max=\"197322877.2\" data-scale=\"2\"><\/div>\n      <span style=\"float: left;\"><\/span>\n      <span style=\"float: right;\"><\/span>\n    <\/div>\n  <\/td>\n  <td data-type=\"number\" style=\"vertical-align: top;\">\n    <div class=\"form-group has-feedback\" style=\"margin-bottom: auto;\">\n      <input type=\"search\" placeholder=\"All\" class=\"form-control\" style=\"width: 100%;\"/>\n      <span class=\"glyphicon glyphicon-remove-circle form-control-feedback\"><\/span>\n    <\/div>\n    <div style=\"display: none;position: absolute;width: 200px;opacity: 1\">\n      <div data-min=\"0\" data-max=\"1288755475.83\" data-scale=\"2\"><\/div>\n      <span style=\"float: left;\"><\/span>\n      <span style=\"float: right;\"><\/span>\n    <\/div>\n  <\/td>\n<\/tr>","data":[["1","2","3","4","5","6","7","8","9","10","11","12","13","14","15","16","17","18","19","20","21","22","23","24","25","26","27","28","29","30","31","32","33","34","35","36","37","38","39","40","41","42","43","44","45","46","47","48","49","50","51","52","53","54","55","56","57","58","59","60","61","62","63","64","65","66","67","68","69","70","71","72","73","74","75","76","77","78","79","80","81","82","83","84","85","86","87","88","89","90","91","92","93","94","95","96","97","98","99","100","101","102","103","104","105","106","107","108","109","110","111","112","113","114","115","116","117","118","119","120","121","122","123","124","125","126","127","128","129","130","131","132","133","134","135","136","137","138","139","140","141","142","143","144","145","146","147","148","149","150","151","152","153","154","155","156","157","158","159","160","161","162","163","164","165","166","167","168","169","170","171","172","173","174","175","176","177","178","179","180","181","182","183","184","185","186","187","188","189","190","191","192","193","194","195","196","197","198","199","200","201","202","203","204","205","206","207","208","209","210","211","212","213","214","215","216","217","218","219","220","221","222","223","224","225","226","227","228","229","230","231","232","233","234","235","236","237","238","239","240","241","242","243","244","245","246","247","248","249","250","251","252","253","254","255","256","257","258","259","260","261","262","263","264","265","266","267","268","269","270","271"],["index","today","_id","hromada_code","hromada_name","hromada_full_name","raion_code","raion_name","oblast_code","oblast_name","region_en","region_en.y","deoccupied_at_feb_2023","type","occupation","military_action","population_text","partners_text","friends_text","state_communication","prep_first_aid_water","prep_first_aid_fuel","prep_reaction_plan","prep_evacuation_plan","prep_reaction_plan_oth_hromadas","prep_reaction_plan_oda","prep_dftg_creation","prep_national_resistance","prep_starosta_meeting","prep_communal_meetiing","prep_online_map","prep_shelter_list","prep_notification_check","prep_backup","prep_partly_backup","shelter_capacity_before_text","shelter_capacity_before_coded","shelter_capacity_now_text","shelter_capacity_now_coded","telegram","viber","facebook","chat_help","hotline","telegram_link","facebook_link","head_hromada_communication","dftg_creation","dftg_creation_date","help_for_military","help_for_military/rooms","help_for_military/transport","help_for_military/money","help_for_military/products","help_for_military/other","help_for_military/none","help_for_military_text","transport_help_communal","transport_help_bought","transport_help_communal_coded","transport_help_bought_coded","percent_working_march","percent_working_now","commun_between_hromadas","evacuation","idp_accept","idp_registration_date","idp_registration_number","idp_real_number","idp_help","idp_help/communal_placement","idp_help/private_placement","idp_help/regular_meal","idp_help/humanitar_help","idp_help/fundraising","idp_help/employ","idp_help/psych_help","idp_help/law_help","idp_help/transit_center","idp_place_rooms","idp_room_number","idp_child_education","special_fund_relocation","special_fund_relocation_needs","special_fund_relocation_needs/state_functions","special_fund_relocation_needs/defense","special_fund_relocation_needs/public_order","special_fund_relocation_needs/economic_activity","special_fund_relocation_needs/environment","special_fund_relocation_needs/utilities","special_fund_relocation_needs/spirit_development","special_fund_relocation_needs/education","special_fund_relocation_needs/social_protection","special_fund_relocation_needs/healthcare","relocated_companies_text","created_jobs","bussiness_stimules","bussiness_stimules/tax_benefits","bussiness_stimules/free_rooms","bussiness_stimules/education","bussiness_stimules/other","bussiness_stimules_none","bussiness_stimules_other","humanitarian_hub","hromada_cooperation","hromada_cooperation/medicine","hromada_cooperation/food","hromada_cooperation/pensions","hromada_cooperation/evacuation","hromada_cooperation/other","hromada_cooperation/none","hromada_cooperation_text","is_damaged","percent_damaged","damage_evaluation_persons","damage_evaluation_communal","damage_evaluation_bussiness","reconstruction_plan","reconstruction_financing","reconstruction_financing_text","international_projects","percent_reconstructed","finance_school_shelters","finance_school_shelters_coded","info_campaign","reserves","count_power_sources","count_heaters_need","solid_fuel_boiler","no_school_days","no_school_days_coded","hromada_exp","hromada_problem_info","hromada_problem_info/idp","hromada_problem_info/citizens","hromada_problem_info/bussiness","hromada_problem_info/experts","hromada_problem_info/ngo","hromada_problem_info/nobody","hromada_problem_consultation","hromada_problem_consultation/idp","hromada_problem_consultation/citizens","hromada_problem_consultation/bussiness","hromada_problem_consultation/experts","hromada_problem_consultation/ngo","hromada_problem_consultation/nobody","hromada_problem_proposition","hromada_problem_proposition/idp","hromada_problem_proposition/citizens","hromada_problem_proposition/bussiness","hromada_problem_proposition/experts","hromada_problem_proposition/ngo","hromada_problem_proposition/nobody","hromada_problem_system","hromada_problem_system/idp","hromada_problem_system/citizens","hromada_problem_system/bussiness","hromada_problem_system/experts","hromada_problem_system/ngo","hromada_problem_system/nobody","hromada_problem_feedback","hromada_problem_feedback/idp","hromada_problem_feedback/citizens","hromada_problem_feedback/bussiness","hromada_problem_feedback/experts","hromada_problem_feedback/ngo","hromada_problem_feedback/nobody","hromada_problem_execution","hromada_problem_execution/idp","hromada_problem_execution/citizens","hromada_problem_execution/bussiness","hromada_problem_execution/experts","hromada_problem_execution/ngo","hromada_problem_execution/nobody","skills_needed","skills_needed/fundraising","skills_needed/project_management","skills_needed/longterm_planning","skills_needed/crisis_planning","skills_needed/data_analysis","skills_needed/human_resourse","skills_needed/other","skills_needed_text","contact_text","prep_count","comm_channels_count","help_military_count","hromada_cooperation_count","idp_registration_time","prep_winter_count","oblast_center","hromada_center_code","hromada_center","lat_center","lon_center","travel_time","n_settlements","square","occipied_before_2022","total_population_2022","urban_population_2022","urban_pct","budget_code","budget_name","oblast_name_en","region_en.x","region_code_en","income_total_2021","income_transfert_2021","income_military_2021","income_pdfo_2021","income_unified_tax_2021","income_property_tax_2021","income_excise_duty_2021","income_own_2021","own_income_prop_2021","transfert_prop_2021","military_tax_prop_2021","pdfo_prop_2021","unified_tax_prop_2021","property_tax_prop_2021","excise_duty_prop_2021","own_income_change","own_prop_change","total_income_change","income_own","income_total","income_transfert","dfrr_executed","turnout_2020","sex_head","age_head","education_head","incumbent","rda","not_from_here","party","enterpreuner","unemployed","priv_work","polit_work","communal_work","ngo_work","party_national_winner","no_party","male","high_educ","sum_osbb_2020","edem_total","edem_petitions","edem_consultations","edem_participatory_budget","edem_open_hromada","youth_councils","youth_centers","business_support_centers","creation_date","creation_year","time_before_24th","voluntary","income_own_per_capita","income_total_per_capita","income_tranfert_per_capita","idp_registration_share","idp_real_share","idp_child_share","idp_help_count","occupation_and_combat","occupation_and_combat_fct","occupation_fct","voluntary_fct"],["dbl","dat","dbl","chr","chr","chr","chr","chr","chr","chr","chr","chr","dbl","fct","chr","chr","dbl","dbl","dbl","chr","dbl","dbl","dbl","dbl","dbl","dbl","dbl","dbl","dbl","dbl","dbl","dbl","dbl","dbl","dbl","chr","dbl","chr","dbl","dbl","dbl","dbl","dbl","dbl","chr","chr","chr","chr","dat","chr","dbl","dbl","dbl","dbl","dbl","dbl","chr","chr","chr","dbl","dbl","dbl","dbl","chr","chr","chr","dat","dbl","dbl","chr","dbl","dbl","dbl","dbl","dbl","dbl","dbl","dbl","dbl","chr","chr","dbl","chr","chr","dbl","dbl","dbl","dbl","dbl","dbl","dbl","dbl","dbl","dbl","chr","chr","chr","dbl","dbl","dbl","dbl","dbl","chr","chr","chr","dbl","dbl","dbl","dbl","dbl","dbl","chr","chr","chr","chr","chr","chr","chr","chr","chr","chr","chr","chr","dbl","dbl","dbl","dbl","dbl","dbl","chr","chr","chr","chr","dbl","dbl","dbl","dbl","dbl","dbl","chr","dbl","dbl","dbl","dbl","dbl","dbl","chr","dbl","dbl","dbl","dbl","dbl","dbl","chr","dbl","dbl","dbl","dbl","dbl","dbl","chr","dbl","dbl","dbl","dbl","dbl","dbl","chr","dbl","dbl","dbl","dbl","dbl","dbl","chr","dbl","dbl","dbl","dbl","dbl","dbl","dbl","chr","chr","dbl","dbl","dbl","dbl","chr","dbl","dbl","chr","chr","dbl","dbl","dbl","dbl","dbl","dbl","dbl","dbl","dbl","chr","chr","chr","chr","chr","dbl","dbl","dbl","dbl","dbl","dbl","dbl","dbl","dbl","dbl","dbl","dbl","dbl","dbl","dbl","dbl","dbl","dbl","dbl","dbl","dbl","dbl","dbl","chr","dbl","chr","dbl","dbl","dbl","chr","dbl","dbl","dbl","dbl","dbl","dbl","dbl","dbl","dbl","dbl","dbl","dbl","dbl","dbl","dbl","dbl","dbl","dbl","dbl","dat","dbl","dbl","dbl","dbl","dbl","dbl","dbl","dbl","dbl","dbl","dbl","fct","fct","fct"],[0,0,0,0,0,0,0,0,0,0,0,0,0,0,1,1,1,1,1,0,4,6,1,2,7,7,5,7,2,6,9,4,7,10,10,0,12,0,12,0,0,0,0,0,81,6,0,0,41,6,6,6,6,6,6,6,83,46,46,50,50,1,0,12,0,8,16,9,16,8,8,8,8,8,8,8,8,8,8,46,131,15,0,77,77,77,77,77,77,77,77,77,77,77,11,8,8,8,8,8,8,8,99,132,132,132,132,132,132,132,132,136,0,100,100,100,100,105,105,129,14,105,8,16,11,13,12,17,27,22,33,0,26,26,26,26,26,26,26,26,26,26,26,26,26,26,26,26,26,26,26,26,26,26,26,26,26,26,26,26,26,26,26,26,26,26,26,26,26,26,26,26,26,26,0,0,0,0,0,0,0,0,130,15,0,0,6,0,41,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,44,1,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,61,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,9,16,16,8,0,0,0,0],[0,0,0,0,0,0,0,0,0,0,0,0,0,0,0.7,0.7,0.7,0.7,0.7,0,2.9,4.3,0.7,1.4,5.1,5.1,3.6,5.1,1.4,4.3,6.5,2.9,5.1,7.2,7.2,0,8.7,0,8.7,0,0,0,0,0,58.7,4.3,0,0,29.7,4.3,4.3,4.3,4.3,4.3,4.3,4.3,60.1,33.3,33.3,36.2,36.2,0.7,0,8.7,0,5.8,11.6,6.5,11.6,5.8,5.8,5.8,5.8,5.8,5.8,5.8,5.8,5.8,5.8,33.3,94.9,10.9,0,55.8,55.8,55.8,55.8,55.8,55.8,55.8,55.8,55.8,55.8,55.8,8,5.8,5.8,5.8,5.8,5.8,5.8,5.8,71.7,95.7,95.7,95.7,95.7,95.7,95.7,95.7,95.7,98.6,0,72.5,72.5,72.5,72.5,76.1,76.1,93.5,10.1,76.1,5.8,11.6,8,9.4,8.7,12.3,19.6,15.9,23.9,0,18.8,18.8,18.8,18.8,18.8,18.8,18.8,18.8,18.8,18.8,18.8,18.8,18.8,18.8,18.8,18.8,18.8,18.8,18.8,18.8,18.8,18.8,18.8,18.8,18.8,18.8,18.8,18.8,18.8,18.8,18.8,18.8,18.8,18.8,18.8,18.8,18.8,18.8,18.8,18.8,18.8,18.8,0,0,0,0,0,0,0,0,94.2,10.9,0,0,4.3,0,29.7,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,31.9,0.7,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,44.2,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,6.5,11.6,11.6,5.8,0,0,0,0],[138,30,138,138,135,137,76,76,22,22,5,5,2,3,5,4,120,11,15,2,4,4,4,4,4,4,4,4,4,4,4,4,4,4,4,105,78,106,91,3,3,3,3,3,58,133,5,3,51,20,3,3,3,3,3,3,56,23,32,14,22,26,29,6,3,2,32,120,110,68,3,3,3,3,3,3,3,3,3,6,8,79,2,45,3,3,3,3,3,3,3,3,3,3,12,5,11,3,3,3,3,3,32,3,5,3,3,2,3,3,3,3,2,5,3,3,3,3,3,8,11,6,109,82,3,3,3,3,3,61,37,2,18,3,3,3,3,3,3,23,3,3,3,3,3,3,21,3,3,3,3,3,3,24,3,3,3,3,3,3,23,3,3,3,3,3,3,22,3,3,3,3,3,3,42,2,2,2,2,2,2,2,9,124,22,11,6,4,51,6,2,138,138,138,138,134,54,137,1,138,95,96,138,137,22,5,5,138,138,91,138,138,138,138,138,52,52,10,42,12,30,15,71,42,58,138,138,138,95,138,2,35,2,2,2,2,24,2,2,2,2,2,1,2,2,2,2,38,5,2,2,2,2,3,4,9,15,6,15,2,138,138,138,130,123,117,10,2,2,2,2],[2,null,191541757,null,null,null,null,null,null,null,null,null,0,null,null,null,140,0,0,null,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,null,0,null,0,0,0,0,0,0,null,null,null,null,null,null,0,0,0,0,0,0,null,null,null,0,0,0,0,null,null,null,null,23,23,null,0,0,0,0,0,0,0,0,0,null,null,0,null,null,0,0,0,0,0,0,0,0,0,0,null,null,null,0,0,0,0,0,null,null,null,0,0,0,0,0,0,null,null,null,null,null,null,null,null,null,null,null,null,0,0,0,0,0,0,null,null,null,null,0,0,0,0,0,0,null,0,0,0,0,0,0,null,0,0,0,0,0,0,null,0,0,0,0,0,0,null,0,0,0,0,0,0,null,0,0,0,0,0,0,null,0,0,0,0,0,0,0,null,null,2,0,0,0,null,0,0,null,null,45.68,22.49,0,1,42.2,0,3359,0,0,null,null,null,null,null,10846101.81,5163331,0,1056172.94,227066.07,224034.84,8271,3131966.65,0.14,0.14,0,0.09,0.01,0.01,0,-0.83,-0.39,-0.43,1972353.16,11030764.44,5642000,78.5,0.27,null,32,null,0,0,0,null,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,null,2015,556.79,0,507.13,2607.8,962.57,0.01,0.62,0,1,0,null,null,null],[78.2,null,197322877.2,null,null,null,null,null,null,null,null,null,0.12,null,null,null,21136.72,1.34,1.34,null,1.04,1.08,1.41,1,0.55,0.84,1.02,0.56,1.12,1.13,0.78,1.08,1.24,0.55,0.86,null,2809.67,null,5620.71,0.6,0.71,1.8,0.33,0.86,null,null,null,null,null,null,0.73,0.7,0.73,0.97,0.42,0.05,null,null,null,2.28,7.69,89.22,91.77,null,null,null,null,2001.63,2326.16,null,0.71,0.65,0.41,0.99,0.26,0.22,0.69,0.61,0.28,null,null,64.67,null,null,0.23,0.51,0.34,0.13,0.08,0.49,0.03,0.39,0.39,0.28,null,null,null,0.19,0.18,0.45,0.3,0.19,null,null,null,0.33,0.33,0,0.5,0.33,0.33,null,null,null,null,null,null,null,null,null,null,null,null,853167.02,0.86,0.93,0.9,0.43,0.34,null,null,null,null,0.38,0.64,0.58,0.09,0.35,0.08,null,0.34,0.39,0.33,0.09,0.27,0.24,null,0.28,0.46,0.49,0.08,0.29,0.21,null,0.26,0.45,0.55,0.16,0.34,0.16,null,0.36,0.45,0.47,0.11,0.33,0.16,null,0.15,0.37,0.46,0.07,0.38,0.21,null,0.75,0.41,0.32,0.49,0.26,0.32,0.06,null,null,13.72,4.29,3.13,0.08,null,3.11,0.01,null,null,49.07,29.43,93.67,22.23,410.51,0,22076.86,12499.06,0.35,null,null,null,null,null,91899785.68,37659862.26,1815527.65,31364648.7,6130599.53,8123370.3,3741911.43,54239923.43,0.51,0.49,0.01,0.27,0.06,0.1,0.03,0.04,0.01,-0.01,56792346.01,92278010.92,35485664.91,32738.42,0.42,null,52.36,null,0.54,0.07,0.11,null,0.02,0.02,0.08,0.83,0.04,0,0.16,0.43,0.27,0.93,35.48,0.62,0.22,0.16,0.15,0.09,0.1,0.22,0.56,null,2018.22,1209.18,0.58,2244.72,4224.02,1979.3,0.1,10.03,4.66,4.82,0.16,null,null,null],[151,null,206471695,null,null,null,null,null,null,null,null,null,1,null,null,null,243000,20,17,null,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,null,67520,null,216000,2,2,2,2,2,null,null,null,null,null,null,1,1,1,1,1,1,null,null,null,18,156,100,100,null,null,null,null,20000,60000,null,1,1,1,1,1,1,1,1,1,null,null,800,null,null,1,1,1,1,1,1,1,1,1,1,null,null,null,1,1,1,1,1,null,null,null,1,1,0,1,1,1,null,null,null,null,null,null,null,null,null,null,null,null,13936323,1,1,1,1,1,null,null,null,null,1,1,1,1,1,1,null,1,1,1,1,1,1,null,1,1,1,1,1,1,null,1,1,1,1,1,1,null,1,1,1,1,1,1,null,1,1,1,1,1,1,null,1,1,1,1,1,1,1,null,null,29,10,4,3,null,5,1,null,null,52.06,36.73,288,97,2497.1,0,317752,305239,1,null,null,null,null,null,1288755475.83,346574777.46,47254976.84,608781726.22,124876522.55,78663469.37,73206177.69,942180698.37,0.86,0.86,0.14,0.59,0.13,0.44,0.27,1.69,0.23,0.89,969725144.97,1248182878.17,315122334.64,757596.25,0.65,null,71,null,1,1,1,null,1,1,1,1,1,0,1,1,1,1,638,4,1,1,1,1,2,4,17,null,2020,2383.79,1,7418.91,9388.75,3470.86,0.63,62.72,33.43,9,1,null,null,null]],"container":"<table class=\"cell-border stripe\">\n  <thead>\n    <tr>\n      <th> <\/th>\n      <th>variable<\/th>\n      <th>type<\/th>\n      <th>na<\/th>\n      <th>na_pct<\/th>\n      <th>unique<\/th>\n      <th>min<\/th>\n      <th>mean<\/th>\n      <th>max<\/th>\n    <\/tr>\n  <\/thead>\n<\/table>","options":{"pageLength":6,"autoWidth":false,"columnDefs":[{"className":"dt-right","targets":[3,4,5,6,7,8]},{"orderable":false,"targets":0}],"order":[],"orderClasses":false,"orderCellsTop":true,"lengthMenu":[6,10,25,50,100]}},"evals":[],"jsHooks":[]}</script>
```

# 0. Introduction

<mark>0.1</mark> What is the goal of this report?

> This report overviews the responses to the survey conducted by KSE Institute in Ukraine during 2022


# 1. General Information


```{.r .fold-hide}
meta_survey %>% filter(group=="preamble") %>% pull(label) %>% cat()
```

```
Супротив України у загарбницькій війні Російської Федерації великою мірою пов’язаний зі стійкістю, яку виявили територіальні громади. 
Щоб оцінити, як різні потрясіння вплинули на громади та перевірити, які фактори впливають на стійкість громад у війні, Київська Школа Економіки проводить опитування.
Ми гарантуємо конфіденційність Ваших відповідей. Всі отримані дані будуть аналізуватись тільки в узагальненому вигляді.
Крайній термін заповнення анкети - 4 листопада.
```

<mark>1.1</mark> How many hromadas contributed responses to so far?

> As of 2023-02-15, 138 hromadas contributed valid response to the survey

<mark>1.2</mark> What regions are represented in this sample? 


```{.r .fold-hide}
ds_survey %>% 
  group_by(region_en) %>% 
  summarize(
    hromada_count = n_distinct(hromada_code)
    ,.groups = "drop"
  ) %>%
  mutate(hromada_survey_prop = hromada_count / sum(hromada_count)
         ,hromada_survey_pct = scales::percent(hromada_survey_prop, accuracy = .1)) %>% 
  right_join(
    ds_general %>% 
      group_by(region_en) %>% 
      summarize(hromada_count_total = n())
  ) %>% 
  filter(!is.na(region_en)) %>%
  mutate(
    hromada_count = replace_na(hromada_count, 0)
    ,hromada_total_prop = hromada_count_total / sum(hromada_count_total)
    ,hromada_total_pct = scales::percent(hromada_total_prop, accuracy = .1)
  ) %>%
  arrange(region_en, desc(hromada_survey_prop)) %>% 
  select(-c(hromada_survey_prop, hromada_total_prop)) %>%
  relocate(hromada_count_total, .after = hromada_count) %>%
  ungroup() %>%
  # neat_DT()
  gt::gt() %>%
  gt::cols_label(region_en = 'Region',
                 hromada_count_total = 'Total number \nof ATCs',
                 hromada_count = 'ATCs in the survey',
                 hromada_total_pct = 'Proportion of Region \nin the General Population',
                 hromada_survey_pct = 'Proportion of Region \nin the Survey'
  )
```

```{=html}
<div id="vvvseckwsw" style="padding-left:0px;padding-right:0px;padding-top:10px;padding-bottom:10px;overflow-x:auto;overflow-y:auto;width:auto;height:auto;">
<style>html {
  font-family: -apple-system, BlinkMacSystemFont, 'Segoe UI', Roboto, Oxygen, Ubuntu, Cantarell, 'Helvetica Neue', 'Fira Sans', 'Droid Sans', Arial, sans-serif;
}

#vvvseckwsw .gt_table {
  display: table;
  border-collapse: collapse;
  margin-left: auto;
  margin-right: auto;
  color: #333333;
  font-size: 16px;
  font-weight: normal;
  font-style: normal;
  background-color: #FFFFFF;
  width: auto;
  border-top-style: solid;
  border-top-width: 2px;
  border-top-color: #A8A8A8;
  border-right-style: none;
  border-right-width: 2px;
  border-right-color: #D3D3D3;
  border-bottom-style: solid;
  border-bottom-width: 2px;
  border-bottom-color: #A8A8A8;
  border-left-style: none;
  border-left-width: 2px;
  border-left-color: #D3D3D3;
}

#vvvseckwsw .gt_heading {
  background-color: #FFFFFF;
  text-align: center;
  border-bottom-color: #FFFFFF;
  border-left-style: none;
  border-left-width: 1px;
  border-left-color: #D3D3D3;
  border-right-style: none;
  border-right-width: 1px;
  border-right-color: #D3D3D3;
}

#vvvseckwsw .gt_caption {
  padding-top: 4px;
  padding-bottom: 4px;
}

#vvvseckwsw .gt_title {
  color: #333333;
  font-size: 125%;
  font-weight: initial;
  padding-top: 4px;
  padding-bottom: 4px;
  padding-left: 5px;
  padding-right: 5px;
  border-bottom-color: #FFFFFF;
  border-bottom-width: 0;
}

#vvvseckwsw .gt_subtitle {
  color: #333333;
  font-size: 85%;
  font-weight: initial;
  padding-top: 0;
  padding-bottom: 6px;
  padding-left: 5px;
  padding-right: 5px;
  border-top-color: #FFFFFF;
  border-top-width: 0;
}

#vvvseckwsw .gt_bottom_border {
  border-bottom-style: solid;
  border-bottom-width: 2px;
  border-bottom-color: #D3D3D3;
}

#vvvseckwsw .gt_col_headings {
  border-top-style: solid;
  border-top-width: 2px;
  border-top-color: #D3D3D3;
  border-bottom-style: solid;
  border-bottom-width: 2px;
  border-bottom-color: #D3D3D3;
  border-left-style: none;
  border-left-width: 1px;
  border-left-color: #D3D3D3;
  border-right-style: none;
  border-right-width: 1px;
  border-right-color: #D3D3D3;
}

#vvvseckwsw .gt_col_heading {
  color: #333333;
  background-color: #FFFFFF;
  font-size: 100%;
  font-weight: normal;
  text-transform: inherit;
  border-left-style: none;
  border-left-width: 1px;
  border-left-color: #D3D3D3;
  border-right-style: none;
  border-right-width: 1px;
  border-right-color: #D3D3D3;
  vertical-align: bottom;
  padding-top: 5px;
  padding-bottom: 6px;
  padding-left: 5px;
  padding-right: 5px;
  overflow-x: hidden;
}

#vvvseckwsw .gt_column_spanner_outer {
  color: #333333;
  background-color: #FFFFFF;
  font-size: 100%;
  font-weight: normal;
  text-transform: inherit;
  padding-top: 0;
  padding-bottom: 0;
  padding-left: 4px;
  padding-right: 4px;
}

#vvvseckwsw .gt_column_spanner_outer:first-child {
  padding-left: 0;
}

#vvvseckwsw .gt_column_spanner_outer:last-child {
  padding-right: 0;
}

#vvvseckwsw .gt_column_spanner {
  border-bottom-style: solid;
  border-bottom-width: 2px;
  border-bottom-color: #D3D3D3;
  vertical-align: bottom;
  padding-top: 5px;
  padding-bottom: 5px;
  overflow-x: hidden;
  display: inline-block;
  width: 100%;
}

#vvvseckwsw .gt_group_heading {
  padding-top: 8px;
  padding-bottom: 8px;
  padding-left: 5px;
  padding-right: 5px;
  color: #333333;
  background-color: #FFFFFF;
  font-size: 100%;
  font-weight: initial;
  text-transform: inherit;
  border-top-style: solid;
  border-top-width: 2px;
  border-top-color: #D3D3D3;
  border-bottom-style: solid;
  border-bottom-width: 2px;
  border-bottom-color: #D3D3D3;
  border-left-style: none;
  border-left-width: 1px;
  border-left-color: #D3D3D3;
  border-right-style: none;
  border-right-width: 1px;
  border-right-color: #D3D3D3;
  vertical-align: middle;
  text-align: left;
}

#vvvseckwsw .gt_empty_group_heading {
  padding: 0.5px;
  color: #333333;
  background-color: #FFFFFF;
  font-size: 100%;
  font-weight: initial;
  border-top-style: solid;
  border-top-width: 2px;
  border-top-color: #D3D3D3;
  border-bottom-style: solid;
  border-bottom-width: 2px;
  border-bottom-color: #D3D3D3;
  vertical-align: middle;
}

#vvvseckwsw .gt_from_md > :first-child {
  margin-top: 0;
}

#vvvseckwsw .gt_from_md > :last-child {
  margin-bottom: 0;
}

#vvvseckwsw .gt_row {
  padding-top: 8px;
  padding-bottom: 8px;
  padding-left: 5px;
  padding-right: 5px;
  margin: 10px;
  border-top-style: solid;
  border-top-width: 1px;
  border-top-color: #D3D3D3;
  border-left-style: none;
  border-left-width: 1px;
  border-left-color: #D3D3D3;
  border-right-style: none;
  border-right-width: 1px;
  border-right-color: #D3D3D3;
  vertical-align: middle;
  overflow-x: hidden;
}

#vvvseckwsw .gt_stub {
  color: #333333;
  background-color: #FFFFFF;
  font-size: 100%;
  font-weight: initial;
  text-transform: inherit;
  border-right-style: solid;
  border-right-width: 2px;
  border-right-color: #D3D3D3;
  padding-left: 5px;
  padding-right: 5px;
}

#vvvseckwsw .gt_stub_row_group {
  color: #333333;
  background-color: #FFFFFF;
  font-size: 100%;
  font-weight: initial;
  text-transform: inherit;
  border-right-style: solid;
  border-right-width: 2px;
  border-right-color: #D3D3D3;
  padding-left: 5px;
  padding-right: 5px;
  vertical-align: top;
}

#vvvseckwsw .gt_row_group_first td {
  border-top-width: 2px;
}

#vvvseckwsw .gt_summary_row {
  color: #333333;
  background-color: #FFFFFF;
  text-transform: inherit;
  padding-top: 8px;
  padding-bottom: 8px;
  padding-left: 5px;
  padding-right: 5px;
}

#vvvseckwsw .gt_first_summary_row {
  border-top-style: solid;
  border-top-color: #D3D3D3;
}

#vvvseckwsw .gt_first_summary_row.thick {
  border-top-width: 2px;
}

#vvvseckwsw .gt_last_summary_row {
  padding-top: 8px;
  padding-bottom: 8px;
  padding-left: 5px;
  padding-right: 5px;
  border-bottom-style: solid;
  border-bottom-width: 2px;
  border-bottom-color: #D3D3D3;
}

#vvvseckwsw .gt_grand_summary_row {
  color: #333333;
  background-color: #FFFFFF;
  text-transform: inherit;
  padding-top: 8px;
  padding-bottom: 8px;
  padding-left: 5px;
  padding-right: 5px;
}

#vvvseckwsw .gt_first_grand_summary_row {
  padding-top: 8px;
  padding-bottom: 8px;
  padding-left: 5px;
  padding-right: 5px;
  border-top-style: double;
  border-top-width: 6px;
  border-top-color: #D3D3D3;
}

#vvvseckwsw .gt_striped {
  background-color: rgba(128, 128, 128, 0.05);
}

#vvvseckwsw .gt_table_body {
  border-top-style: solid;
  border-top-width: 2px;
  border-top-color: #D3D3D3;
  border-bottom-style: solid;
  border-bottom-width: 2px;
  border-bottom-color: #D3D3D3;
}

#vvvseckwsw .gt_footnotes {
  color: #333333;
  background-color: #FFFFFF;
  border-bottom-style: none;
  border-bottom-width: 2px;
  border-bottom-color: #D3D3D3;
  border-left-style: none;
  border-left-width: 2px;
  border-left-color: #D3D3D3;
  border-right-style: none;
  border-right-width: 2px;
  border-right-color: #D3D3D3;
}

#vvvseckwsw .gt_footnote {
  margin: 0px;
  font-size: 90%;
  padding-left: 4px;
  padding-right: 4px;
  padding-left: 5px;
  padding-right: 5px;
}

#vvvseckwsw .gt_sourcenotes {
  color: #333333;
  background-color: #FFFFFF;
  border-bottom-style: none;
  border-bottom-width: 2px;
  border-bottom-color: #D3D3D3;
  border-left-style: none;
  border-left-width: 2px;
  border-left-color: #D3D3D3;
  border-right-style: none;
  border-right-width: 2px;
  border-right-color: #D3D3D3;
}

#vvvseckwsw .gt_sourcenote {
  font-size: 90%;
  padding-top: 4px;
  padding-bottom: 4px;
  padding-left: 5px;
  padding-right: 5px;
}

#vvvseckwsw .gt_left {
  text-align: left;
}

#vvvseckwsw .gt_center {
  text-align: center;
}

#vvvseckwsw .gt_right {
  text-align: right;
  font-variant-numeric: tabular-nums;
}

#vvvseckwsw .gt_font_normal {
  font-weight: normal;
}

#vvvseckwsw .gt_font_bold {
  font-weight: bold;
}

#vvvseckwsw .gt_font_italic {
  font-style: italic;
}

#vvvseckwsw .gt_super {
  font-size: 65%;
}

#vvvseckwsw .gt_footnote_marks {
  font-style: italic;
  font-weight: normal;
  font-size: 75%;
  vertical-align: 0.4em;
}

#vvvseckwsw .gt_asterisk {
  font-size: 100%;
  vertical-align: 0;
}

#vvvseckwsw .gt_indent_1 {
  text-indent: 5px;
}

#vvvseckwsw .gt_indent_2 {
  text-indent: 10px;
}

#vvvseckwsw .gt_indent_3 {
  text-indent: 15px;
}

#vvvseckwsw .gt_indent_4 {
  text-indent: 20px;
}

#vvvseckwsw .gt_indent_5 {
  text-indent: 25px;
}
</style>
<table class="gt_table">
  
  <thead class="gt_col_headings">
    <tr>
      <th class="gt_col_heading gt_columns_bottom_border gt_left" rowspan="1" colspan="1" scope="col" id="Region">Region</th>
      <th class="gt_col_heading gt_columns_bottom_border gt_right" rowspan="1" colspan="1" scope="col" id="ATCs in the survey">ATCs in the survey</th>
      <th class="gt_col_heading gt_columns_bottom_border gt_right" rowspan="1" colspan="1" scope="col" id="Total number &#10;of ATCs">Total number 
of ATCs</th>
      <th class="gt_col_heading gt_columns_bottom_border gt_right" rowspan="1" colspan="1" scope="col" id="Proportion of Region &#10;in the Survey">Proportion of Region 
in the Survey</th>
      <th class="gt_col_heading gt_columns_bottom_border gt_right" rowspan="1" colspan="1" scope="col" id="Proportion of Region &#10;in the General Population">Proportion of Region 
in the General Population</th>
    </tr>
  </thead>
  <tbody class="gt_table_body">
    <tr><td headers="region_en" class="gt_row gt_left">Center</td>
<td headers="hromada_count" class="gt_row gt_right">24</td>
<td headers="hromada_count_total" class="gt_row gt_right">298</td>
<td headers="hromada_survey_pct" class="gt_row gt_right">17.4%</td>
<td headers="hromada_total_pct" class="gt_row gt_right">20.7%</td></tr>
    <tr><td headers="region_en" class="gt_row gt_left">East</td>
<td headers="hromada_count" class="gt_row gt_right">19</td>
<td headers="hromada_count_total" class="gt_row gt_right">281</td>
<td headers="hromada_survey_pct" class="gt_row gt_right">13.8%</td>
<td headers="hromada_total_pct" class="gt_row gt_right">19.5%</td></tr>
    <tr><td headers="region_en" class="gt_row gt_left">North</td>
<td headers="hromada_count" class="gt_row gt_right">23</td>
<td headers="hromada_count_total" class="gt_row gt_right">243</td>
<td headers="hromada_survey_pct" class="gt_row gt_right">16.7%</td>
<td headers="hromada_total_pct" class="gt_row gt_right">16.9%</td></tr>
    <tr><td headers="region_en" class="gt_row gt_left">South</td>
<td headers="hromada_count" class="gt_row gt_right">20</td>
<td headers="hromada_count_total" class="gt_row gt_right">192</td>
<td headers="hromada_survey_pct" class="gt_row gt_right">14.5%</td>
<td headers="hromada_total_pct" class="gt_row gt_right">13.4%</td></tr>
    <tr><td headers="region_en" class="gt_row gt_left">West</td>
<td headers="hromada_count" class="gt_row gt_right">52</td>
<td headers="hromada_count_total" class="gt_row gt_right">424</td>
<td headers="hromada_survey_pct" class="gt_row gt_right">37.7%</td>
<td headers="hromada_total_pct" class="gt_row gt_right">29.5%</td></tr>
  </tbody>
  
  
</table>
</div>
```


<mark>1.3</mark> What oblasts are represented in this sample? 


```{.r .fold-hide}
ds0 %>% 
  summarize(
    hromada_count = n_distinct(hromada_code)
    ,deoccupied_count = sum(deoccupied_at_feb_2023)
    ,.by = c(region_en, oblast_name_en)
  ) %>%
  mutate(hromada_survey_prop = hromada_count / sum(hromada_count)
         ,hromada_survey_pct = scales::percent(hromada_survey_prop, accuracy = .1)) %>% 
  right_join(
    ds_general1 %>% 
      group_by(region_en, oblast_name_en) %>% 
      summarize(hromada_count_total = n())
  ) %>% 
  filter(!is.na(region_en)) %>%
  mutate(
    hromada_count = replace_na(hromada_count, 0)
    ,hromada_total_prop = hromada_count_total / sum(hromada_count_total)
    ,hromada_total_pct = scales::percent(hromada_total_prop, accuracy = .1)
  ) %>%
  arrange(region_en, desc(hromada_survey_prop)) %>% 
  select(-c(hromada_survey_prop, hromada_total_prop)) %>%
  relocate(hromada_count_total, .after = hromada_count) %>%
  ungroup() %>%
  mutate(oblast_name_en = case_when(oblast_name_en == "Vonyn" ~ "Volyn",
                                    oblast_name_en == "Driproptrovska" ~ "Dnipro",
                                    TRUE ~ oblast_name_en)) %>%
  # neat_DT()
  gt::gt() %>%
  gt::cols_label(region_en = 'Region',
                 oblast_name_en = 'Oblast',
                 hromada_count_total = 'Total number \nof ATCs',
                 hromada_count = 'ATCs in the survey',
                 deoccupied_count = "Deoccupied at Feb 2023 ATCs",
                 hromada_total_pct = 'Proportion of Oblast \nin the General Population',
                 hromada_survey_pct = 'Proportion of Oblast \nin the Survey'
  ) %>%
  tab_row_group(
    label = "Center"
    ,rows = region_en == "Center"
  ) %>%
  tab_row_group(
    label = "East"
    ,rows = region_en == "East"
  ) %>%
    tab_row_group(
    label = "North"
    ,rows = region_en == "North"
  ) %>%
    tab_row_group(
    label = "South"
    ,rows = region_en == "South"
  ) %>%
    tab_row_group(
    label = "West"
    ,rows = region_en == "West"
    ) %>%
  cols_hide(region_en)
```

```{=html}
<div id="pbpadersza" style="padding-left:0px;padding-right:0px;padding-top:10px;padding-bottom:10px;overflow-x:auto;overflow-y:auto;width:auto;height:auto;">
<style>html {
  font-family: -apple-system, BlinkMacSystemFont, 'Segoe UI', Roboto, Oxygen, Ubuntu, Cantarell, 'Helvetica Neue', 'Fira Sans', 'Droid Sans', Arial, sans-serif;
}

#pbpadersza .gt_table {
  display: table;
  border-collapse: collapse;
  margin-left: auto;
  margin-right: auto;
  color: #333333;
  font-size: 16px;
  font-weight: normal;
  font-style: normal;
  background-color: #FFFFFF;
  width: auto;
  border-top-style: solid;
  border-top-width: 2px;
  border-top-color: #A8A8A8;
  border-right-style: none;
  border-right-width: 2px;
  border-right-color: #D3D3D3;
  border-bottom-style: solid;
  border-bottom-width: 2px;
  border-bottom-color: #A8A8A8;
  border-left-style: none;
  border-left-width: 2px;
  border-left-color: #D3D3D3;
}

#pbpadersza .gt_heading {
  background-color: #FFFFFF;
  text-align: center;
  border-bottom-color: #FFFFFF;
  border-left-style: none;
  border-left-width: 1px;
  border-left-color: #D3D3D3;
  border-right-style: none;
  border-right-width: 1px;
  border-right-color: #D3D3D3;
}

#pbpadersza .gt_caption {
  padding-top: 4px;
  padding-bottom: 4px;
}

#pbpadersza .gt_title {
  color: #333333;
  font-size: 125%;
  font-weight: initial;
  padding-top: 4px;
  padding-bottom: 4px;
  padding-left: 5px;
  padding-right: 5px;
  border-bottom-color: #FFFFFF;
  border-bottom-width: 0;
}

#pbpadersza .gt_subtitle {
  color: #333333;
  font-size: 85%;
  font-weight: initial;
  padding-top: 0;
  padding-bottom: 6px;
  padding-left: 5px;
  padding-right: 5px;
  border-top-color: #FFFFFF;
  border-top-width: 0;
}

#pbpadersza .gt_bottom_border {
  border-bottom-style: solid;
  border-bottom-width: 2px;
  border-bottom-color: #D3D3D3;
}

#pbpadersza .gt_col_headings {
  border-top-style: solid;
  border-top-width: 2px;
  border-top-color: #D3D3D3;
  border-bottom-style: solid;
  border-bottom-width: 2px;
  border-bottom-color: #D3D3D3;
  border-left-style: none;
  border-left-width: 1px;
  border-left-color: #D3D3D3;
  border-right-style: none;
  border-right-width: 1px;
  border-right-color: #D3D3D3;
}

#pbpadersza .gt_col_heading {
  color: #333333;
  background-color: #FFFFFF;
  font-size: 100%;
  font-weight: normal;
  text-transform: inherit;
  border-left-style: none;
  border-left-width: 1px;
  border-left-color: #D3D3D3;
  border-right-style: none;
  border-right-width: 1px;
  border-right-color: #D3D3D3;
  vertical-align: bottom;
  padding-top: 5px;
  padding-bottom: 6px;
  padding-left: 5px;
  padding-right: 5px;
  overflow-x: hidden;
}

#pbpadersza .gt_column_spanner_outer {
  color: #333333;
  background-color: #FFFFFF;
  font-size: 100%;
  font-weight: normal;
  text-transform: inherit;
  padding-top: 0;
  padding-bottom: 0;
  padding-left: 4px;
  padding-right: 4px;
}

#pbpadersza .gt_column_spanner_outer:first-child {
  padding-left: 0;
}

#pbpadersza .gt_column_spanner_outer:last-child {
  padding-right: 0;
}

#pbpadersza .gt_column_spanner {
  border-bottom-style: solid;
  border-bottom-width: 2px;
  border-bottom-color: #D3D3D3;
  vertical-align: bottom;
  padding-top: 5px;
  padding-bottom: 5px;
  overflow-x: hidden;
  display: inline-block;
  width: 100%;
}

#pbpadersza .gt_group_heading {
  padding-top: 8px;
  padding-bottom: 8px;
  padding-left: 5px;
  padding-right: 5px;
  color: #333333;
  background-color: #FFFFFF;
  font-size: 100%;
  font-weight: initial;
  text-transform: inherit;
  border-top-style: solid;
  border-top-width: 2px;
  border-top-color: #D3D3D3;
  border-bottom-style: solid;
  border-bottom-width: 2px;
  border-bottom-color: #D3D3D3;
  border-left-style: none;
  border-left-width: 1px;
  border-left-color: #D3D3D3;
  border-right-style: none;
  border-right-width: 1px;
  border-right-color: #D3D3D3;
  vertical-align: middle;
  text-align: left;
}

#pbpadersza .gt_empty_group_heading {
  padding: 0.5px;
  color: #333333;
  background-color: #FFFFFF;
  font-size: 100%;
  font-weight: initial;
  border-top-style: solid;
  border-top-width: 2px;
  border-top-color: #D3D3D3;
  border-bottom-style: solid;
  border-bottom-width: 2px;
  border-bottom-color: #D3D3D3;
  vertical-align: middle;
}

#pbpadersza .gt_from_md > :first-child {
  margin-top: 0;
}

#pbpadersza .gt_from_md > :last-child {
  margin-bottom: 0;
}

#pbpadersza .gt_row {
  padding-top: 8px;
  padding-bottom: 8px;
  padding-left: 5px;
  padding-right: 5px;
  margin: 10px;
  border-top-style: solid;
  border-top-width: 1px;
  border-top-color: #D3D3D3;
  border-left-style: none;
  border-left-width: 1px;
  border-left-color: #D3D3D3;
  border-right-style: none;
  border-right-width: 1px;
  border-right-color: #D3D3D3;
  vertical-align: middle;
  overflow-x: hidden;
}

#pbpadersza .gt_stub {
  color: #333333;
  background-color: #FFFFFF;
  font-size: 100%;
  font-weight: initial;
  text-transform: inherit;
  border-right-style: solid;
  border-right-width: 2px;
  border-right-color: #D3D3D3;
  padding-left: 5px;
  padding-right: 5px;
}

#pbpadersza .gt_stub_row_group {
  color: #333333;
  background-color: #FFFFFF;
  font-size: 100%;
  font-weight: initial;
  text-transform: inherit;
  border-right-style: solid;
  border-right-width: 2px;
  border-right-color: #D3D3D3;
  padding-left: 5px;
  padding-right: 5px;
  vertical-align: top;
}

#pbpadersza .gt_row_group_first td {
  border-top-width: 2px;
}

#pbpadersza .gt_summary_row {
  color: #333333;
  background-color: #FFFFFF;
  text-transform: inherit;
  padding-top: 8px;
  padding-bottom: 8px;
  padding-left: 5px;
  padding-right: 5px;
}

#pbpadersza .gt_first_summary_row {
  border-top-style: solid;
  border-top-color: #D3D3D3;
}

#pbpadersza .gt_first_summary_row.thick {
  border-top-width: 2px;
}

#pbpadersza .gt_last_summary_row {
  padding-top: 8px;
  padding-bottom: 8px;
  padding-left: 5px;
  padding-right: 5px;
  border-bottom-style: solid;
  border-bottom-width: 2px;
  border-bottom-color: #D3D3D3;
}

#pbpadersza .gt_grand_summary_row {
  color: #333333;
  background-color: #FFFFFF;
  text-transform: inherit;
  padding-top: 8px;
  padding-bottom: 8px;
  padding-left: 5px;
  padding-right: 5px;
}

#pbpadersza .gt_first_grand_summary_row {
  padding-top: 8px;
  padding-bottom: 8px;
  padding-left: 5px;
  padding-right: 5px;
  border-top-style: double;
  border-top-width: 6px;
  border-top-color: #D3D3D3;
}

#pbpadersza .gt_striped {
  background-color: rgba(128, 128, 128, 0.05);
}

#pbpadersza .gt_table_body {
  border-top-style: solid;
  border-top-width: 2px;
  border-top-color: #D3D3D3;
  border-bottom-style: solid;
  border-bottom-width: 2px;
  border-bottom-color: #D3D3D3;
}

#pbpadersza .gt_footnotes {
  color: #333333;
  background-color: #FFFFFF;
  border-bottom-style: none;
  border-bottom-width: 2px;
  border-bottom-color: #D3D3D3;
  border-left-style: none;
  border-left-width: 2px;
  border-left-color: #D3D3D3;
  border-right-style: none;
  border-right-width: 2px;
  border-right-color: #D3D3D3;
}

#pbpadersza .gt_footnote {
  margin: 0px;
  font-size: 90%;
  padding-left: 4px;
  padding-right: 4px;
  padding-left: 5px;
  padding-right: 5px;
}

#pbpadersza .gt_sourcenotes {
  color: #333333;
  background-color: #FFFFFF;
  border-bottom-style: none;
  border-bottom-width: 2px;
  border-bottom-color: #D3D3D3;
  border-left-style: none;
  border-left-width: 2px;
  border-left-color: #D3D3D3;
  border-right-style: none;
  border-right-width: 2px;
  border-right-color: #D3D3D3;
}

#pbpadersza .gt_sourcenote {
  font-size: 90%;
  padding-top: 4px;
  padding-bottom: 4px;
  padding-left: 5px;
  padding-right: 5px;
}

#pbpadersza .gt_left {
  text-align: left;
}

#pbpadersza .gt_center {
  text-align: center;
}

#pbpadersza .gt_right {
  text-align: right;
  font-variant-numeric: tabular-nums;
}

#pbpadersza .gt_font_normal {
  font-weight: normal;
}

#pbpadersza .gt_font_bold {
  font-weight: bold;
}

#pbpadersza .gt_font_italic {
  font-style: italic;
}

#pbpadersza .gt_super {
  font-size: 65%;
}

#pbpadersza .gt_footnote_marks {
  font-style: italic;
  font-weight: normal;
  font-size: 75%;
  vertical-align: 0.4em;
}

#pbpadersza .gt_asterisk {
  font-size: 100%;
  vertical-align: 0;
}

#pbpadersza .gt_indent_1 {
  text-indent: 5px;
}

#pbpadersza .gt_indent_2 {
  text-indent: 10px;
}

#pbpadersza .gt_indent_3 {
  text-indent: 15px;
}

#pbpadersza .gt_indent_4 {
  text-indent: 20px;
}

#pbpadersza .gt_indent_5 {
  text-indent: 25px;
}
</style>
<table class="gt_table">
  
  <thead class="gt_col_headings">
    <tr>
      <th class="gt_col_heading gt_columns_bottom_border gt_left" rowspan="1" colspan="1" scope="col" id="Oblast">Oblast</th>
      <th class="gt_col_heading gt_columns_bottom_border gt_right" rowspan="1" colspan="1" scope="col" id="ATCs in the survey">ATCs in the survey</th>
      <th class="gt_col_heading gt_columns_bottom_border gt_right" rowspan="1" colspan="1" scope="col" id="Total number &#10;of ATCs">Total number 
of ATCs</th>
      <th class="gt_col_heading gt_columns_bottom_border gt_right" rowspan="1" colspan="1" scope="col" id="Deoccupied at Feb 2023 ATCs">Deoccupied at Feb 2023 ATCs</th>
      <th class="gt_col_heading gt_columns_bottom_border gt_right" rowspan="1" colspan="1" scope="col" id="Proportion of Oblast &#10;in the Survey">Proportion of Oblast 
in the Survey</th>
      <th class="gt_col_heading gt_columns_bottom_border gt_right" rowspan="1" colspan="1" scope="col" id="Proportion of Oblast &#10;in the General Population">Proportion of Oblast 
in the General Population</th>
    </tr>
  </thead>
  <tbody class="gt_table_body">
    <tr class="gt_group_heading_row">
      <th colspan="6" class="gt_group_heading" scope="colgroup" id="West">West</th>
    </tr>
    <tr class="gt_row_group_first"><td headers="West  oblast_name_en" class="gt_row gt_left">Ivano-Frankivsk</td>
<td headers="West  hromada_count" class="gt_row gt_right">16</td>
<td headers="West  hromada_count_total" class="gt_row gt_right">62</td>
<td headers="West  deoccupied_count" class="gt_row gt_right">0</td>
<td headers="West  hromada_survey_pct" class="gt_row gt_right">11.6%</td>
<td headers="West  hromada_total_pct" class="gt_row gt_right">4.3%</td></tr>
    <tr><td headers="West  oblast_name_en" class="gt_row gt_left">Zakarpatska</td>
<td headers="West  hromada_count" class="gt_row gt_right">11</td>
<td headers="West  hromada_count_total" class="gt_row gt_right">64</td>
<td headers="West  deoccupied_count" class="gt_row gt_right">0</td>
<td headers="West  hromada_survey_pct" class="gt_row gt_right">8.0%</td>
<td headers="West  hromada_total_pct" class="gt_row gt_right">4.5%</td></tr>
    <tr><td headers="West  oblast_name_en" class="gt_row gt_left">Lviv</td>
<td headers="West  hromada_count" class="gt_row gt_right">9</td>
<td headers="West  hromada_count_total" class="gt_row gt_right">73</td>
<td headers="West  deoccupied_count" class="gt_row gt_right">0</td>
<td headers="West  hromada_survey_pct" class="gt_row gt_right">6.5%</td>
<td headers="West  hromada_total_pct" class="gt_row gt_right">5.1%</td></tr>
    <tr><td headers="West  oblast_name_en" class="gt_row gt_left">Volyn</td>
<td headers="West  hromada_count" class="gt_row gt_right">5</td>
<td headers="West  hromada_count_total" class="gt_row gt_right">54</td>
<td headers="West  deoccupied_count" class="gt_row gt_right">0</td>
<td headers="West  hromada_survey_pct" class="gt_row gt_right">3.6%</td>
<td headers="West  hromada_total_pct" class="gt_row gt_right">3.8%</td></tr>
    <tr><td headers="West  oblast_name_en" class="gt_row gt_left">Rivenska</td>
<td headers="West  hromada_count" class="gt_row gt_right">4</td>
<td headers="West  hromada_count_total" class="gt_row gt_right">64</td>
<td headers="West  deoccupied_count" class="gt_row gt_right">0</td>
<td headers="West  hromada_survey_pct" class="gt_row gt_right">2.9%</td>
<td headers="West  hromada_total_pct" class="gt_row gt_right">4.5%</td></tr>
    <tr><td headers="West  oblast_name_en" class="gt_row gt_left">Ternopilska</td>
<td headers="West  hromada_count" class="gt_row gt_right">4</td>
<td headers="West  hromada_count_total" class="gt_row gt_right">55</td>
<td headers="West  deoccupied_count" class="gt_row gt_right">0</td>
<td headers="West  hromada_survey_pct" class="gt_row gt_right">2.9%</td>
<td headers="West  hromada_total_pct" class="gt_row gt_right">3.8%</td></tr>
    <tr><td headers="West  oblast_name_en" class="gt_row gt_left">Cherniveska</td>
<td headers="West  hromada_count" class="gt_row gt_right">3</td>
<td headers="West  hromada_count_total" class="gt_row gt_right">52</td>
<td headers="West  deoccupied_count" class="gt_row gt_right">0</td>
<td headers="West  hromada_survey_pct" class="gt_row gt_right">2.2%</td>
<td headers="West  hromada_total_pct" class="gt_row gt_right">3.6%</td></tr>
    <tr class="gt_group_heading_row">
      <th colspan="6" class="gt_group_heading" scope="colgroup" id="South">South</th>
    </tr>
    <tr class="gt_row_group_first"><td headers="South  oblast_name_en" class="gt_row gt_left">Odesa</td>
<td headers="South  hromada_count" class="gt_row gt_right">12</td>
<td headers="South  hromada_count_total" class="gt_row gt_right">91</td>
<td headers="South  deoccupied_count" class="gt_row gt_right">0</td>
<td headers="South  hromada_survey_pct" class="gt_row gt_right">8.7%</td>
<td headers="South  hromada_total_pct" class="gt_row gt_right">6.3%</td></tr>
    <tr><td headers="South  oblast_name_en" class="gt_row gt_left">Kherson</td>
<td headers="South  hromada_count" class="gt_row gt_right">7</td>
<td headers="South  hromada_count_total" class="gt_row gt_right">49</td>
<td headers="South  deoccupied_count" class="gt_row gt_right">4</td>
<td headers="South  hromada_survey_pct" class="gt_row gt_right">5.1%</td>
<td headers="South  hromada_total_pct" class="gt_row gt_right">3.4%</td></tr>
    <tr><td headers="South  oblast_name_en" class="gt_row gt_left">Mykolayiv</td>
<td headers="South  hromada_count" class="gt_row gt_right">1</td>
<td headers="South  hromada_count_total" class="gt_row gt_right">52</td>
<td headers="South  deoccupied_count" class="gt_row gt_right">0</td>
<td headers="South  hromada_survey_pct" class="gt_row gt_right">0.7%</td>
<td headers="South  hromada_total_pct" class="gt_row gt_right">3.6%</td></tr>
    <tr class="gt_group_heading_row">
      <th colspan="6" class="gt_group_heading" scope="colgroup" id="North">North</th>
    </tr>
    <tr class="gt_row_group_first"><td headers="North  oblast_name_en" class="gt_row gt_left">Sumska</td>
<td headers="North  hromada_count" class="gt_row gt_right">8</td>
<td headers="North  hromada_count_total" class="gt_row gt_right">51</td>
<td headers="North  deoccupied_count" class="gt_row gt_right">6</td>
<td headers="North  hromada_survey_pct" class="gt_row gt_right">5.8%</td>
<td headers="North  hromada_total_pct" class="gt_row gt_right">3.5%</td></tr>
    <tr><td headers="North  oblast_name_en" class="gt_row gt_left">Chernigiv</td>
<td headers="North  hromada_count" class="gt_row gt_right">7</td>
<td headers="North  hromada_count_total" class="gt_row gt_right">57</td>
<td headers="North  deoccupied_count" class="gt_row gt_right">5</td>
<td headers="North  hromada_survey_pct" class="gt_row gt_right">5.1%</td>
<td headers="North  hromada_total_pct" class="gt_row gt_right">4.0%</td></tr>
    <tr><td headers="North  oblast_name_en" class="gt_row gt_left">Kyiv-oblast</td>
<td headers="North  hromada_count" class="gt_row gt_right">5</td>
<td headers="North  hromada_count_total" class="gt_row gt_right">69</td>
<td headers="North  deoccupied_count" class="gt_row gt_right">1</td>
<td headers="North  hromada_survey_pct" class="gt_row gt_right">3.6%</td>
<td headers="North  hromada_total_pct" class="gt_row gt_right">4.8%</td></tr>
    <tr><td headers="North  oblast_name_en" class="gt_row gt_left">Zhytomir</td>
<td headers="North  hromada_count" class="gt_row gt_right">3</td>
<td headers="North  hromada_count_total" class="gt_row gt_right">66</td>
<td headers="North  deoccupied_count" class="gt_row gt_right">0</td>
<td headers="North  hromada_survey_pct" class="gt_row gt_right">2.2%</td>
<td headers="North  hromada_total_pct" class="gt_row gt_right">4.6%</td></tr>
    <tr class="gt_group_heading_row">
      <th colspan="6" class="gt_group_heading" scope="colgroup" id="East">East</th>
    </tr>
    <tr class="gt_row_group_first"><td headers="East  oblast_name_en" class="gt_row gt_left">Dnipropetrovska</td>
<td headers="East  hromada_count" class="gt_row gt_right">12</td>
<td headers="East  hromada_count_total" class="gt_row gt_right">86</td>
<td headers="East  deoccupied_count" class="gt_row gt_right">0</td>
<td headers="East  hromada_survey_pct" class="gt_row gt_right">8.7%</td>
<td headers="East  hromada_total_pct" class="gt_row gt_right">6.0%</td></tr>
    <tr><td headers="East  oblast_name_en" class="gt_row gt_left">Kharkiv</td>
<td headers="East  hromada_count" class="gt_row gt_right">4</td>
<td headers="East  hromada_count_total" class="gt_row gt_right">56</td>
<td headers="East  deoccupied_count" class="gt_row gt_right">0</td>
<td headers="East  hromada_survey_pct" class="gt_row gt_right">2.9%</td>
<td headers="East  hromada_total_pct" class="gt_row gt_right">3.9%</td></tr>
    <tr><td headers="East  oblast_name_en" class="gt_row gt_left">Zaporizka</td>
<td headers="East  hromada_count" class="gt_row gt_right">3</td>
<td headers="East  hromada_count_total" class="gt_row gt_right">67</td>
<td headers="East  deoccupied_count" class="gt_row gt_right">0</td>
<td headers="East  hromada_survey_pct" class="gt_row gt_right">2.2%</td>
<td headers="East  hromada_total_pct" class="gt_row gt_right">4.7%</td></tr>
    <tr><td headers="East  oblast_name_en" class="gt_row gt_left">Donetks</td>
<td headers="East  hromada_count" class="gt_row gt_right">0</td>
<td headers="East  hromada_count_total" class="gt_row gt_right">46</td>
<td headers="East  deoccupied_count" class="gt_row gt_right">NA</td>
<td headers="East  hromada_survey_pct" class="gt_row gt_right">NA</td>
<td headers="East  hromada_total_pct" class="gt_row gt_right">3.2%</td></tr>
    <tr><td headers="East  oblast_name_en" class="gt_row gt_left">Luhansk</td>
<td headers="East  hromada_count" class="gt_row gt_right">0</td>
<td headers="East  hromada_count_total" class="gt_row gt_right">26</td>
<td headers="East  deoccupied_count" class="gt_row gt_right">NA</td>
<td headers="East  hromada_survey_pct" class="gt_row gt_right">NA</td>
<td headers="East  hromada_total_pct" class="gt_row gt_right">1.8%</td></tr>
    <tr class="gt_group_heading_row">
      <th colspan="6" class="gt_group_heading" scope="colgroup" id="Center">Center</th>
    </tr>
    <tr class="gt_row_group_first"><td headers="Center  oblast_name_en" class="gt_row gt_left">Poltava</td>
<td headers="Center  hromada_count" class="gt_row gt_right">8</td>
<td headers="Center  hromada_count_total" class="gt_row gt_right">60</td>
<td headers="Center  deoccupied_count" class="gt_row gt_right">0</td>
<td headers="Center  hromada_survey_pct" class="gt_row gt_right">5.8%</td>
<td headers="Center  hromada_total_pct" class="gt_row gt_right">4.2%</td></tr>
    <tr><td headers="Center  oblast_name_en" class="gt_row gt_left">Vinnytsia</td>
<td headers="Center  hromada_count" class="gt_row gt_right">5</td>
<td headers="Center  hromada_count_total" class="gt_row gt_right">63</td>
<td headers="Center  deoccupied_count" class="gt_row gt_right">0</td>
<td headers="Center  hromada_survey_pct" class="gt_row gt_right">3.6%</td>
<td headers="Center  hromada_total_pct" class="gt_row gt_right">4.4%</td></tr>
    <tr><td headers="Center  oblast_name_en" class="gt_row gt_left">Kirovograd</td>
<td headers="Center  hromada_count" class="gt_row gt_right">5</td>
<td headers="Center  hromada_count_total" class="gt_row gt_right">49</td>
<td headers="Center  deoccupied_count" class="gt_row gt_right">0</td>
<td headers="Center  hromada_survey_pct" class="gt_row gt_right">3.6%</td>
<td headers="Center  hromada_total_pct" class="gt_row gt_right">3.4%</td></tr>
    <tr><td headers="Center  oblast_name_en" class="gt_row gt_left">Cherkassy</td>
<td headers="Center  hromada_count" class="gt_row gt_right">5</td>
<td headers="Center  hromada_count_total" class="gt_row gt_right">66</td>
<td headers="Center  deoccupied_count" class="gt_row gt_right">0</td>
<td headers="Center  hromada_survey_pct" class="gt_row gt_right">3.6%</td>
<td headers="Center  hromada_total_pct" class="gt_row gt_right">4.6%</td></tr>
    <tr><td headers="Center  oblast_name_en" class="gt_row gt_left">Khmelnitsk</td>
<td headers="Center  hromada_count" class="gt_row gt_right">1</td>
<td headers="Center  hromada_count_total" class="gt_row gt_right">60</td>
<td headers="Center  deoccupied_count" class="gt_row gt_right">0</td>
<td headers="Center  hromada_survey_pct" class="gt_row gt_right">0.7%</td>
<td headers="Center  hromada_total_pct" class="gt_row gt_right">4.2%</td></tr>
  </tbody>
  
  
</table>
</div>
```

\
\
<mark>1.4</mark> What type of hromadas are represented in the sample? 


```{.r .fold-hide}
ds0 %>% 
  group_by(type) %>% 
  summarize(
    hromada_count = n_distinct(hromada_code)
    ,.groups = "drop"
  ) %>%
  mutate(hromada_survey_prop = hromada_count / sum(hromada_count)
         ,hromada_survey_pct = scales::percent(hromada_survey_prop, accuracy = .1)) %>% 
  right_join(
    ds_general %>% 
      group_by(type) %>% 
      summarize(hromada_count_total = n())
  ) %>% 
  mutate(
    hromada_count = replace_na(hromada_count, 0)
    ,hromada_total_prop = hromada_count_total / sum(hromada_count_total)
    ,hromada_total_pct = scales::percent(hromada_total_prop, accuracy = .1)
    ,type = case_when(type == 'міська' ~ "urban",
                      type == 'селищна' ~ "urban village",
                      type == 'сільська' ~ "village"
                      )
  ) %>%
  arrange(factor(type, levels = c('urban', "urban village", "village"))) %>% 
  select(-c(hromada_survey_prop, hromada_total_prop)) %>%
  relocate(hromada_count_total, .after = hromada_count) %>%
  ungroup() %>%
  # neat_DT()
  gt::gt() %>%
  gt::cols_label(type = 'Type of Hromada',
                 hromada_count_total = 'Total number \nof ATCs',
                 hromada_count = 'ATCs in the survey',
                 hromada_total_pct = 'Proportion of Hromada Type \nin the General Population',
                 hromada_survey_pct = 'Proportion of Hromada Type \nin the Survey'
  )
```

```{=html}
<div id="uwwmgovift" style="padding-left:0px;padding-right:0px;padding-top:10px;padding-bottom:10px;overflow-x:auto;overflow-y:auto;width:auto;height:auto;">
<style>html {
  font-family: -apple-system, BlinkMacSystemFont, 'Segoe UI', Roboto, Oxygen, Ubuntu, Cantarell, 'Helvetica Neue', 'Fira Sans', 'Droid Sans', Arial, sans-serif;
}

#uwwmgovift .gt_table {
  display: table;
  border-collapse: collapse;
  margin-left: auto;
  margin-right: auto;
  color: #333333;
  font-size: 16px;
  font-weight: normal;
  font-style: normal;
  background-color: #FFFFFF;
  width: auto;
  border-top-style: solid;
  border-top-width: 2px;
  border-top-color: #A8A8A8;
  border-right-style: none;
  border-right-width: 2px;
  border-right-color: #D3D3D3;
  border-bottom-style: solid;
  border-bottom-width: 2px;
  border-bottom-color: #A8A8A8;
  border-left-style: none;
  border-left-width: 2px;
  border-left-color: #D3D3D3;
}

#uwwmgovift .gt_heading {
  background-color: #FFFFFF;
  text-align: center;
  border-bottom-color: #FFFFFF;
  border-left-style: none;
  border-left-width: 1px;
  border-left-color: #D3D3D3;
  border-right-style: none;
  border-right-width: 1px;
  border-right-color: #D3D3D3;
}

#uwwmgovift .gt_caption {
  padding-top: 4px;
  padding-bottom: 4px;
}

#uwwmgovift .gt_title {
  color: #333333;
  font-size: 125%;
  font-weight: initial;
  padding-top: 4px;
  padding-bottom: 4px;
  padding-left: 5px;
  padding-right: 5px;
  border-bottom-color: #FFFFFF;
  border-bottom-width: 0;
}

#uwwmgovift .gt_subtitle {
  color: #333333;
  font-size: 85%;
  font-weight: initial;
  padding-top: 0;
  padding-bottom: 6px;
  padding-left: 5px;
  padding-right: 5px;
  border-top-color: #FFFFFF;
  border-top-width: 0;
}

#uwwmgovift .gt_bottom_border {
  border-bottom-style: solid;
  border-bottom-width: 2px;
  border-bottom-color: #D3D3D3;
}

#uwwmgovift .gt_col_headings {
  border-top-style: solid;
  border-top-width: 2px;
  border-top-color: #D3D3D3;
  border-bottom-style: solid;
  border-bottom-width: 2px;
  border-bottom-color: #D3D3D3;
  border-left-style: none;
  border-left-width: 1px;
  border-left-color: #D3D3D3;
  border-right-style: none;
  border-right-width: 1px;
  border-right-color: #D3D3D3;
}

#uwwmgovift .gt_col_heading {
  color: #333333;
  background-color: #FFFFFF;
  font-size: 100%;
  font-weight: normal;
  text-transform: inherit;
  border-left-style: none;
  border-left-width: 1px;
  border-left-color: #D3D3D3;
  border-right-style: none;
  border-right-width: 1px;
  border-right-color: #D3D3D3;
  vertical-align: bottom;
  padding-top: 5px;
  padding-bottom: 6px;
  padding-left: 5px;
  padding-right: 5px;
  overflow-x: hidden;
}

#uwwmgovift .gt_column_spanner_outer {
  color: #333333;
  background-color: #FFFFFF;
  font-size: 100%;
  font-weight: normal;
  text-transform: inherit;
  padding-top: 0;
  padding-bottom: 0;
  padding-left: 4px;
  padding-right: 4px;
}

#uwwmgovift .gt_column_spanner_outer:first-child {
  padding-left: 0;
}

#uwwmgovift .gt_column_spanner_outer:last-child {
  padding-right: 0;
}

#uwwmgovift .gt_column_spanner {
  border-bottom-style: solid;
  border-bottom-width: 2px;
  border-bottom-color: #D3D3D3;
  vertical-align: bottom;
  padding-top: 5px;
  padding-bottom: 5px;
  overflow-x: hidden;
  display: inline-block;
  width: 100%;
}

#uwwmgovift .gt_group_heading {
  padding-top: 8px;
  padding-bottom: 8px;
  padding-left: 5px;
  padding-right: 5px;
  color: #333333;
  background-color: #FFFFFF;
  font-size: 100%;
  font-weight: initial;
  text-transform: inherit;
  border-top-style: solid;
  border-top-width: 2px;
  border-top-color: #D3D3D3;
  border-bottom-style: solid;
  border-bottom-width: 2px;
  border-bottom-color: #D3D3D3;
  border-left-style: none;
  border-left-width: 1px;
  border-left-color: #D3D3D3;
  border-right-style: none;
  border-right-width: 1px;
  border-right-color: #D3D3D3;
  vertical-align: middle;
  text-align: left;
}

#uwwmgovift .gt_empty_group_heading {
  padding: 0.5px;
  color: #333333;
  background-color: #FFFFFF;
  font-size: 100%;
  font-weight: initial;
  border-top-style: solid;
  border-top-width: 2px;
  border-top-color: #D3D3D3;
  border-bottom-style: solid;
  border-bottom-width: 2px;
  border-bottom-color: #D3D3D3;
  vertical-align: middle;
}

#uwwmgovift .gt_from_md > :first-child {
  margin-top: 0;
}

#uwwmgovift .gt_from_md > :last-child {
  margin-bottom: 0;
}

#uwwmgovift .gt_row {
  padding-top: 8px;
  padding-bottom: 8px;
  padding-left: 5px;
  padding-right: 5px;
  margin: 10px;
  border-top-style: solid;
  border-top-width: 1px;
  border-top-color: #D3D3D3;
  border-left-style: none;
  border-left-width: 1px;
  border-left-color: #D3D3D3;
  border-right-style: none;
  border-right-width: 1px;
  border-right-color: #D3D3D3;
  vertical-align: middle;
  overflow-x: hidden;
}

#uwwmgovift .gt_stub {
  color: #333333;
  background-color: #FFFFFF;
  font-size: 100%;
  font-weight: initial;
  text-transform: inherit;
  border-right-style: solid;
  border-right-width: 2px;
  border-right-color: #D3D3D3;
  padding-left: 5px;
  padding-right: 5px;
}

#uwwmgovift .gt_stub_row_group {
  color: #333333;
  background-color: #FFFFFF;
  font-size: 100%;
  font-weight: initial;
  text-transform: inherit;
  border-right-style: solid;
  border-right-width: 2px;
  border-right-color: #D3D3D3;
  padding-left: 5px;
  padding-right: 5px;
  vertical-align: top;
}

#uwwmgovift .gt_row_group_first td {
  border-top-width: 2px;
}

#uwwmgovift .gt_summary_row {
  color: #333333;
  background-color: #FFFFFF;
  text-transform: inherit;
  padding-top: 8px;
  padding-bottom: 8px;
  padding-left: 5px;
  padding-right: 5px;
}

#uwwmgovift .gt_first_summary_row {
  border-top-style: solid;
  border-top-color: #D3D3D3;
}

#uwwmgovift .gt_first_summary_row.thick {
  border-top-width: 2px;
}

#uwwmgovift .gt_last_summary_row {
  padding-top: 8px;
  padding-bottom: 8px;
  padding-left: 5px;
  padding-right: 5px;
  border-bottom-style: solid;
  border-bottom-width: 2px;
  border-bottom-color: #D3D3D3;
}

#uwwmgovift .gt_grand_summary_row {
  color: #333333;
  background-color: #FFFFFF;
  text-transform: inherit;
  padding-top: 8px;
  padding-bottom: 8px;
  padding-left: 5px;
  padding-right: 5px;
}

#uwwmgovift .gt_first_grand_summary_row {
  padding-top: 8px;
  padding-bottom: 8px;
  padding-left: 5px;
  padding-right: 5px;
  border-top-style: double;
  border-top-width: 6px;
  border-top-color: #D3D3D3;
}

#uwwmgovift .gt_striped {
  background-color: rgba(128, 128, 128, 0.05);
}

#uwwmgovift .gt_table_body {
  border-top-style: solid;
  border-top-width: 2px;
  border-top-color: #D3D3D3;
  border-bottom-style: solid;
  border-bottom-width: 2px;
  border-bottom-color: #D3D3D3;
}

#uwwmgovift .gt_footnotes {
  color: #333333;
  background-color: #FFFFFF;
  border-bottom-style: none;
  border-bottom-width: 2px;
  border-bottom-color: #D3D3D3;
  border-left-style: none;
  border-left-width: 2px;
  border-left-color: #D3D3D3;
  border-right-style: none;
  border-right-width: 2px;
  border-right-color: #D3D3D3;
}

#uwwmgovift .gt_footnote {
  margin: 0px;
  font-size: 90%;
  padding-left: 4px;
  padding-right: 4px;
  padding-left: 5px;
  padding-right: 5px;
}

#uwwmgovift .gt_sourcenotes {
  color: #333333;
  background-color: #FFFFFF;
  border-bottom-style: none;
  border-bottom-width: 2px;
  border-bottom-color: #D3D3D3;
  border-left-style: none;
  border-left-width: 2px;
  border-left-color: #D3D3D3;
  border-right-style: none;
  border-right-width: 2px;
  border-right-color: #D3D3D3;
}

#uwwmgovift .gt_sourcenote {
  font-size: 90%;
  padding-top: 4px;
  padding-bottom: 4px;
  padding-left: 5px;
  padding-right: 5px;
}

#uwwmgovift .gt_left {
  text-align: left;
}

#uwwmgovift .gt_center {
  text-align: center;
}

#uwwmgovift .gt_right {
  text-align: right;
  font-variant-numeric: tabular-nums;
}

#uwwmgovift .gt_font_normal {
  font-weight: normal;
}

#uwwmgovift .gt_font_bold {
  font-weight: bold;
}

#uwwmgovift .gt_font_italic {
  font-style: italic;
}

#uwwmgovift .gt_super {
  font-size: 65%;
}

#uwwmgovift .gt_footnote_marks {
  font-style: italic;
  font-weight: normal;
  font-size: 75%;
  vertical-align: 0.4em;
}

#uwwmgovift .gt_asterisk {
  font-size: 100%;
  vertical-align: 0;
}

#uwwmgovift .gt_indent_1 {
  text-indent: 5px;
}

#uwwmgovift .gt_indent_2 {
  text-indent: 10px;
}

#uwwmgovift .gt_indent_3 {
  text-indent: 15px;
}

#uwwmgovift .gt_indent_4 {
  text-indent: 20px;
}

#uwwmgovift .gt_indent_5 {
  text-indent: 25px;
}
</style>
<table class="gt_table">
  
  <thead class="gt_col_headings">
    <tr>
      <th class="gt_col_heading gt_columns_bottom_border gt_left" rowspan="1" colspan="1" scope="col" id="Type of Hromada">Type of Hromada</th>
      <th class="gt_col_heading gt_columns_bottom_border gt_right" rowspan="1" colspan="1" scope="col" id="ATCs in the survey">ATCs in the survey</th>
      <th class="gt_col_heading gt_columns_bottom_border gt_right" rowspan="1" colspan="1" scope="col" id="Total number &#10;of ATCs">Total number 
of ATCs</th>
      <th class="gt_col_heading gt_columns_bottom_border gt_left" rowspan="1" colspan="1" scope="col" id="Proportion of Hromada Type &#10;in the Survey">Proportion of Hromada Type 
in the Survey</th>
      <th class="gt_col_heading gt_columns_bottom_border gt_right" rowspan="1" colspan="1" scope="col" id="Proportion of Hromada Type &#10;in the General Population">Proportion of Hromada Type 
in the General Population</th>
    </tr>
  </thead>
  <tbody class="gt_table_body">
    <tr><td headers="type" class="gt_row gt_left">urban</td>
<td headers="hromada_count" class="gt_row gt_right">0</td>
<td headers="hromada_count_total" class="gt_row gt_right">409</td>
<td headers="hromada_survey_pct" class="gt_row gt_left">NA</td>
<td headers="hromada_total_pct" class="gt_row gt_right">27.8%</td></tr>
    <tr><td headers="type" class="gt_row gt_left">urban village</td>
<td headers="hromada_count" class="gt_row gt_right">0</td>
<td headers="hromada_count_total" class="gt_row gt_right">435</td>
<td headers="hromada_survey_pct" class="gt_row gt_left">NA</td>
<td headers="hromada_total_pct" class="gt_row gt_right">29.6%</td></tr>
    <tr><td headers="type" class="gt_row gt_left">village</td>
<td headers="hromada_count" class="gt_row gt_right">0</td>
<td headers="hromada_count_total" class="gt_row gt_right">625</td>
<td headers="hromada_survey_pct" class="gt_row gt_left">NA</td>
<td headers="hromada_total_pct" class="gt_row gt_right">42.5%</td></tr>
  </tbody>
  
  
</table>
</div>
```

\
\
<mark>1.5</mark> What hromadas experienced military occupation or  military actions? 


```{.r .fold-hide}
(ds0 %>% make_bi_freq_graph("military_action")) +
  labs(
    title = "How many respondent hromadas have experienced military action at the time of the interview?"
    ,subtitle = "Data were collected during October-November of 2022"
    ,x = NULL
    ,y = NULL
    ,fill = NULL
  ) +  
  scale_x_discrete(labels=c('no_combat' = 'No military actions', 'combat_now' = 'Experiencing military actions now', "combat_ended" = "Experienced military actions in the past")) +
  guides(fill = "none") +
  theme_bw()
```

![](figure-png-iso/unnamed-chunk-5-1.png)<!-- -->

```{.r .fold-hide}
(ds0 %>% make_bi_freq_graph("occupation"))+
  labs(
    title = "How many respondent hromadas have experienced occupation at the time of the interview?"
    ,subtitle = "Data were collected during October-November of 2022"
    ,y=NULL
    ,x=NULL
  ) +  
  scale_x_discrete(labels=c('occupied_now' = 'Occupied now', 'occupied_august' = 'Occupied until August-October', "occupied_april" = "Occupied until April", "not_occupied" = "Wasn't occupied")) +
  guides(fill = "none") +
  theme_bw()
```

![](figure-png-iso/unnamed-chunk-5-2.png)<!-- -->


```{=html}
<div id="hmpiclakfn" style="padding-left:0px;padding-right:0px;padding-top:10px;padding-bottom:10px;overflow-x:auto;overflow-y:auto;width:auto;height:auto;">
<style>html {
  font-family: -apple-system, BlinkMacSystemFont, 'Segoe UI', Roboto, Oxygen, Ubuntu, Cantarell, 'Helvetica Neue', 'Fira Sans', 'Droid Sans', Arial, sans-serif;
}

#hmpiclakfn .gt_table {
  display: table;
  border-collapse: collapse;
  margin-left: auto;
  margin-right: auto;
  color: #333333;
  font-size: 16px;
  font-weight: normal;
  font-style: normal;
  background-color: #FFFFFF;
  width: auto;
  border-top-style: solid;
  border-top-width: 2px;
  border-top-color: #A8A8A8;
  border-right-style: none;
  border-right-width: 2px;
  border-right-color: #D3D3D3;
  border-bottom-style: solid;
  border-bottom-width: 2px;
  border-bottom-color: #A8A8A8;
  border-left-style: none;
  border-left-width: 2px;
  border-left-color: #D3D3D3;
}

#hmpiclakfn .gt_heading {
  background-color: #FFFFFF;
  text-align: center;
  border-bottom-color: #FFFFFF;
  border-left-style: none;
  border-left-width: 1px;
  border-left-color: #D3D3D3;
  border-right-style: none;
  border-right-width: 1px;
  border-right-color: #D3D3D3;
}

#hmpiclakfn .gt_caption {
  padding-top: 4px;
  padding-bottom: 4px;
}

#hmpiclakfn .gt_title {
  color: #333333;
  font-size: 125%;
  font-weight: initial;
  padding-top: 4px;
  padding-bottom: 4px;
  padding-left: 5px;
  padding-right: 5px;
  border-bottom-color: #FFFFFF;
  border-bottom-width: 0;
}

#hmpiclakfn .gt_subtitle {
  color: #333333;
  font-size: 85%;
  font-weight: initial;
  padding-top: 0;
  padding-bottom: 6px;
  padding-left: 5px;
  padding-right: 5px;
  border-top-color: #FFFFFF;
  border-top-width: 0;
}

#hmpiclakfn .gt_bottom_border {
  border-bottom-style: solid;
  border-bottom-width: 2px;
  border-bottom-color: #D3D3D3;
}

#hmpiclakfn .gt_col_headings {
  border-top-style: solid;
  border-top-width: 2px;
  border-top-color: #D3D3D3;
  border-bottom-style: solid;
  border-bottom-width: 2px;
  border-bottom-color: #D3D3D3;
  border-left-style: none;
  border-left-width: 1px;
  border-left-color: #D3D3D3;
  border-right-style: none;
  border-right-width: 1px;
  border-right-color: #D3D3D3;
}

#hmpiclakfn .gt_col_heading {
  color: #333333;
  background-color: #FFFFFF;
  font-size: 100%;
  font-weight: normal;
  text-transform: inherit;
  border-left-style: none;
  border-left-width: 1px;
  border-left-color: #D3D3D3;
  border-right-style: none;
  border-right-width: 1px;
  border-right-color: #D3D3D3;
  vertical-align: bottom;
  padding-top: 5px;
  padding-bottom: 6px;
  padding-left: 5px;
  padding-right: 5px;
  overflow-x: hidden;
}

#hmpiclakfn .gt_column_spanner_outer {
  color: #333333;
  background-color: #FFFFFF;
  font-size: 100%;
  font-weight: normal;
  text-transform: inherit;
  padding-top: 0;
  padding-bottom: 0;
  padding-left: 4px;
  padding-right: 4px;
}

#hmpiclakfn .gt_column_spanner_outer:first-child {
  padding-left: 0;
}

#hmpiclakfn .gt_column_spanner_outer:last-child {
  padding-right: 0;
}

#hmpiclakfn .gt_column_spanner {
  border-bottom-style: solid;
  border-bottom-width: 2px;
  border-bottom-color: #D3D3D3;
  vertical-align: bottom;
  padding-top: 5px;
  padding-bottom: 5px;
  overflow-x: hidden;
  display: inline-block;
  width: 100%;
}

#hmpiclakfn .gt_group_heading {
  padding-top: 8px;
  padding-bottom: 8px;
  padding-left: 5px;
  padding-right: 5px;
  color: #333333;
  background-color: #FFFFFF;
  font-size: 100%;
  font-weight: initial;
  text-transform: inherit;
  border-top-style: solid;
  border-top-width: 2px;
  border-top-color: #D3D3D3;
  border-bottom-style: solid;
  border-bottom-width: 2px;
  border-bottom-color: #D3D3D3;
  border-left-style: none;
  border-left-width: 1px;
  border-left-color: #D3D3D3;
  border-right-style: none;
  border-right-width: 1px;
  border-right-color: #D3D3D3;
  vertical-align: middle;
  text-align: left;
}

#hmpiclakfn .gt_empty_group_heading {
  padding: 0.5px;
  color: #333333;
  background-color: #FFFFFF;
  font-size: 100%;
  font-weight: initial;
  border-top-style: solid;
  border-top-width: 2px;
  border-top-color: #D3D3D3;
  border-bottom-style: solid;
  border-bottom-width: 2px;
  border-bottom-color: #D3D3D3;
  vertical-align: middle;
}

#hmpiclakfn .gt_from_md > :first-child {
  margin-top: 0;
}

#hmpiclakfn .gt_from_md > :last-child {
  margin-bottom: 0;
}

#hmpiclakfn .gt_row {
  padding-top: 8px;
  padding-bottom: 8px;
  padding-left: 5px;
  padding-right: 5px;
  margin: 10px;
  border-top-style: solid;
  border-top-width: 1px;
  border-top-color: #D3D3D3;
  border-left-style: none;
  border-left-width: 1px;
  border-left-color: #D3D3D3;
  border-right-style: none;
  border-right-width: 1px;
  border-right-color: #D3D3D3;
  vertical-align: middle;
  overflow-x: hidden;
}

#hmpiclakfn .gt_stub {
  color: #333333;
  background-color: #FFFFFF;
  font-size: 100%;
  font-weight: initial;
  text-transform: inherit;
  border-right-style: solid;
  border-right-width: 2px;
  border-right-color: #D3D3D3;
  padding-left: 5px;
  padding-right: 5px;
}

#hmpiclakfn .gt_stub_row_group {
  color: #333333;
  background-color: #FFFFFF;
  font-size: 100%;
  font-weight: initial;
  text-transform: inherit;
  border-right-style: solid;
  border-right-width: 2px;
  border-right-color: #D3D3D3;
  padding-left: 5px;
  padding-right: 5px;
  vertical-align: top;
}

#hmpiclakfn .gt_row_group_first td {
  border-top-width: 2px;
}

#hmpiclakfn .gt_summary_row {
  color: #333333;
  background-color: #FFFFFF;
  text-transform: inherit;
  padding-top: 8px;
  padding-bottom: 8px;
  padding-left: 5px;
  padding-right: 5px;
}

#hmpiclakfn .gt_first_summary_row {
  border-top-style: solid;
  border-top-color: #D3D3D3;
}

#hmpiclakfn .gt_first_summary_row.thick {
  border-top-width: 2px;
}

#hmpiclakfn .gt_last_summary_row {
  padding-top: 8px;
  padding-bottom: 8px;
  padding-left: 5px;
  padding-right: 5px;
  border-bottom-style: solid;
  border-bottom-width: 2px;
  border-bottom-color: #D3D3D3;
}

#hmpiclakfn .gt_grand_summary_row {
  color: #333333;
  background-color: #FFFFFF;
  text-transform: inherit;
  padding-top: 8px;
  padding-bottom: 8px;
  padding-left: 5px;
  padding-right: 5px;
}

#hmpiclakfn .gt_first_grand_summary_row {
  padding-top: 8px;
  padding-bottom: 8px;
  padding-left: 5px;
  padding-right: 5px;
  border-top-style: double;
  border-top-width: 6px;
  border-top-color: #D3D3D3;
}

#hmpiclakfn .gt_striped {
  background-color: rgba(128, 128, 128, 0.05);
}

#hmpiclakfn .gt_table_body {
  border-top-style: solid;
  border-top-width: 2px;
  border-top-color: #D3D3D3;
  border-bottom-style: solid;
  border-bottom-width: 2px;
  border-bottom-color: #D3D3D3;
}

#hmpiclakfn .gt_footnotes {
  color: #333333;
  background-color: #FFFFFF;
  border-bottom-style: none;
  border-bottom-width: 2px;
  border-bottom-color: #D3D3D3;
  border-left-style: none;
  border-left-width: 2px;
  border-left-color: #D3D3D3;
  border-right-style: none;
  border-right-width: 2px;
  border-right-color: #D3D3D3;
}

#hmpiclakfn .gt_footnote {
  margin: 0px;
  font-size: 90%;
  padding-left: 4px;
  padding-right: 4px;
  padding-left: 5px;
  padding-right: 5px;
}

#hmpiclakfn .gt_sourcenotes {
  color: #333333;
  background-color: #FFFFFF;
  border-bottom-style: none;
  border-bottom-width: 2px;
  border-bottom-color: #D3D3D3;
  border-left-style: none;
  border-left-width: 2px;
  border-left-color: #D3D3D3;
  border-right-style: none;
  border-right-width: 2px;
  border-right-color: #D3D3D3;
}

#hmpiclakfn .gt_sourcenote {
  font-size: 90%;
  padding-top: 4px;
  padding-bottom: 4px;
  padding-left: 5px;
  padding-right: 5px;
}

#hmpiclakfn .gt_left {
  text-align: left;
}

#hmpiclakfn .gt_center {
  text-align: center;
}

#hmpiclakfn .gt_right {
  text-align: right;
  font-variant-numeric: tabular-nums;
}

#hmpiclakfn .gt_font_normal {
  font-weight: normal;
}

#hmpiclakfn .gt_font_bold {
  font-weight: bold;
}

#hmpiclakfn .gt_font_italic {
  font-style: italic;
}

#hmpiclakfn .gt_super {
  font-size: 65%;
}

#hmpiclakfn .gt_footnote_marks {
  font-style: italic;
  font-weight: normal;
  font-size: 75%;
  vertical-align: 0.4em;
}

#hmpiclakfn .gt_asterisk {
  font-size: 100%;
  vertical-align: 0;
}

#hmpiclakfn .gt_indent_1 {
  text-indent: 5px;
}

#hmpiclakfn .gt_indent_2 {
  text-indent: 10px;
}

#hmpiclakfn .gt_indent_3 {
  text-indent: 15px;
}

#hmpiclakfn .gt_indent_4 {
  text-indent: 20px;
}

#hmpiclakfn .gt_indent_5 {
  text-indent: 25px;
}
</style>
<table class="gt_table">
  <thead class="gt_header">
    <tr>
      <td colspan="5" class="gt_heading gt_title gt_font_normal gt_bottom_border" style>Crosstable of Hromadas that experienced occupation and/or military actions</td>
    </tr>
    
  </thead>
  <thead class="gt_col_headings">
    <tr>
      <th class="gt_col_heading gt_columns_bottom_border gt_left" rowspan="1" colspan="1" scope="col" id=""></th>
      <th class="gt_col_heading gt_columns_bottom_border gt_right" rowspan="1" colspan="1" scope="col" id="Wasn't occupied">Wasn't occupied</th>
      <th class="gt_col_heading gt_columns_bottom_border gt_right" rowspan="1" colspan="1" scope="col" id="Occupied until April">Occupied until April</th>
      <th class="gt_col_heading gt_columns_bottom_border gt_right" rowspan="1" colspan="1" scope="col" id="Occupied until August-October">Occupied until August-October</th>
      <th class="gt_col_heading gt_columns_bottom_border gt_right" rowspan="1" colspan="1" scope="col" id="Occupied now">Occupied now</th>
    </tr>
  </thead>
  <tbody class="gt_table_body">
    <tr><td headers="Var2" class="gt_row gt_left">Experienced military actions in the past</td>
<td headers="not_occupied" class="gt_row gt_right">1</td>
<td headers="occupied_april" class="gt_row gt_right">6</td>
<td headers="occupied_august" class="gt_row gt_right">0</td>
<td headers="occupied_now" class="gt_row gt_right">0</td></tr>
    <tr><td headers="Var2" class="gt_row gt_left">Experiencing military actions now</td>
<td headers="not_occupied" class="gt_row gt_right">1</td>
<td headers="occupied_april" class="gt_row gt_right">3</td>
<td headers="occupied_august" class="gt_row gt_right">1</td>
<td headers="occupied_now" class="gt_row gt_right">4</td></tr>
    <tr><td headers="Var2" class="gt_row gt_left">No military actions</td>
<td headers="not_occupied" class="gt_row gt_right">116</td>
<td headers="occupied_april" class="gt_row gt_right">3</td>
<td headers="occupied_august" class="gt_row gt_right">0</td>
<td headers="occupied_now" class="gt_row gt_right">2</td></tr>
  </tbody>
  
  
</table>
</div>
```

\
\
<mark>1.6</mark> How many partherships with other hromadas in Ukraine hromadas have?



> Median number of partnerships that hromada has is 0, mean - 1.34. 
Maximum number of partnerships that hromada has in our survey is 20.

![](figure-png-iso/partnerships-hist-1.png)<!-- -->

> These are hromadas in our survey that have the most partnerships:


```{=html}
<div id="knvbojobie" style="padding-left:0px;padding-right:0px;padding-top:10px;padding-bottom:10px;overflow-x:auto;overflow-y:auto;width:auto;height:auto;">
<style>html {
  font-family: -apple-system, BlinkMacSystemFont, 'Segoe UI', Roboto, Oxygen, Ubuntu, Cantarell, 'Helvetica Neue', 'Fira Sans', 'Droid Sans', Arial, sans-serif;
}

#knvbojobie .gt_table {
  display: table;
  border-collapse: collapse;
  margin-left: auto;
  margin-right: auto;
  color: #333333;
  font-size: 16px;
  font-weight: normal;
  font-style: normal;
  background-color: #FFFFFF;
  width: auto;
  border-top-style: solid;
  border-top-width: 2px;
  border-top-color: #A8A8A8;
  border-right-style: none;
  border-right-width: 2px;
  border-right-color: #D3D3D3;
  border-bottom-style: solid;
  border-bottom-width: 2px;
  border-bottom-color: #A8A8A8;
  border-left-style: none;
  border-left-width: 2px;
  border-left-color: #D3D3D3;
}

#knvbojobie .gt_heading {
  background-color: #FFFFFF;
  text-align: center;
  border-bottom-color: #FFFFFF;
  border-left-style: none;
  border-left-width: 1px;
  border-left-color: #D3D3D3;
  border-right-style: none;
  border-right-width: 1px;
  border-right-color: #D3D3D3;
}

#knvbojobie .gt_caption {
  padding-top: 4px;
  padding-bottom: 4px;
}

#knvbojobie .gt_title {
  color: #333333;
  font-size: 125%;
  font-weight: initial;
  padding-top: 4px;
  padding-bottom: 4px;
  padding-left: 5px;
  padding-right: 5px;
  border-bottom-color: #FFFFFF;
  border-bottom-width: 0;
}

#knvbojobie .gt_subtitle {
  color: #333333;
  font-size: 85%;
  font-weight: initial;
  padding-top: 0;
  padding-bottom: 6px;
  padding-left: 5px;
  padding-right: 5px;
  border-top-color: #FFFFFF;
  border-top-width: 0;
}

#knvbojobie .gt_bottom_border {
  border-bottom-style: solid;
  border-bottom-width: 2px;
  border-bottom-color: #D3D3D3;
}

#knvbojobie .gt_col_headings {
  border-top-style: solid;
  border-top-width: 2px;
  border-top-color: #D3D3D3;
  border-bottom-style: solid;
  border-bottom-width: 2px;
  border-bottom-color: #D3D3D3;
  border-left-style: none;
  border-left-width: 1px;
  border-left-color: #D3D3D3;
  border-right-style: none;
  border-right-width: 1px;
  border-right-color: #D3D3D3;
}

#knvbojobie .gt_col_heading {
  color: #333333;
  background-color: #FFFFFF;
  font-size: 100%;
  font-weight: normal;
  text-transform: inherit;
  border-left-style: none;
  border-left-width: 1px;
  border-left-color: #D3D3D3;
  border-right-style: none;
  border-right-width: 1px;
  border-right-color: #D3D3D3;
  vertical-align: bottom;
  padding-top: 5px;
  padding-bottom: 6px;
  padding-left: 5px;
  padding-right: 5px;
  overflow-x: hidden;
}

#knvbojobie .gt_column_spanner_outer {
  color: #333333;
  background-color: #FFFFFF;
  font-size: 100%;
  font-weight: normal;
  text-transform: inherit;
  padding-top: 0;
  padding-bottom: 0;
  padding-left: 4px;
  padding-right: 4px;
}

#knvbojobie .gt_column_spanner_outer:first-child {
  padding-left: 0;
}

#knvbojobie .gt_column_spanner_outer:last-child {
  padding-right: 0;
}

#knvbojobie .gt_column_spanner {
  border-bottom-style: solid;
  border-bottom-width: 2px;
  border-bottom-color: #D3D3D3;
  vertical-align: bottom;
  padding-top: 5px;
  padding-bottom: 5px;
  overflow-x: hidden;
  display: inline-block;
  width: 100%;
}

#knvbojobie .gt_group_heading {
  padding-top: 8px;
  padding-bottom: 8px;
  padding-left: 5px;
  padding-right: 5px;
  color: #333333;
  background-color: #FFFFFF;
  font-size: 100%;
  font-weight: initial;
  text-transform: inherit;
  border-top-style: solid;
  border-top-width: 2px;
  border-top-color: #D3D3D3;
  border-bottom-style: solid;
  border-bottom-width: 2px;
  border-bottom-color: #D3D3D3;
  border-left-style: none;
  border-left-width: 1px;
  border-left-color: #D3D3D3;
  border-right-style: none;
  border-right-width: 1px;
  border-right-color: #D3D3D3;
  vertical-align: middle;
  text-align: left;
}

#knvbojobie .gt_empty_group_heading {
  padding: 0.5px;
  color: #333333;
  background-color: #FFFFFF;
  font-size: 100%;
  font-weight: initial;
  border-top-style: solid;
  border-top-width: 2px;
  border-top-color: #D3D3D3;
  border-bottom-style: solid;
  border-bottom-width: 2px;
  border-bottom-color: #D3D3D3;
  vertical-align: middle;
}

#knvbojobie .gt_from_md > :first-child {
  margin-top: 0;
}

#knvbojobie .gt_from_md > :last-child {
  margin-bottom: 0;
}

#knvbojobie .gt_row {
  padding-top: 8px;
  padding-bottom: 8px;
  padding-left: 5px;
  padding-right: 5px;
  margin: 10px;
  border-top-style: solid;
  border-top-width: 1px;
  border-top-color: #D3D3D3;
  border-left-style: none;
  border-left-width: 1px;
  border-left-color: #D3D3D3;
  border-right-style: none;
  border-right-width: 1px;
  border-right-color: #D3D3D3;
  vertical-align: middle;
  overflow-x: hidden;
}

#knvbojobie .gt_stub {
  color: #333333;
  background-color: #FFFFFF;
  font-size: 100%;
  font-weight: initial;
  text-transform: inherit;
  border-right-style: solid;
  border-right-width: 2px;
  border-right-color: #D3D3D3;
  padding-left: 5px;
  padding-right: 5px;
}

#knvbojobie .gt_stub_row_group {
  color: #333333;
  background-color: #FFFFFF;
  font-size: 100%;
  font-weight: initial;
  text-transform: inherit;
  border-right-style: solid;
  border-right-width: 2px;
  border-right-color: #D3D3D3;
  padding-left: 5px;
  padding-right: 5px;
  vertical-align: top;
}

#knvbojobie .gt_row_group_first td {
  border-top-width: 2px;
}

#knvbojobie .gt_summary_row {
  color: #333333;
  background-color: #FFFFFF;
  text-transform: inherit;
  padding-top: 8px;
  padding-bottom: 8px;
  padding-left: 5px;
  padding-right: 5px;
}

#knvbojobie .gt_first_summary_row {
  border-top-style: solid;
  border-top-color: #D3D3D3;
}

#knvbojobie .gt_first_summary_row.thick {
  border-top-width: 2px;
}

#knvbojobie .gt_last_summary_row {
  padding-top: 8px;
  padding-bottom: 8px;
  padding-left: 5px;
  padding-right: 5px;
  border-bottom-style: solid;
  border-bottom-width: 2px;
  border-bottom-color: #D3D3D3;
}

#knvbojobie .gt_grand_summary_row {
  color: #333333;
  background-color: #FFFFFF;
  text-transform: inherit;
  padding-top: 8px;
  padding-bottom: 8px;
  padding-left: 5px;
  padding-right: 5px;
}

#knvbojobie .gt_first_grand_summary_row {
  padding-top: 8px;
  padding-bottom: 8px;
  padding-left: 5px;
  padding-right: 5px;
  border-top-style: double;
  border-top-width: 6px;
  border-top-color: #D3D3D3;
}

#knvbojobie .gt_striped {
  background-color: rgba(128, 128, 128, 0.05);
}

#knvbojobie .gt_table_body {
  border-top-style: solid;
  border-top-width: 2px;
  border-top-color: #D3D3D3;
  border-bottom-style: solid;
  border-bottom-width: 2px;
  border-bottom-color: #D3D3D3;
}

#knvbojobie .gt_footnotes {
  color: #333333;
  background-color: #FFFFFF;
  border-bottom-style: none;
  border-bottom-width: 2px;
  border-bottom-color: #D3D3D3;
  border-left-style: none;
  border-left-width: 2px;
  border-left-color: #D3D3D3;
  border-right-style: none;
  border-right-width: 2px;
  border-right-color: #D3D3D3;
}

#knvbojobie .gt_footnote {
  margin: 0px;
  font-size: 90%;
  padding-left: 4px;
  padding-right: 4px;
  padding-left: 5px;
  padding-right: 5px;
}

#knvbojobie .gt_sourcenotes {
  color: #333333;
  background-color: #FFFFFF;
  border-bottom-style: none;
  border-bottom-width: 2px;
  border-bottom-color: #D3D3D3;
  border-left-style: none;
  border-left-width: 2px;
  border-left-color: #D3D3D3;
  border-right-style: none;
  border-right-width: 2px;
  border-right-color: #D3D3D3;
}

#knvbojobie .gt_sourcenote {
  font-size: 90%;
  padding-top: 4px;
  padding-bottom: 4px;
  padding-left: 5px;
  padding-right: 5px;
}

#knvbojobie .gt_left {
  text-align: left;
}

#knvbojobie .gt_center {
  text-align: center;
}

#knvbojobie .gt_right {
  text-align: right;
  font-variant-numeric: tabular-nums;
}

#knvbojobie .gt_font_normal {
  font-weight: normal;
}

#knvbojobie .gt_font_bold {
  font-weight: bold;
}

#knvbojobie .gt_font_italic {
  font-style: italic;
}

#knvbojobie .gt_super {
  font-size: 65%;
}

#knvbojobie .gt_footnote_marks {
  font-style: italic;
  font-weight: normal;
  font-size: 75%;
  vertical-align: 0.4em;
}

#knvbojobie .gt_asterisk {
  font-size: 100%;
  vertical-align: 0;
}

#knvbojobie .gt_indent_1 {
  text-indent: 5px;
}

#knvbojobie .gt_indent_2 {
  text-indent: 10px;
}

#knvbojobie .gt_indent_3 {
  text-indent: 15px;
}

#knvbojobie .gt_indent_4 {
  text-indent: 20px;
}

#knvbojobie .gt_indent_5 {
  text-indent: 25px;
}
</style>
<table class="gt_table">
  
  <thead class="gt_col_headings">
    <tr>
      <th class="gt_col_heading gt_columns_bottom_border gt_left" rowspan="1" colspan="1" scope="col" id="Hromada Name">Hromada Name</th>
      <th class="gt_col_heading gt_columns_bottom_border gt_right" rowspan="1" colspan="1" scope="col" id="Number of hromada's partnerships in Ukraine">Number of hromada's partnerships in Ukraine</th>
    </tr>
  </thead>
  <tbody class="gt_table_body">
    <tr><td headers="Hromada Name" class="gt_row gt_left">Ясінянська селищна громада</td>
<td headers="Number of hromada's partnerships in Ukraine" class="gt_row gt_right">20</td></tr>
    <tr><td headers="Hromada Name" class="gt_row gt_left">Кутська селищна громада</td>
<td headers="Number of hromada's partnerships in Ukraine" class="gt_row gt_right">20</td></tr>
    <tr><td headers="Hromada Name" class="gt_row gt_left">Коломийська міська громада</td>
<td headers="Number of hromada's partnerships in Ukraine" class="gt_row gt_right">10</td></tr>
    <tr><td headers="Hromada Name" class="gt_row gt_left">Бориславська міська громада</td>
<td headers="Number of hromada's partnerships in Ukraine" class="gt_row gt_right">7</td></tr>
    <tr><td headers="Hromada Name" class="gt_row gt_left">Брошнів-Осадська селищна громада</td>
<td headers="Number of hromada's partnerships in Ukraine" class="gt_row gt_right">6</td></tr>
  </tbody>
  
  
</table>
</div>
```

<mark>1.6</mark> How many hromadas-friends abroad hromadas have?


```{.r .fold-hide}
ds0 %>% 
  mutate(friends_text = as.numeric(friends_text)) %>% 
  select(hromada_full_name, friends_text) %>%
  mutate(friends_group = cut(friends_text,
                              breaks = c(0, 1, 2, 3, 4, 5, 20), 
                              labels = c('None', '1', '2', '3', '4', 'More than 4'),
                              right = F)) %>%
  na.omit() %>%
  ggplot(aes(x=friends_group, group = 1)) +
  geom_bar(aes(y = ..prop..), stat = 'count') +
  geom_text(aes(label = scales::percent(..prop..), y = ..prop..), stat = 'count', vjust = -.4) +
  scale_y_continuous(labels = scales::percent_format()) +
  labs(
    title = "How many hromadas-friends abroad hromadas have?"
    ,y = NULL, x = NULL
  )
```

![](figure-png-iso/friends-hist-1.png)<!-- -->

> These are hromadas in our survey that have the most friends abroad:


```{=html}
<div id="jizhuyyyqs" style="padding-left:0px;padding-right:0px;padding-top:10px;padding-bottom:10px;overflow-x:auto;overflow-y:auto;width:auto;height:auto;">
<style>html {
  font-family: -apple-system, BlinkMacSystemFont, 'Segoe UI', Roboto, Oxygen, Ubuntu, Cantarell, 'Helvetica Neue', 'Fira Sans', 'Droid Sans', Arial, sans-serif;
}

#jizhuyyyqs .gt_table {
  display: table;
  border-collapse: collapse;
  margin-left: auto;
  margin-right: auto;
  color: #333333;
  font-size: 16px;
  font-weight: normal;
  font-style: normal;
  background-color: #FFFFFF;
  width: auto;
  border-top-style: solid;
  border-top-width: 2px;
  border-top-color: #A8A8A8;
  border-right-style: none;
  border-right-width: 2px;
  border-right-color: #D3D3D3;
  border-bottom-style: solid;
  border-bottom-width: 2px;
  border-bottom-color: #A8A8A8;
  border-left-style: none;
  border-left-width: 2px;
  border-left-color: #D3D3D3;
}

#jizhuyyyqs .gt_heading {
  background-color: #FFFFFF;
  text-align: center;
  border-bottom-color: #FFFFFF;
  border-left-style: none;
  border-left-width: 1px;
  border-left-color: #D3D3D3;
  border-right-style: none;
  border-right-width: 1px;
  border-right-color: #D3D3D3;
}

#jizhuyyyqs .gt_caption {
  padding-top: 4px;
  padding-bottom: 4px;
}

#jizhuyyyqs .gt_title {
  color: #333333;
  font-size: 125%;
  font-weight: initial;
  padding-top: 4px;
  padding-bottom: 4px;
  padding-left: 5px;
  padding-right: 5px;
  border-bottom-color: #FFFFFF;
  border-bottom-width: 0;
}

#jizhuyyyqs .gt_subtitle {
  color: #333333;
  font-size: 85%;
  font-weight: initial;
  padding-top: 0;
  padding-bottom: 6px;
  padding-left: 5px;
  padding-right: 5px;
  border-top-color: #FFFFFF;
  border-top-width: 0;
}

#jizhuyyyqs .gt_bottom_border {
  border-bottom-style: solid;
  border-bottom-width: 2px;
  border-bottom-color: #D3D3D3;
}

#jizhuyyyqs .gt_col_headings {
  border-top-style: solid;
  border-top-width: 2px;
  border-top-color: #D3D3D3;
  border-bottom-style: solid;
  border-bottom-width: 2px;
  border-bottom-color: #D3D3D3;
  border-left-style: none;
  border-left-width: 1px;
  border-left-color: #D3D3D3;
  border-right-style: none;
  border-right-width: 1px;
  border-right-color: #D3D3D3;
}

#jizhuyyyqs .gt_col_heading {
  color: #333333;
  background-color: #FFFFFF;
  font-size: 100%;
  font-weight: normal;
  text-transform: inherit;
  border-left-style: none;
  border-left-width: 1px;
  border-left-color: #D3D3D3;
  border-right-style: none;
  border-right-width: 1px;
  border-right-color: #D3D3D3;
  vertical-align: bottom;
  padding-top: 5px;
  padding-bottom: 6px;
  padding-left: 5px;
  padding-right: 5px;
  overflow-x: hidden;
}

#jizhuyyyqs .gt_column_spanner_outer {
  color: #333333;
  background-color: #FFFFFF;
  font-size: 100%;
  font-weight: normal;
  text-transform: inherit;
  padding-top: 0;
  padding-bottom: 0;
  padding-left: 4px;
  padding-right: 4px;
}

#jizhuyyyqs .gt_column_spanner_outer:first-child {
  padding-left: 0;
}

#jizhuyyyqs .gt_column_spanner_outer:last-child {
  padding-right: 0;
}

#jizhuyyyqs .gt_column_spanner {
  border-bottom-style: solid;
  border-bottom-width: 2px;
  border-bottom-color: #D3D3D3;
  vertical-align: bottom;
  padding-top: 5px;
  padding-bottom: 5px;
  overflow-x: hidden;
  display: inline-block;
  width: 100%;
}

#jizhuyyyqs .gt_group_heading {
  padding-top: 8px;
  padding-bottom: 8px;
  padding-left: 5px;
  padding-right: 5px;
  color: #333333;
  background-color: #FFFFFF;
  font-size: 100%;
  font-weight: initial;
  text-transform: inherit;
  border-top-style: solid;
  border-top-width: 2px;
  border-top-color: #D3D3D3;
  border-bottom-style: solid;
  border-bottom-width: 2px;
  border-bottom-color: #D3D3D3;
  border-left-style: none;
  border-left-width: 1px;
  border-left-color: #D3D3D3;
  border-right-style: none;
  border-right-width: 1px;
  border-right-color: #D3D3D3;
  vertical-align: middle;
  text-align: left;
}

#jizhuyyyqs .gt_empty_group_heading {
  padding: 0.5px;
  color: #333333;
  background-color: #FFFFFF;
  font-size: 100%;
  font-weight: initial;
  border-top-style: solid;
  border-top-width: 2px;
  border-top-color: #D3D3D3;
  border-bottom-style: solid;
  border-bottom-width: 2px;
  border-bottom-color: #D3D3D3;
  vertical-align: middle;
}

#jizhuyyyqs .gt_from_md > :first-child {
  margin-top: 0;
}

#jizhuyyyqs .gt_from_md > :last-child {
  margin-bottom: 0;
}

#jizhuyyyqs .gt_row {
  padding-top: 8px;
  padding-bottom: 8px;
  padding-left: 5px;
  padding-right: 5px;
  margin: 10px;
  border-top-style: solid;
  border-top-width: 1px;
  border-top-color: #D3D3D3;
  border-left-style: none;
  border-left-width: 1px;
  border-left-color: #D3D3D3;
  border-right-style: none;
  border-right-width: 1px;
  border-right-color: #D3D3D3;
  vertical-align: middle;
  overflow-x: hidden;
}

#jizhuyyyqs .gt_stub {
  color: #333333;
  background-color: #FFFFFF;
  font-size: 100%;
  font-weight: initial;
  text-transform: inherit;
  border-right-style: solid;
  border-right-width: 2px;
  border-right-color: #D3D3D3;
  padding-left: 5px;
  padding-right: 5px;
}

#jizhuyyyqs .gt_stub_row_group {
  color: #333333;
  background-color: #FFFFFF;
  font-size: 100%;
  font-weight: initial;
  text-transform: inherit;
  border-right-style: solid;
  border-right-width: 2px;
  border-right-color: #D3D3D3;
  padding-left: 5px;
  padding-right: 5px;
  vertical-align: top;
}

#jizhuyyyqs .gt_row_group_first td {
  border-top-width: 2px;
}

#jizhuyyyqs .gt_summary_row {
  color: #333333;
  background-color: #FFFFFF;
  text-transform: inherit;
  padding-top: 8px;
  padding-bottom: 8px;
  padding-left: 5px;
  padding-right: 5px;
}

#jizhuyyyqs .gt_first_summary_row {
  border-top-style: solid;
  border-top-color: #D3D3D3;
}

#jizhuyyyqs .gt_first_summary_row.thick {
  border-top-width: 2px;
}

#jizhuyyyqs .gt_last_summary_row {
  padding-top: 8px;
  padding-bottom: 8px;
  padding-left: 5px;
  padding-right: 5px;
  border-bottom-style: solid;
  border-bottom-width: 2px;
  border-bottom-color: #D3D3D3;
}

#jizhuyyyqs .gt_grand_summary_row {
  color: #333333;
  background-color: #FFFFFF;
  text-transform: inherit;
  padding-top: 8px;
  padding-bottom: 8px;
  padding-left: 5px;
  padding-right: 5px;
}

#jizhuyyyqs .gt_first_grand_summary_row {
  padding-top: 8px;
  padding-bottom: 8px;
  padding-left: 5px;
  padding-right: 5px;
  border-top-style: double;
  border-top-width: 6px;
  border-top-color: #D3D3D3;
}

#jizhuyyyqs .gt_striped {
  background-color: rgba(128, 128, 128, 0.05);
}

#jizhuyyyqs .gt_table_body {
  border-top-style: solid;
  border-top-width: 2px;
  border-top-color: #D3D3D3;
  border-bottom-style: solid;
  border-bottom-width: 2px;
  border-bottom-color: #D3D3D3;
}

#jizhuyyyqs .gt_footnotes {
  color: #333333;
  background-color: #FFFFFF;
  border-bottom-style: none;
  border-bottom-width: 2px;
  border-bottom-color: #D3D3D3;
  border-left-style: none;
  border-left-width: 2px;
  border-left-color: #D3D3D3;
  border-right-style: none;
  border-right-width: 2px;
  border-right-color: #D3D3D3;
}

#jizhuyyyqs .gt_footnote {
  margin: 0px;
  font-size: 90%;
  padding-left: 4px;
  padding-right: 4px;
  padding-left: 5px;
  padding-right: 5px;
}

#jizhuyyyqs .gt_sourcenotes {
  color: #333333;
  background-color: #FFFFFF;
  border-bottom-style: none;
  border-bottom-width: 2px;
  border-bottom-color: #D3D3D3;
  border-left-style: none;
  border-left-width: 2px;
  border-left-color: #D3D3D3;
  border-right-style: none;
  border-right-width: 2px;
  border-right-color: #D3D3D3;
}

#jizhuyyyqs .gt_sourcenote {
  font-size: 90%;
  padding-top: 4px;
  padding-bottom: 4px;
  padding-left: 5px;
  padding-right: 5px;
}

#jizhuyyyqs .gt_left {
  text-align: left;
}

#jizhuyyyqs .gt_center {
  text-align: center;
}

#jizhuyyyqs .gt_right {
  text-align: right;
  font-variant-numeric: tabular-nums;
}

#jizhuyyyqs .gt_font_normal {
  font-weight: normal;
}

#jizhuyyyqs .gt_font_bold {
  font-weight: bold;
}

#jizhuyyyqs .gt_font_italic {
  font-style: italic;
}

#jizhuyyyqs .gt_super {
  font-size: 65%;
}

#jizhuyyyqs .gt_footnote_marks {
  font-style: italic;
  font-weight: normal;
  font-size: 75%;
  vertical-align: 0.4em;
}

#jizhuyyyqs .gt_asterisk {
  font-size: 100%;
  vertical-align: 0;
}

#jizhuyyyqs .gt_indent_1 {
  text-indent: 5px;
}

#jizhuyyyqs .gt_indent_2 {
  text-indent: 10px;
}

#jizhuyyyqs .gt_indent_3 {
  text-indent: 15px;
}

#jizhuyyyqs .gt_indent_4 {
  text-indent: 20px;
}

#jizhuyyyqs .gt_indent_5 {
  text-indent: 25px;
}
</style>
<table class="gt_table">
  
  <thead class="gt_col_headings">
    <tr>
      <th class="gt_col_heading gt_columns_bottom_border gt_left" rowspan="1" colspan="1" scope="col" id="Hromada Name">Hromada Name</th>
      <th class="gt_col_heading gt_columns_bottom_border gt_right" rowspan="1" colspan="1" scope="col" id="Number of hromada's friends abroad">Number of hromada's friends abroad</th>
    </tr>
  </thead>
  <tbody class="gt_table_body">
    <tr><td headers="Hromada Name" class="gt_row gt_left">Луцька міська громада</td>
<td headers="Number of hromada's friends abroad" class="gt_row gt_right">17</td></tr>
    <tr><td headers="Hromada Name" class="gt_row gt_left">Тячівська міська громада</td>
<td headers="Number of hromada's friends abroad" class="gt_row gt_right">13</td></tr>
    <tr><td headers="Hromada Name" class="gt_row gt_left">Перечинська міська громада</td>
<td headers="Number of hromada's friends abroad" class="gt_row gt_right">12</td></tr>
    <tr><td headers="Hromada Name" class="gt_row gt_left">Коломийська міська громада</td>
<td headers="Number of hromada's friends abroad" class="gt_row gt_right">11</td></tr>
    <tr><td headers="Hromada Name" class="gt_row gt_left">Самбірська міська громада</td>
<td headers="Number of hromada's friends abroad" class="gt_row gt_right">11</td></tr>
    <tr><td headers="Hromada Name" class="gt_row gt_left">Нововолинська міська громада</td>
<td headers="Number of hromada's friends abroad" class="gt_row gt_right">10</td></tr>
    <tr><td headers="Hromada Name" class="gt_row gt_left">Великобийганська сільська громада</td>
<td headers="Number of hromada's friends abroad" class="gt_row gt_right">9</td></tr>
    <tr><td headers="Hromada Name" class="gt_row gt_left">Долинська міська громада</td>
<td headers="Number of hromada's friends abroad" class="gt_row gt_right">7</td></tr>
    <tr><td headers="Hromada Name" class="gt_row gt_left">Миргородська міська громада</td>
<td headers="Number of hromada's friends abroad" class="gt_row gt_right">7</td></tr>
    <tr><td headers="Hromada Name" class="gt_row gt_left">Бориславська міська громада</td>
<td headers="Number of hromada's friends abroad" class="gt_row gt_right">6</td></tr>
    <tr><td headers="Hromada Name" class="gt_row gt_left">Болградська міська громада</td>
<td headers="Number of hromada's friends abroad" class="gt_row gt_right">6</td></tr>
  </tbody>
  
  
</table>
</div>
```

# 2. Preparation




```{.r .fold-hide}
d1 <- 
  ds1_prep_ordinal_factors %>% 
  pivot_longer(cols = preparation, names_to = "item_name") %>% 
  group_by(item_name,value) %>% 
  summarize(
    count = n_distinct(hromada_code)
    ,.groups = "drop"
  ) %>% 
  group_by(item_name) %>% 
  mutate(
    prop = count/sum(count, rm.na = T)
    ,pct = scales::percent(prop, accuracy = 1)
  ) %>% 
  ungroup()

d2 <- 
  ds1_prep_binary_factors %>% 
  pivot_longer(cols = preparation, names_to = "item_name") %>% 
  group_by(item_name,value) %>% 
  summarize(
    count = n_distinct(hromada_code)
    ,.groups = "drop"
  ) %>% 
  group_by(item_name) %>% 
  mutate(
    prop = count/sum(count, rm.na = T)
    ,pct = scales::percent(prop, accuracy = 1)
  ) %>% 
  ungroup()

d12 <- 
  bind_rows(
    d1
    ,d2 %>% filter(value == "Yes")
  ) %>% 
  left_join(
    d_meta_prep
  ) %>% 
  arrange(item_name, value) 

d_in <- 
  d12 %>% 
    mutate(
    display_name = label_en
    ,display_name = factor(
      display_name
      ,levels =  d12 %>%
        left_join(d_meta_prep) %>%
        filter(value == "Yes") %>%
        # filter(value == "Before Feb 24") %>%
        arrange(prop) %>%
        # arrange(desc(item_number)) %>%
        pull(label_en)
    ) # display_name
    ,value = fct_relevel(
      value
      , "As of Oct", "As of Feb","Yes", "No", "Not Applicable",
    ) %>% fct_rev()
  ) 

g <- 
  d_in %>% 
  { 
  ggplot(
    data = (.) %>% filter(value !="Yes")
    ,aes(x=prop, y = display_name, fill=value)
  )+
  geom_col(position = position_stack()
           , alpha = .7
  )+
  geom_text(aes(label = str_remove(pct,"\\%"))
            ,hjust = 1
            , size = 4
            ,color="white"
            ,position = position_stack()
            ,data = . %>% filter(value !="Yes")
            )+
  geom_text(aes(label = str_remove(pct,"\\%"))
            ,hjust = -.5
            ,vjust = .5
            , size = 2
            ,color="black"
            ,position = position_stack()
            ,data = (.) %>% filter(value=="Yes") %>% mutate(value=NA)

            )+
  scale_x_continuous(
    breaks = seq(.1,1,.1)
    ,labels = scales::percent_format()
    ,expand = expansion(add=c(-.000,-.0))
  )+
  scale_fill_viridis_d(
    breaks = c("As of Oct", "As of Feb", "No", "Not Applicable"),
    begin = 0, end = .8, direction = 1, option = "plasma",guide= guide_legend(reverse=F)
    )+
  labs(
    title = "Have your hromada made the following preparations?"
    ,x = "Percent of respondents", y = NULL, fill = NULL
    ,caption = "Cummulative percent shown in black"
  )+
  theme(
    # panel.grid = element_blank()
    panel.grid.major.y  = element_blank()
    ,panel.border = element_blank()
    ,panel.grid.major.x = element_line(color = "black")
  )
  }

g
```

![](figure-png-iso/preparation-summary-1-1.png)<!-- -->

```{.r .fold-hide}
# g %>% quick_save("2-preparation-summary-yes",w=12,h=5)
```


```{.r .fold-hide}
d1 <- 
  ds1_prep_ordinal_factors %>% 
  pivot_longer(cols = preparation, names_to = "item_name") %>% 
  group_by(item_name,value) %>% 
  summarize(
    count = n_distinct(hromada_code)
    ,.groups = "drop"
  ) %>% 
  group_by(item_name) %>% 
  mutate(
    prop = count/sum(count, rm.na = T)
    ,pct = scales::percent(prop, accuracy = 1)
  ) %>% 
  ungroup()

d2 <- 
  ds1_prep_binary_factors %>% 
  pivot_longer(cols = preparation, names_to = "item_name") %>% 
  group_by(item_name,value) %>% 
  summarize(
    count = n_distinct(hromada_code)
    ,.groups = "drop"
  ) %>% 
  group_by(item_name) %>% 
  mutate(
    prop = count/sum(count, rm.na = T)
    ,pct = scales::percent(prop, accuracy = 1)
  ) %>% 
  ungroup()

d12 <- 
  bind_rows(
    d1
    ,d2 %>% filter(value == "Yes")
  ) %>% 
  left_join(
    d_meta_prep
  ) %>% 
  arrange(item_name, value) 

d_in <- 
  d12 %>% 
    mutate(
    display_name = label_en
    ,display_name = factor(
      display_name
      ,levels =  d12 %>%
        left_join(d_meta_prep) %>%
        filter(value == "Yes") %>%
        # filter(value == "Before Feb 24") %>%
        arrange(prop) %>%
        # arrange(desc(item_number)) %>%
        pull(label_en)
    ) # display_name
    ,value = fct_relevel(
      value
      , "As of Feb", "As of Oct", "Yes", "No", "Not Applicable",
    ) %>% fct_rev()
  ) 

g <- 
  d_in %>% 
  { 
  ggplot(
    data = (.) %>% filter(value !="Yes")
    ,aes(x=prop, y = display_name, fill=value)
  )+
  geom_col(position = position_stack()
           , alpha = .7
  )+
  geom_text(aes(label = str_remove(pct,"\\%"))
            ,hjust = 1
            , size = 4
            ,color="white"
            ,position = position_stack()
            ,data = . %>% filter(value !="Yes")
            )+
  geom_text(aes(label = str_remove(pct,"\\%"))
            ,hjust = -.5
            ,vjust = .5
            , size = 2
            ,color="black"
            ,position = position_stack()
            ,data = (.) %>% filter(value=="Yes") %>% mutate(value=NA)

            )+
  scale_x_continuous(
    breaks = seq(.1,1,.1)
    ,labels = scales::percent_format()
    ,expand = expansion(add=c(-.000,-.0))
  )+
  scale_fill_manual(
    breaks = c("As of Feb", "As of Oct",  "No", "Not Applicable"),
    values = c("#0d0887", "#0b2c9c", "#cc475a", "#bab7b7")
    )+
  labs(
    title = "Have your hromada made the following preparations?"
    ,x = "Percent of respondents", y = NULL, fill = NULL
    ,caption = "Cummulative percent shown in black"
  )+
  theme(
    # panel.grid = element_blank()
    panel.grid.major.y  = element_blank()
    ,panel.border = element_blank()
    ,panel.grid.major.x = element_line(color = "black")
  )
  }

g
```

![](figure-png-iso/preparation-summary-3-1.png)<!-- -->

```{.r .fold-hide}
# g %>% quick_save("2-preparation-summary-yes",w=12,h=5)
```



```{.r .fold-hide}
d1 <- 
  ds1_prep_binary_factors_feb %>% 
  pivot_longer(cols = preparation, names_to = "item_name") %>% 
  group_by(item_name,value) %>% 
  summarize(
    count = n_distinct(hromada_code)
    ,.groups = "drop"
  ) %>% 
  group_by(item_name) %>% 
  mutate(
    prop = count/sum(count, rm.na = T)
    ,pct = scales::percent(prop, accuracy = 1)
  ) %>% 
  ungroup()

d2 <- 
  d1 %>% 
  left_join(
    d_meta_prep
  ) %>% 
  arrange(item_name, value) 

d_in <- 
  d2 %>% 
    mutate(
    display_name = label_en
    ,display_name = factor(
      display_name
      ,levels =  d2 %>%
        left_join(d_meta_prep) %>%
        filter(value == "Yes") %>%
        # filter(value == "Before Feb 24") %>%
        arrange(prop) %>%
        # arrange(desc(item_number)) %>%
        pull(label_en)
    ) # display_name
    ,value = fct_relevel(
      value
      ,"Yes", "No"
    ) %>% fct_rev()
  ) 

g <- 
  d_in %>% 
  { 
  ggplot(data = .,
    aes(x=prop, y = display_name, fill=value)
  )+
  geom_col(position = position_stack()
           , alpha = .7
  )+
  geom_text(aes(label = str_remove(pct,"\\%"))
            ,hjust = -.5
            , size = 4
            ,color="white"
            ,position = position_stack()
            ,data = . %>% filter(value == "Yes")
            )+
  scale_x_continuous(
    breaks = seq(.1,1,.1)
    ,labels = scales::percent_format()
    ,expand = expansion(add=c(-.000,-.0))
  )+
  scale_fill_viridis_d(
    breaks = c("Yes", "No"),
    begin = .5, end = .0, direction = 1, option = "plasma",guide= guide_legend(reverse=F)
    )+
  labs(
    title = "Have your hromada made the following preparations? (before Feb 24)"
    ,x = "Percent of respondents", y = NULL, fill = NULL
  )+
  theme(
    # panel.grid = element_blank()
    panel.grid.major.y  = element_blank()
    ,panel.border = element_blank()
    ,panel.grid.major.x = element_line(color = "black")
  )
  }

g
```

![](figure-png-iso/preparation-summary-2-1.png)<!-- -->

<mark>2.1</mark> What questions were asked about preparations hromadas made? 


```{.r .fold-hide}
ds0 %>% 
  select(preparation) %>% 
  explore::describe_all() %>% 
  left_join(
    meta_survey %>% filter(group=="preparation") %>% select(name,label_en,label)
    ,by=c("variable"="name")) %>% 
  relocate(c("label_en","label"),.after = "variable") %>% 
  select(1:3) %>%
  neat()
```

<table class="table table-striped table-hover table-condensed table-responsive" style="width: auto !important; ">
 <thead>
  <tr>
   <th style="text-align:left;"> variable </th>
   <th style="text-align:left;"> label_en </th>
   <th style="text-align:left;"> label </th>
  </tr>
 </thead>
<tbody>
  <tr>
   <td style="text-align:left;"> prep_first_aid_water </td>
   <td style="text-align:left;"> Water stored (1) </td>
   <td style="text-align:left;"> Сформовані запаси товарів першої необхідності (вода, їжа, медичні засоби) </td>
  </tr>
  <tr>
   <td style="text-align:left;"> prep_first_aid_fuel </td>
   <td style="text-align:left;"> Fuel stored (2) </td>
   <td style="text-align:left;"> Сформовані запаси товарів першої необхідності (паливо) </td>
  </tr>
  <tr>
   <td style="text-align:left;"> prep_reaction_plan </td>
   <td style="text-align:left;"> Plan of response (3) </td>
   <td style="text-align:left;"> Оновлено чи затверджено план реагування на надзвичайні ситуації </td>
  </tr>
  <tr>
   <td style="text-align:left;"> prep_evacuation_plan </td>
   <td style="text-align:left;"> Plan of evacuation (4) </td>
   <td style="text-align:left;"> Складено спеціальний план евакуації населення при загрозі збройного конфлікту </td>
  </tr>
  <tr>
   <td style="text-align:left;"> prep_reaction_plan_oth_hromadas </td>
   <td style="text-align:left;"> Plan coord w/ oth. Hs (5) </td>
   <td style="text-align:left;"> Узгоджено план реагування/дій у разі повномасштабного вторгнення з представниками інших громад </td>
  </tr>
  <tr>
   <td style="text-align:left;"> prep_reaction_plan_oda </td>
   <td style="text-align:left;"> Plan coord w/ Oblast (6) </td>
   <td style="text-align:left;"> Узгоджено план реагування/дій у разі повномасштабного вторгнення з представниками ОДА </td>
  </tr>
  <tr>
   <td style="text-align:left;"> prep_dftg_creation </td>
   <td style="text-align:left;"> Territorial Defense (7) </td>
   <td style="text-align:left;"> Розпочато створення (добровольчого) формування територіальної громади </td>
  </tr>
  <tr>
   <td style="text-align:left;"> prep_national_resistance </td>
   <td style="text-align:left;"> Plan of resistance (8) </td>
   <td style="text-align:left;"> Затверджена та опрацьована представниками ОМС програма національного спротиву на території громади </td>
  </tr>
  <tr>
   <td style="text-align:left;"> prep_starosta_meeting </td>
   <td style="text-align:left;"> Meeting with heads (9) </td>
   <td style="text-align:left;"> Проведена зустріч зі старостами з приводу дій у випадку вторгнення </td>
  </tr>
  <tr>
   <td style="text-align:left;"> prep_communal_meetiing </td>
   <td style="text-align:left;"> Meeting with utilities (10) </td>
   <td style="text-align:left;"> Проведена зустріч з головами комунальних підприємств з приводу дій у випадку вторгнення </td>
  </tr>
  <tr>
   <td style="text-align:left;"> prep_online_map </td>
   <td style="text-align:left;"> Shelter map online (11) </td>
   <td style="text-align:left;"> Опублікована онлайн-мапа укриттів в громаді </td>
  </tr>
  <tr>
   <td style="text-align:left;"> prep_shelter_list </td>
   <td style="text-align:left;"> Shelter list online (12) </td>
   <td style="text-align:left;"> Опублікований перелік адрес укриттів в соцмережах або на сайті громади </td>
  </tr>
  <tr>
   <td style="text-align:left;"> prep_notification_check </td>
   <td style="text-align:left;"> Communication tested (13) </td>
   <td style="text-align:left;"> Перевірено засоби оповіщення населення </td>
  </tr>
  <tr>
   <td style="text-align:left;"> prep_backup </td>
   <td style="text-align:left;"> Data backup fully (14) </td>
   <td style="text-align:left;"> Здійснено повне централізоване резервне копіювання даних громади (критично важливої інформації, наприклад, даних про інфраструктуру) </td>
  </tr>
  <tr>
   <td style="text-align:left;"> prep_partly_backup </td>
   <td style="text-align:left;"> Data backed up partially (15) </td>
   <td style="text-align:left;"> Здійснено часткове резервне копіювання даних громади (критично важливої інформації, наприклад, даних про інфраструктуру) </td>
  </tr>
</tbody>
</table>

## Item-total correlations 


> We can conceptualize hromadas' the preparation for invasion as two quantities:  
- `prep_score_feb` - the number of security measures implemented as of February 2022   
- `prep_score_oct` - the number of security measures implemented as of October 2022


```r
ds1_prep %>% select(1:4) # + individual preparation items
```

```
# A tibble: 138 x 4
   hromada_code        prep_score_combo prep_score_feb prep_score_oct
   <chr>                          <dbl>          <dbl>          <dbl>
 1 UA05100110000070795               12              0             12
 2 UA05100170000071290                9              0              9
 3 UA05120070000075759               18              5             13
 4 UA05060130000030729               13              2             11
 5 UA05020110000052014               14              3             11
 6 UA07060370000022360               17              3             14
 7 UA07080170000013585               19              4             15
 8 UA07020130000036300               12              1             11
 9 UA07060290000054842               17              2             15
10 UA07060390000098670                7              0              7
# ... with 128 more rows
```

> We  also compute `prep_score_combo`, which is a sum of `prep_score_feb` and `prep_score_oct`, the quantity equivalent to weighting the implementation of security measures  prior to Feb 24, 2022 **twice as important**.

> These three scores are distributed as follows:


```{.r .fold-hide}
g <-  
  ds1_prep %>%
  select(starts_with("prep_score")) %>% 
  pivot_longer(cols = everything(),names_to = "measure",values_to="value") %>% 
  mutate( 
    measure = factor(measure,
                        levels = c("prep_score_feb","prep_score_oct","prep_score_combo")
                        ,labels = c("..as of February","..as of October", "Combined = Feb + Oct")
                        )
  ) %>% 
  ggplot(aes(x=value))+
  geom_histogram(binwidth = 1, alpha = .4)+
  scale_x_continuous(breaks = seq(0,30,5),minor_breaks = NULL)+
  facet_wrap("measure",ncol =1)+
  labs(
    title = "How many security measures have your hromada implemented..."
    # ,subtitle = "Combined score = February + October"
  )
g
```

![](figure-png-iso/info-score-distribution-1.png)<!-- -->

```{.r .fold-hide}
# g %>%  quick_save("score-distribution",w=3.5, h=6)
```


```{.r .fold-hide}
ds1_prep %>% select(starts_with("prep_score")) %>% GGally::ggpairs()
```

![](figure-png-iso/unnamed-chunk-9-1.png)<!-- -->

```{.r .fold-hide}
# Note that correlation coefficient is Pearson
```

> The item-total correlations indicates that all three preparedness scores are adequate unidimensional measures.


```{.r .fold-hide}
# Step 1 - create data sets with re-coded item responses
# As of February 2022, how many of these security steps have been implemented?
d_feb <- 
  ds_survey %>% 
  mutate(
    across(
      .cols = preparation
      ,.fns = ~case_when(
        .  == 0 ~ 0 #"No"
        ,. == 1 ~ 0 #"After Feb 24"
        ,. == 2 ~ 1 #"Before Feb 24"
      )
    )
  ) %>% 
  select(hromada_code, preparation)


# As of October 20200, how many of these security steps have been implemented?
d_oct <- 
  ds_survey %>% 
  mutate(
    across(
      .cols = preparation
      ,.fns = ~case_when(
        .  == 0 ~ 0 #"No"
        ,. == 1 ~ 1 #"After Feb 24"
        ,. == 2 ~ 1 #"Before Feb 24"
      )
    )
  ) %>% 
  select(hromada_code, preparation)

# What is the combined score of preparedness if we give 2 points for having
# a security measure implemented as of February, and 1 point - as of October?
d_combo <- 
  ds_survey %>% 
  select(hromada_code, preparation)



# convert to matrices
m_feb <- 
  ds1_prep %>% select(hromada_code, starts_with("prep_score_")) %>% 
  left_join(d_feb) %>% # raw scores have been converted to binary for as of Feb
  select(-hromada_code) %>% 
  make_corr_matrix(na_action = "remove", method="spearman")

m_oct <- 
  ds1_prep %>% select(hromada_code, starts_with("prep_score_")) %>% 
  left_join(d_oct) %>% # raw scores have been converted to binary for as of Oct
  select(-hromada_code) %>% 
  make_corr_matrix(na_action = "remove", method="spearman")

m_combo <- 
  ds1_prep %>% select(hromada_code, starts_with("prep_score_")) %>% 
  left_join(d_combo) %>% 
  select(-hromada_code) %>% 
  make_corr_matrix(na_action = "remove", method="spearman")


d_item_total <- 
  list(
    "Combination" = m_combo[,"prep_score_combo"]
    ,"February"  = m_feb[,"prep_score_feb"]
    ,"October"  = m_oct[,"prep_score_oct"]
  ) %>% 
  as_tibble() %>% 
  mutate(item_name = rownames(m_combo)) %>% 
  filter(item_name != c("prep_score_combo","prep_score_feb","prep_score_oct")) %>% 
  mutate(item_name = factor(item_name)) %>% 
  relocate(item_name) %>% 
  pivot_longer(
    cols = 2:4
    ,names_to = "scenario"
    ,values_to = "correlation"
  ) %>% 
  mutate(
    discrimination = case_when(
      correlation <= 0  ~ "problematic"
      ,correlation > 0 & correlation < .2 ~ "poor"
      ,correlation >=.2 & correlation < .4 ~ "good"
      ,correlation >=.4  ~ "very good"
    ) %>% factor(levels = c("problematic","poor","good","very good"))
    ,scenario = scenario %>% factor(
      labels=c("February","October","Combination"))
    ,item_name = factor(item_name,levels = preparation) %>% fct_rev()
  )

discrimination_levels <- c(
  "problematic" = "#d01c8b"
  ,"poor"        = "#f1b6da"
  ,"good"        = "#b8e186"
  ,"very good"   = "#4dac26"
)

g_item_total <-
  d_item_total %>% 
  ggplot(aes(x = item_name, y = correlation, color = discrimination, group = scenario))+
  geom_line(aes(group = "scenario"))+
  geom_point()+
  geom_text(aes(label=correlation %>% scales::number(accuracy = .01) %>% RemoveLeadingZero()),hjust=-.3
            ,size = 3)+
  geom_hline(aes( yintercept = 0))+ 
  facet_wrap("scenario",nrow=1)+
  scale_y_continuous(limits = c(-.3,.7), expand = expansion(add = c(0,.2)))+
  scale_color_manual(
    values = discrimination_levels
    , limits = names(discrimination_levels)
  )+
  coord_flip() +
  labs(
    title = "Item-total corellations under three scoring scenarios"
    ,y = "Item-total Correlation (Spearman)"
    ,x = NULL
    ,color = "Discrimination"
  )

g_item_total
```

![](figure-png-iso/prep-item-total-1.png)<!-- -->

```{.r .fold-hide}
g_item_total %>% quick_save("item-total",w=8,h=4)
```

> While all three metrics should be considered during modeling, the next section demonstrates why and how the interpreations of these scores will differ

## Prep score change
 
> Let's us visualize individual scores of invasion preparedness


```{.r .fold-hide}
d <- 
  ds1_prep %>% 
  select(hromada_code, starts_with("prep_score_")) %>% 
arrange(prep_score_oct, prep_score_feb) 

make_plot_prep_change_bw <- function(
    d
    ,order_by #= c("prep_score_feb","prep_score_oct")
    ,color_by #= "row_number_ntile"
    ,ntile_count = 10
){
  # browser()
  
  level_order <- d %>% arrange(!!!rlang::syms(order_by)) %>% pull(hromada_code)
  caption_text = paste0("Order by: ", paste0(order_by,collapse = " + "), " | Color by: ", color_by)
  g <- 
    d %>%
    arrange(!!!rlang::syms(order_by)) %>% 
    mutate(
      hromada_code = hromada_code %>% factor(levels = level_order)
      ,prep_score_combo_ntile = ntile(prep_score_combo,ntile_count)        %>% factor()
      ,prep_score_feb_ntile   = ntile(prep_score_feb,ntile_count) %>% factor()
      ,prep_score_oct_ntile   = ntile(prep_score_oct,ntile_count)  %>% factor()
      ,row_number_ntile       = ntile(row_number(),ntile_count)      %>% factor()
    ) %>% 
    # graphing begins
    ggplot(aes(y=hromada_code, color = !!rlang::sym(color_by) ))+
    geom_segment(
      aes(
        y     = hromada_code
        ,yend = hromada_code
        ,x    = prep_score_feb
        ,xend = prep_score_oct
      )
      ,linewidth = 2 ,alpha = 1
    )+
    geom_segment(
      aes(
        y     = hromada_code
        ,yend = hromada_code
        ,x    = 0
        ,xend = prep_score_feb
      )
      ,linewidth = 2 ,alpha = .1
      , color = "black"
    )+
    scale_color_brewer(type="div", palette = "Spectral")+
    scale_x_continuous(
      breaks = seq(0,15,5),minor_breaks = seq(0,15,1)
      # ,limits = c(-10,25)
      )+
    labs(
      title = paste0("The number of security measures implemented by hromadas (N= ",
                     d %>% summarize(n=n_distinct(hromada_code)) %>% pull(n)
                     ,")")
      ,subtitle = caption_text
      ,x = "Each segment starts at February and ends at October preparedness score"
      # ,caption = caption_text
      ,y = NULL
      ,color = "Percentile\nGroup\n"
    )+
    theme(
      axis.text.y = element_blank()
      ,panel.grid.major.y = element_blank()
      ,panel.border = element_blank()
    )+
    guides(color = guide_legend(override.aes = list(linewidth=7), reverse=TRUE))
  return(g)
}
# Ordering by the total score (before + after OR sum(0|1|2)) 
g <- d %>% make_plot_prep_change_bw(order_by = "prep_score_combo",color_by = "prep_score_combo_ntile") # 
g + labs(color = "Percentile\nGroup\n(Combo)")
```

![](figure-png-iso/prep-change-segment-1-1.png)<!-- -->

```{.r .fold-hide}
# g %>% quick_save("prep-change-segment-bw",w=5.5,h=9)
```

> However,this scoring method may not work for operationalizing preparedness as of October


```{.r .fold-hide}
g <- d %>% make_plot_prep_change_bw(order_by = c("prep_score_oct","prep_score_feb"), color_by = "prep_score_combo_ntile")
g + labs(color = "Percentile\nGroup\n(Combo)")
```

![](figure-png-iso/prep-change-segment-2-1.png)<!-- -->

> or as of February


```{.r .fold-hide}
g <- d %>% make_plot_prep_change_bw(order_by = c("prep_score_feb","prep_score_oct"), color_by = "prep_score_combo_ntile")
g + labs(color = "Percentile\nGroup\n(Combo)")
```

![](figure-png-iso/prep-change-segment-3-1.png)<!-- -->


 
<mark>**Conclusion**</mark> 

> Both `prep_score_feb` and `prep_score_oct` are meaningful, adequate unidimensional measures with a straightforward interpretation: *Number of security measures implemented as of a given date*. 

> The measure `prep_score_combo` is also an adequate unidimensional measure, but it does not have a clear interpretation of its value.
 





# 3. Information



```{.r .fold-hide}
meta_survey %>% filter(group=="information_hat") %>% pull(label) %>% cat()
```

```
Який з перелічених нижче засобів використовується громадою для інформування населення...
```

> The most popular channel of communication for hromadas before February 24th and still is Facebook.
Before February 24th, only about a quarter of hromadas in our sample had accounts on Viber and Telegram. \
After February, hromadas doubled their use of alternative channels for communicating with their hromada: Viber, Telegram, Chat for getting help, hotline - now about half of hromadas have official groups on Viber and a hotline.

![](figure-png-iso/information-summary-1-1.png)<!-- -->

> Most hromadas had at least one account in social networks before the invasion.


```{.r .fold-hide}
d <- 
  ds0 %>%  
  select(hromada_code,head_hromada_communication, facebook,viber,telegram ) %>% 
  pivot_longer(cols = c("facebook","viber","telegram")) %>% 
  mutate(
    had_sn_before = case_when(
      value %in% c(0,1) ~ "No cccount before Feb24"
      ,value %in% c(2)  ~ "Had cccount before Feb24"
    )
  ) %>% 
  arrange(hromada_code, head_hromada_communication) %>% 
  group_by(hromada_code,head_hromada_communication) %>% 
  mutate(
    had_any_sn_before_feb24 = sum(value==2,na.rm = T)>0
  ) %>% 
  ungroup() %>% 
  distinct(hromada_code, head_hromada_communication,had_any_sn_before_feb24) %>% 
  mutate(
    time_per_week = fct_recode(head_hromada_communication,
        "1"  = "once_a_week"    
      , "7"  = "once_a_day"   
      , "0"  = "none"   
      , "3"  = "few_times_a_week"  
      , "15" = "2_3_times"   
    ) %>% as.character() %>% as.integer()
    , head_hromada_communication = fct_recode(
      head_hromada_communication,
       "Once a week"      = "once_a_week"
      ,"Once a day"       = "once_a_day"
      ,"Never"            = "none"
      ,"Few times a week" = "few_times_a_week"
      ,"2-3 times a day"  = "2_3_times"
    ) %>% factor( levels = c(
       "Never"           
       ,"Once a week"     
       ,"Few times a week"
       ,"Once a day"      
       ,"2-3 times a day"
    )
    )
  ) 
(d %>% make_bi_freq_graph("had_any_sn_before_feb24")) +
  labs(
    title = "Did hromadas have account on any social network before the invasion?"
    ,subtitle = "Social networks considered: Facebook, Viber, Telegram"
    ,y = NULL, x = NULL, fill = "Had account"
  ) +
  scale_x_discrete(labels=c('TRUE' = 'Had account', 'FALSE' = 'No account')) +
  scale_fill_discrete(labels=c('TRUE' = 'Had account', 'FALSE' = 'No account'))
```

![](figure-png-iso/info-2-1.png)<!-- -->



```{.r .fold-hide}
meta_survey %>% filter(group=="information_freq") %>% pull(label) %>% cat()
```

```
Як часто в середньому голова громади звертався (відео чи повідомлення) до населення громади стосовно стану справ у перший місяць від повномасштабного вторгнення?
```


> Almost all hromada heads actively communicated with their hromada during the first month of the invasion: 95% addressed the hromada once a week or more, and 37% did it daily.


```{.r .fold-hide}
(ds0 %>% 
  mutate(
    
    head_hromada_communication = fct_recode(
      head_hromada_communication,
       "Once a week"      = "once_a_week"
      ,"Once a day"       = "once_a_day"
      ,"Never"            = "none"
      ,"Few times a week" = "few_times_a_week"
      ,"2-3 times a day"  = "2_3_times"
    ) %>% factor( levels = c(
       "Never"           
       ,"Once a week"     
       ,"Few times a week"
       ,"Once a day"      
       ,"2-3 times a day"
    )
    )
  ) %>% 
  make_bi_freq_graph("head_hromada_communication") )+
  labs(
    title = "How frequently did hromada head communicated in the first month of invasion?"
    ,x = NULL, fill = NULL
  )
```

![](figure-png-iso/info-1 -1.png)<!-- -->

> Mayors of hromadas that had any social account before February 24th on average adressed hromada about the state of affairs significantly more during first month of invasion than those that didn't have.


```{.r .fold-hide}
d %>% 
  group_by(had_any_sn_before_feb24) %>% 
  summarize(mean_times_per_week = mean(time_per_week,na.rm =T)) %>% 
  ggplot(aes(x=mean_times_per_week, y= had_any_sn_before_feb24,
             fill = had_any_sn_before_feb24))+
  geom_col()+
  geom_text(aes(label=scales::comma(mean_times_per_week)))+
  labs(
    title = "How frequently did heads of hromadas communicate with the community during the first month of invation \ndepending on having any social account before 24th February?"
    ,subtitle = "Social networks considered: Facebook, Viber, Telegram"
    ,y = NULL, x = "Average times per week", fill = "Had account on\nany social network"
  ) +
  scale_y_discrete(labels=c('TRUE' = 'Had account', 'FALSE' = 'No account')) +
  scale_fill_discrete(labels=c('TRUE' = 'Had account', 'FALSE' = 'No account'))
```

![](figure-png-iso/info-3 -1.png)<!-- -->


```{.r .fold-hide}
d %>% 
  group_by(head_hromada_communication,had_any_sn_before_feb24) %>% 
  summarize(
    hromada_count = n_distinct(hromada_code)
    ,.groups = "drop"
  ) %>% 
  group_by(had_any_sn_before_feb24) %>% 
  mutate(
    prop = hromada_count/sum(hromada_count)
    ,pct = scales::percent(prop, accuracy = 1)
  ) %>% 
  ggplot(aes(x=prop, y = head_hromada_communication, fill = had_any_sn_before_feb24))+
  geom_col(position = position_dodge())+
  geom_text(aes(label = pct), hjust = -.5,position = position_dodge(width = .9))+
  scale_x_continuous(labels = scales::percent_format(),expand = expansion(add = c(0,.1)))+
  ggplot2::scale_fill_viridis_d(begin = 0, end = .8, direction = -1, option = "plasma",guide= guide_legend(reverse=T)
                                ,labels = c('TRUE' = 'Had account', 'FALSE' = 'No account')
    )+
  labs(
    title = "How frequently did heads of hromadas communicated during the first month of invasion?"
    ,fill = "Had accounts\non social networks\nbefore Feb 24"
    ,x = "Percent of respondents in each group"
    , y = NULL
  )
```

![](figure-png-iso/info-4 -1.png)<!-- -->


# 4. National Resistance


```{.r .fold-hide}
meta_survey %>% filter(name=="dftg_creation") %>% pull(label) %>% cat()
```

```
Чи було у вашій громаді офіційно сформоване добровольче формування територіальної громади?
```

> About a third of hromadas in our survey had not yet created a volunteer defense force of territorial communities (VDF) as of October.


```{.r .fold-hide}
d <- ds0 %>% 
  select(hromada_code, dftg_creation, type, region_en) %>%
  mutate(dftg_creation = factor(dftg_creation, levels = c('not_able', 'still_not', 'yes')),
         dftg_creation = fct_recode(dftg_creation,
             'Yes' = 'yes'
            ,"Didn't due to quick occupation" = 'not_able'
            ,'Still not created' = 'still_not')
  )

(d %>% make_bi_freq_graph('dftg_creation')) +
  labs(
    title = "Did hromadas create a voluntary formation of a territorial community?"
    ,y = NULL, x = NULL, fill = NULL
  )
```

![](figure-png-iso/dftg-1-1.png)<!-- -->

> Among village and urban village communities, a higher proportion of hromadas had not yet created a volunteer defense force of territorial communities (VDF).


```{.r .fold-hide}
d %>%
  group_by(type) %>%
  count(dftg_creation) %>%
  mutate(prop = n/sum(n)
         ,pct = scales::percent(prop, accuracy = 1)) %>% 
  ggplot(aes(x=prop, y = dftg_creation, fill = type))+
  geom_col(position = position_dodge())+
  geom_text(aes(label = pct), hjust = -.5,position = position_dodge(width = .9))+
  scale_x_continuous(labels = scales::percent_format(),expand = expansion(add = c(0,.1)))+
  ggplot2::scale_fill_viridis_d(begin = 0, end = .8, direction = -1, option = "plasma",guide= guide_legend(reverse=T)
  )+
  labs(
    title = "Did hromadas create a voluntary formation of a territorial community?"
    ,fill = "Hromada Type"
    ,x = "Percent of respondents in each group"
    , y = NULL
  )
```

![](figure-png-iso/dftg-2-1.png)<!-- -->


```{.r .fold-hide}
d %>%
  group_by(region_en) %>%
  count(dftg_creation) %>%
  mutate(prop = n/sum(n)
         ,pct = scales::percent(prop, accuracy = 1)) %>% 
  ggplot(aes(x=prop, y = region_en, fill = fct_rev(dftg_creation)))+
  geom_col(position = position_dodge())+
  geom_text(aes(label = pct), hjust = .3, vjust = -.4, position = position_dodge(width = .9))+
  scale_x_continuous(labels = scales::percent_format(),expand = expansion(add = c(0,.1)))+
  scale_fill_viridis_d(
    begin = 0, end = .8, direction = 1, option = "plasma",guide= guide_legend(reverse=F)
    )+  
  labs(
    title = "Did hromadas create a voluntary formation of a territorial community?"
    ,fill = NULL
    ,x = "Percent of respondents in each group"
    , y = NULL
  )+
  coord_flip()
```

![](figure-png-iso/dftg-region-1.png)<!-- -->

> Here you can see the timeline of creation of these forces for our sample.


```{.r .fold-hide}
d <- ds0 %>% select(dftg_creation_date) %>% group_by(dftg_creation_date) %>% 
  summarise(n = n()) %>%
  filter(!is.na(dftg_creation_date) & dftg_creation_date > '2021-12-28') %>%
  mutate(cum = cumsum(n))

p <- d %>%
  ggplot(aes(x = dftg_creation_date, y = cum)) +
  geom_line() +
  geom_vline(aes(xintercept = as.POSIXct('2022-02-24')), 
             color = 'red', linetype = 'dashed') +
  geom_vline(aes(xintercept = as.POSIXct('2021-12-29')), 
             color = 'red', linetype = 'dashed') +
  geom_rect(aes(xmin = as.POSIXct('2022-02-24'), xmax = as.POSIXct('2022-03-24'), 
                ymin = -Inf, ymax = Inf),
            color = 'coral1', fill = 'coral1', alpha = 0.02) +
  theme_bw() +
  theme(axis.title.y = element_blank(),
        axis.title.x = element_blank()) +
  scale_x_datetime(limits = c(as.POSIXct('2021-12-01'), as.POSIXct('2022-10-15')), date_breaks = '1 month') +
  annotate(geom = 'label', x = as.POSIXct('2021-12-31'), y = 80, size = 3,
           label = 'December 29th: \nThe order of formation and \nactivity of the VDF was defined') +
  annotate(geom = 'label', x = as.POSIXct('2022-02-28'), y = 70, size = 3,
           label = 'Full-scale \nrussian invasion') +
  annotate(geom = 'text', x = as.POSIXct('2022-03-26'), y = 90, hjust = 0, size = 3,
           label = '59 VDFs created after a \nfirst month of invasion in our sample', fontface = 'italic') +
  labs(title = 'Number of VDFs created by ATCs')
p
```

![](figure-png-iso/dftg-date-1.png)<!-- -->


```{.r .fold-hide}
d <- ds0 %>% select(dftg_creation_date, type) %>% group_by(dftg_creation_date, type) %>% 
  filter(!is.na(dftg_creation_date) & dftg_creation_date > '2021-12-28') %>%
  summarise(n = n()) %>%
  pivot_wider(values_from = n, names_from = type) %>%
  rename(c = 'міська', v = 'сільська', s = 'селищна') %>%
  ungroup() %>% 
  mutate(c = replace_na(c, 0),
         v = replace_na(v, 0),
         s = replace_na(s, 0),
         c_cum = cumsum(c),
         s_cum = cumsum(s),
         v_cum = cumsum(v)) %>% 
  select(-c(c,v,s)) %>%
  pivot_longer(-dftg_creation_date, values_to = 'n', names_to = 'type')

p <- d %>%
  ggplot(aes(x = dftg_creation_date, y = n, color = type)) +
  geom_point() +
  geom_vline(aes(xintercept = as.POSIXct('2022-02-24')), 
             color = 'red', linetype = 'dashed') +
  geom_vline(aes(xintercept = as.POSIXct('2021-12-29')), 
             color = 'red', linetype = 'dashed') +
  geom_rect(aes(xmin = as.POSIXct('2022-02-24'), xmax = as.POSIXct('2022-03-24'), 
                ymin = -Inf, ymax = Inf),
            color = 'coral1', fill = 'coral1', alpha = 0.007) +
  theme_bw() +
  theme(axis.title.y = element_blank(),
        axis.title.x = element_blank(),
        legend.position = 'top') +
  scale_x_datetime(limits = c(as.POSIXct('2021-12-01'), as.POSIXct('2022-10-15')), date_breaks = '1 month') +
  scale_y_continuous(limits = c(0,40)) +
  scale_color_discrete(labels = c('c_cum' = 'City', 's_cum' = 'Urban Village', 'v_cum' = 'Village')) +
  annotate(geom = 'label', x = as.POSIXct('2021-12-31'), y = 36, size = 3,
           label = 'December 29th: \nThe order of formation and \nactivity of the voluntary \nformations of ATC was defined') +
  annotate(geom = 'label', x = as.POSIXct('2022-02-28'), y = 37, size = 3,
           label = 'Full-scale \nrussian invasion') +
  annotate(geom = 'text', x = as.POSIXct('2022-03-26'), y = 37, hjust = 0, size = 3,
           label = '59 voluntary formations of ATC \nafter a first month', fontface = 'italic') +
  labs(title = 'Number of DFTG created by ATCs', color = NULL)
p
```


> Only a few hromadas have created VDF before the invasion. \
However, more than a half quickly reacted and managed to create VDF in the first month of the invasion. \
Moreover, there are differences in speed of reaction by hromada type.


```{.r .fold-hide}
before <- interval(start = "2021-12-28", end = "2022-02-23")
first_month <- interval(start = "2022-02-24", end = "2022-03-23")
second_month <- interval(start = "2022-03-24", end = "2022-04-23")
third_and_more <- interval(start = "2022-04-24", end = "2022-11-30")


d <- ds0 %>% filter(!is.na(dftg_creation_date) & dftg_creation_date > '2021-12-28') %>%
  mutate(
    invasion_duration = case_when(dftg_creation_date %within% before ~ "before the invasion",
                                  dftg_creation_date %within% first_month ~ "first month of invasion",
                                  dftg_creation_date %within% second_month ~ "second month of invasion",
                                  dftg_creation_date %within% third_and_more ~ "after second month of invasion"),
    invasion_duration = factor(invasion_duration, levels = c("before the invasion", "first month of invasion",
                                                             "second month of invasion", "after second month of invasion")))

p <- d %>%
  select(invasion_duration) %>% 
  count(invasion_duration) %>%
  mutate(prop = n/sum(n)
         ,pct = scales::percent(prop, accuracy = 1)) %>% 
  ggplot(aes(y=prop, x = invasion_duration))+
  geom_col(position = position_dodge(), fill = "dodgerblue")+
  geom_text(aes(label = pct), position = position_dodge(width = 1), vjust = -0.5)+
  scale_y_continuous(labels = scales::percent_format(),expand = expansion(add = c(0,.1)))+
  labs(
    title = "When VDF was created?"
    ,x = NULL
    ,y = NULL
  )
p
```

![](figure-png-iso/dftg-time-type-1.png)<!-- -->

```{.r .fold-hide}
p <- d %>%
  select(invasion_duration, type) %>% 
  group_by(type) %>%
  count(invasion_duration) %>%
  mutate(prop = n/sum(n)
         ,pct = scales::percent(prop, accuracy = 1)) %>% 
  ggplot(aes(y=prop, x = invasion_duration, fill = type))+
  geom_col(position = position_dodge())+
  geom_text(aes(label = pct), position = position_dodge(width = 1), vjust = -0.5)+
  scale_y_continuous(labels = scales::percent_format(),expand = expansion(add = c(0,.1)))+
  ggplot2::scale_fill_viridis_d(begin = 0, end = .8, direction = -1, option = "plasma")+
  labs(
    title = "When VDF was created?"
    ,fill = "Hromada Type"
    ,y = "Percent of respondents in each group"
    , x = NULL
  )

p
```

![](figure-png-iso/dftg-time-type-2.png)<!-- -->

```{.r .fold-hide}
p <- d %>%
  select(invasion_duration, region_en) %>% 
  group_by(region_en) %>%
  count(invasion_duration) %>%
  mutate(prop = n/sum(n)
         ,pct = scales::percent(prop, accuracy = 1)) %>% 
  ggplot(aes(y=prop, x = region_en, fill = invasion_duration))+
  geom_col(position = position_dodge())+
  geom_text(aes(label = pct), position = position_dodge(width = .9), vjust = -.6)+
  scale_y_continuous(labels = scales::percent_format(),expand = expansion(add = c(0,.1)))+
  ggplot2::scale_fill_viridis_d(begin = 0, end = .8, direction = -1, option = "plasma")+
  labs(
    title = "When VDF was created?"
    ,fill = "Period of creation"
    ,y = "Percent of respondents in each group"
    , x = NULL
  )

p
```

![](figure-png-iso/dftg-time-type-3.png)<!-- -->

> Almost all communities provided at least products for military, significant majority allocated rooms and provided money and transport. 


```{.r .fold-hide}
help_military_levels <- c('rooms', 'transport', 'money', 'products', 'other', "none")

d <- ds0 %>% select(hromada_code, starts_with('help_for_military/')) %>% 
  pivot_longer(-hromada_code, names_to = 'help', values_to = 'count') %>%
  count(help, count) %>% group_by(help) %>% 
  mutate(freq = n/sum(n),
         help = str_remove(help, 'help_for_military/')) %>%
  filter(count == 1)

p <- d %>% 
  ggplot(aes(x = factor(help, levels = help_military_levels), y = freq)) +
  geom_col(fill = "dodgerblue") +
  geom_label(aes(label = scales::percent(freq)))  + 
  theme_bw() +
  theme(axis.title.y = element_blank(),
        axis.title.x = element_blank()) +
  labs(title = 'Provided aid for military', subtitle = "excluding occupied hromadas") +
  scale_y_continuous(labels = scales::percent) +
  scale_x_discrete(labels=c('rooms' = 'Allocated rooms', 'transport' = 'Provided transport',
                            "money" = "Provided money", "products" = "Provided products \n(food, medicine, fuel)",
                            "other" = "Other types of assistance", "none" = "Nothing"))

p
```

![](figure-png-iso/help-military-1.png)<!-- -->

> Most communities provided several types of aid: 42% provided transport, money and products, and allocated rooms for military; 72% provided three and more of those \
>  5% of communities haven't provided anything from our list to military.


```{.r .fold-hide}
ds0 %>% 
  count(help_military_count) %>% 
  filter(!is.na(help_military_count)) %>% 
  mutate(freq = n/sum(n)) %>%
  ggplot(aes(x = factor(help_military_count), y = freq)) +
  geom_bar(stat = "identity", fill = "blue") +
  geom_label(aes(label = scales::percent(freq)))  + 
  scale_y_continuous(labels = scales::percent) +
  labs(
    title = "Number of types of aid provided for the military"
    ,subtitle = "Data were collected during October-November of 2022"
    ,y = "Share of hromadas", x = NULL, fill = NULL
  ) 
```

![](figure-png-iso/help-military-number-1.png)<!-- -->



```{.r .fold-hide}
d <- ds0 %>% filter(`help_for_military/transport` == 1) %>%
  select(hromada_code, transport_help_communal_coded, transport_help_bought_coded) %>%
  pivot_longer(-hromada_code, names_to = 'transport_type', values_to = 'number_transport') %>%
  mutate(transport_type = case_when(
  transport_type == 'transport_help_bought_coded' ~ 'Bought by community',
  transport_type == 'transport_help_communal_coded' ~ 'From communal enterprises'),
  number_transport = case_when(number_transport == 0 ~ NA_real_, 
                               TRUE ~ number_transport)
  )

beeswarm(d$number_transport ~ d$transport_type,  method='swarm', 
         main = 'Number of vehicles provided for military', xlab = NA, ylab = NA,
         col=c(1, 2), pch = 19, cex = 0.7, 
         horizontal = TRUE, corral = 'random', xaxt="n")
axis(1, at=seq(0, 200, by = 10), las=2)
bxplot(d$number_transport ~ d$transport_type, horizontal = TRUE, add = TRUE, lwd = 1.2)
```

# 5. Administrative Adaptation

```{.r .fold-hide}
meta_survey %>% filter(name=="percent_working_march") %>% pull(label) %>% cat()
```

```
Який відсоток працівників виконавчих органів влади (від усіх працівників станом на 23 лютого) фактично працювали станом на 1 березня (в тому числі віддалено)?
```

```{.r .fold-hide}
meta_survey %>% filter(name=="percent_working_now") %>% pull(label) %>% cat()
```

```
Який відсоток працівників виконавчих органів влади (від усіх працівників станом на 23 лютого) фактично працюють станом на сьогоднішній день (в тому числі віддалено)?
```

> Questions regarding the percentage of working administrative staff were not effective: Most hromadas reported \ that 90-100% of staff were working both in March and October, and there was no difference between these numbers that would indicate administrative adaptation.


```{.r .fold-hide}
g <-  ds0 %>%
  select(starts_with("percent_working")) %>% 
  pivot_longer(cols = everything(),names_to = "measure",values_to="value") %>% 
  mutate( 
    measure = factor(measure,
                        levels = c("percent_working_march","percent_working_now")
                        ,labels = c("Percent of staff working in March 2022",
                                    "Percent of staff working as of now")
                        )
  ) %>% 
  ggplot(aes(x=value))+
  geom_histogram(binwidth = 100, alpha = .5, breaks=c(10,20,30,40,50,60,70,80,90,100), fill = "dodgerblue")+
  facet_wrap("measure",ncol =5) +
  labs(
    y='number of hromadas'
    ,x=NULL
  )
g
```

![](figure-png-iso/admin-percent-working-1.png)<!-- -->

```{.r .fold-hide}
g %>%  quick_save("score-distribution",w=4, h=6)
```

> 73% of communities regularly coordinate actions, typically several times a week or more. \
Communities affected by war differ from those in peaceful regions; they were more likely to both have daily meetings with other communities and also have no meetings at all. \


```{.r .fold-hide}
d <- ds0 %>% 
   filter(!is.na(commun_between_hromadas)) %>%
  mutate(
    freq_hromadas_com = fct_recode(
      commun_between_hromadas,
       "Daily"      = "Daily"
      ,"Several times a week"       = "Several times a week"
      ,"Several times a month" = "Several times a month"
      ,"Once a month and less" = "Once a month and less"
      ,"No meetings/calls" = "No meetings/calls"
    ) %>% factor( levels = c(
       "Daily"           
       ,"Several times a week"
       ,"Several times a month"
       ,"Once a month and less"
       ,"No meetings/calls"
    )
    )
  ) %>%
  select(freq_hromadas_com, occupation_and_combat_fct, hromada_code)

d %>% 
  group_by(freq_hromadas_com,occupation_and_combat_fct) %>% 
  summarize(
    hromada_count = n_distinct(hromada_code)
    ,.groups = "drop"
  ) %>% 
  group_by(occupation_and_combat_fct) %>% 
  mutate(
    prop = hromada_count/sum(hromada_count)
    ,pct = scales::percent(prop, accuracy = 1)
  ) %>% 
  ggplot(aes(y=prop, x = freq_hromadas_com, fill = occupation_and_combat_fct))+
  geom_col(position = position_dodge())+
  geom_text(aes(label = pct), hjust = .3, vjust = -.2, position = position_dodge(width = .9))+
  scale_y_continuous(labels = scales::percent_format(),expand = expansion(add = c(0,.1)))+
  ggplot2::scale_fill_viridis_d(begin = 0, end = .8, direction = -1, option = "plasma",guide= guide_legend(reverse=T)
                                ,labels = c('TRUE' = 'Had account', 'FALSE' = 'No account')
    )+
  labs(
    title = "How often were there meetings/calls with other communities to coordinate actions in the first 3 months of invasion"
    ,fill = "War exposure"
    ,y = "Percent of respondents in each group"
    ,x = NULL
  )
```

![](figure-png-iso/comm-between-hromadas-1-1.png)<!-- -->

# 6. Evacuation

> In most rear communities, evacuation of citizens was not necessary. However, of the communities affected by war, 63% required evacuation and 36% were successful in doing so. 


```{.r .fold-hide}
d <- ds0 %>% 
  mutate(
    evacuation_fct = fct_recode(
      evacuation,
       "No need"      = "no"
      ,"Yes, and executed the evacuation"       = "yes_executed"
      ,"Yes, but did not manage to execute the evacuation" = "yes_notexecuted"
    ) %>% factor( levels = c(
       "No need"           
       ,"Yes, and executed the evacuation"
       ,"Yes, but did not manage to execute the evacuation"
    )))
    
d %>%
  group_by(evacuation_fct,occupation_and_combat_fct) %>% 
  summarize(
    hromada_count = n_distinct(hromada_code)
    ,.groups = "drop"
  ) %>% 
  group_by(occupation_and_combat_fct) %>% 
  mutate(
    prop = hromada_count/sum(hromada_count)
    ,pct = scales::percent(prop, accuracy = 1)
  ) %>% 
  ggplot(aes(y=prop, x = evacuation_fct, fill = occupation_and_combat_fct))+
  geom_col(position = position_dodge())+
  geom_text(aes(label = pct), hjust = .3, vjust = -.2, position = position_dodge(width = .9))+
  scale_y_continuous(labels = scales::percent_format(),expand = expansion(add = c(0,.1)))+
  ggplot2::scale_fill_viridis_d(begin = 0, end = .8, direction = -1, option = "plasma",guide= guide_legend(reverse=T))+
  labs(
    title = "Was there a need for an evacuation in your community?"
    ,fill = "War exposure"
    ,y = "Percent of respondents in each group"
    ,x = NULL
  )
```

![](figure-png-iso/evacuation-1.png)<!-- -->


# 7. IDP


```{.r .fold-hide}
d <- ds0 %>% filter(idp_accept == 'yes') %>%
  select(hromada_code, idp_registration_number, type)

beeswarm(d$idp_registration_number ~ d$type,  method='swarm', 
         main = 'Number of registered IDPs in community', xlab = NA, ylab = NA,
         pwcol= d$type, pch = 19, cex = 0.7, 
         horizontal = TRUE, corral = 'random')
# axis(1, at=seq(0, 200, by = 10), las=2)
bxplot(d$idp_registration_number ~ d$type, horizontal = TRUE, add = TRUE, lwd = 1.2)
```

![](figure-png-iso/idp-number-reg-1.png)<!-- -->


```{.r .fold-hide}
d <- ds0 %>% filter(idp_accept == 'yes') %>%
  select(hromada_code, idp_real_number, type)

beeswarm(d$idp_real_number ~ d$type,  method='swarm', 
         main = 'Number of all IDPs in community', xlab = NA, ylab = NA,
         pwcol= d$type, pch = 19, cex = 0.7, 
         xlim = c(0, 20000),
         horizontal = TRUE, corral = 'random', xaxt="n")
axis(1, at=seq(0, 20000, by = 2000), las=2)
bxplot(d$idp_real_number ~ d$type, horizontal = TRUE, add = TRUE, lwd = 1.2)
```

![](figure-png-iso/idp-number-real-1.png)<!-- -->


```{.r .hide}
help_idp_levels <- c('communal_placement', 'private_placement', 'regular_meal', 'humanitar_help', 
                     'fundraising', "employ", "psych_help", "law_help", "transit_center")

d <- ds0 %>% select(hromada_code, starts_with('idp_help/'), -ends_with('number')) %>% 
  pivot_longer(-hromada_code, names_to = 'help', values_to = 'count') %>%
  count(help, count) %>% group_by(help) %>% 
  mutate(freq = n/sum(n),
         help = str_remove(help, 'idp_help/')) %>%
  filter(count == 1)

p <- d %>% 
  ggplot(aes(x = fct_reorder(factor(help), freq, .desc = TRUE), y = freq)) +
  geom_col(fill = "dodgerblue") +
  geom_label(aes(label = scales::percent(freq)))  + 
  theme_bw() +
  theme(axis.title.y = element_blank(),
        axis.title.x = element_blank(),
        axis.text.x = element_text(size = 8)) +
  labs(title = 'Which actions or resources were effectively provided by the community \nfor internally displaced persons (IDPs)?', 
       subtitle = "among communities that hosted IDPs",) +
  scale_y_continuous(labels = scales::percent) +
  scale_x_discrete(labels=c('communal_placement' = "Placement of IDPs in \ncommunal or \nstate-owned \npremises",
                            'private_placement' = "Placement of IDPs in \nprivate premises", 
                            'regular_meal' = "Regular \nmeal", 
                            'humanitar_help' = "Issuance of \nhumanitarian aid", 
                            'fundraising' = "Fundraising for \nthe needs of IDPs", 
                            "employ" = "Employment \nprograms \nin the community", 
                            "psych_help" = "Psychological \nsupport for IDPs", 
                            "law_help" = "Legal \nsupport for IDPs", 
                            "transit_center" = "Organization of \na transit center"))

p
```

![](figure-png-iso/help-idp-1.png)<!-- -->


```{.r .fold-hide}
ds0 %>% 
  count(idp_help_count) %>% 
  filter(!is.na(idp_help_count)) %>% 
  mutate(freq = n/sum(n)) %>%
  ggplot(aes(x = factor(idp_help_count), y = freq)) +
  geom_bar(stat = "identity", fill = "blue") +
  geom_label(aes(label = scales::percent(freq)))  + 
  scale_y_continuous(labels = scales::percent) +
  labs(
    title = "Number of types of aid provided for the internally displaced persons (IDPs)"
    ,subtitle = "among communities that hosted IDPs"
    ,y = "Share of hromadas", x = NULL, fill = NULL
  ) 
```

![](figure-png-iso/help-idp-number-1-1.png)<!-- -->


```{.r .hide}
help_idp_levels <- c('communal_placement', 'private_placement', 'regular_meal', 'humanitar_help', 
                     'fundraising', "employ", "psych_help", "law_help", "transit_center")

d <- ds0 %>% select(hromada_code, voluntary_fct, starts_with('idp_help/'), -ends_with('number')) %>% 
  pivot_longer(-c(hromada_code, voluntary_fct), names_to = 'help', values_to = 'count') %>%
  count(help, voluntary_fct, count) %>% 
  group_by(help, voluntary_fct) %>% 
  mutate(freq = n/sum(n),
         help = str_remove(help, 'idp_help/')) %>%
  filter(count == 1)

p <- d %>% 
  ggplot(aes(x = fct_reorder(factor(help), freq, .desc = TRUE), y = freq, fill = voluntary_fct)) +
  geom_col(position = position_dodge())+
  geom_text(aes(label = scales::percent(freq, 1)), hjust = .4, 
             vjust = -.3, position = position_dodge(width = .9), size = 3.2) + 
  theme_bw() +
  theme(axis.title.y = element_blank(),
        axis.title.x = element_blank(),
        axis.text.x = element_text(size = 7)) +
  labs(title = 'Which actions or resources were effectively provided by the community \nfor internally displaced persons (IDPs)?', 
       subtitle = "excluding occupied hromadas",
       fill = NULL) +
  scale_y_continuous(labels = scales::percent) +
  scale_x_discrete(labels=c('communal_placement' = "Placement of IDPs in \ncommunal or \nstate-owned \npremises",
                            'private_placement' = "Placement of IDPs in \nprivate premises", 
                            'regular_meal' = "Regular \nmeal", 
                            'humanitar_help' = "Issuance of \nhumanitarian \naid", 
                            'fundraising' = "Fundraising for \nthe needs of IDPs", 
                            "employ" = "Employment \nprograms \nin the community", 
                            "psych_help" = "Psychological \nsupport for IDPs", 
                            "law_help" = "Legal \nsupport for IDPs", 
                            "transit_center" = "Organization of \na transit center")) +
  ggplot2::scale_fill_viridis_d(begin = 0, end = .8, direction = -1, option = "plasma",
                                guide= guide_legend(reverse=T))

p
```


```{.r .hide}
d <- ds0 %>% select(idp_place_rooms)

(d %>% make_bi_freq_graph('idp_place_rooms')) +
  labs(
    title = "Premises for how many beds were provided for IDPs?",
    subtitle = "For those hromadas that provided premises (n = 92)"
    ,y = NULL, x = NULL, fill = NULL
  ) +
  scale_x_discrete(labels=c('0_100_beds' = "0 - 100 beds",
                            '101_250_beds' = "101 - 250 beds", 
                            '251_500_beds' = "251 - 500 beds", 
                            '501_1000_beds' = "501 - 1000 beds", 
                            'over1000_beds' = "> 1000 beds")) +
  guides(fill = "none")
```

![](figure-png-iso/idp-place-rooms-1.png)<!-- -->


```{.r .hide}
d <- ds0 %>% filter(idp_accept == 'yes' & !is.na(idp_place_rooms)) %>%
  select(hromada_code, idp_registration_number, idp_place_rooms)

d %>% 
  ggplot(aes(y = idp_place_rooms, x = idp_registration_number, color = idp_place_rooms)) +
  ggbeeswarm::geom_beeswarm() +
  scale_y_discrete(labels=c('0_100_beds' = "0 - 100 beds",
                            '101_250_beds' = "101 - 250 beds", 
                            '251_500_beds' = "251 - 500 beds", 
                            '501_1000_beds' = "501 - 1000 beds", 
                            'over1000_beds' = "> 1000 beds")) +
  labs(title = "Premises for how many beds were provided for IDPs?",
       subtitle = "For those hromadas that provided premises (n = 92)",
       x = "Registered IDPs", y = NULL, color = NULL) +
  guides(color = "none")
```

![](figure-png-iso/idp-place-rooms-registered-1.png)<!-- -->


```{.r .hide}
d <- ds0 %>% filter(idp_accept == 'yes') %>%
  select(hromada_code, idp_child_share, type, occupation_and_combat_fct)

d %>% 
  ggplot(aes(x = idp_child_share, y = type, col = type)) +
  geom_boxplot(width = .3, outlier.shape = NA) +
  geom_jitter(height = .1) +
  labs(title = "Share of IDPs children gone to school to registered IDPs in the community",
       x = NULL, y = NULL, color = NULL) +
  guides(color = "none")
```

![](figure-png-iso/idp-child-share-type-1.png)<!-- -->


```{.r .hide}
d <- ds0 %>% filter(idp_accept == 'yes') %>%
  select(hromada_code, idp_child_share, type, occupation_and_combat_fct)

d %>% 
  ggplot(aes(x = idp_child_share, y = occupation_and_combat_fct, col = occupation_and_combat_fct)) +
  geom_boxplot(width = .3, outlier.shape = NA) +
  geom_jitter(height = .1) +
  labs(title = "Share of IDPs children gone to school to registered IDPs \nin the community",
       x = NULL, y = NULL, color = NULL) +
  guides(color = "none")
```

![](figure-png-iso/idp-child-share-war-1.png)<!-- -->

# 8. Economics


> It has been observed that communities, both rear and war affected, mostly have not redirected their special fund expenditures to other needs. 

> However, it was found that redirection of special fund was more frequent in rear communities, as opposed to war-exposed communities.


```{.r .fold-hide}
d <- ds0 %>% 
  mutate(
    spec_fund_relocation_fct = fct_recode(
      special_fund_relocation,
       "Yes"      = "yes"
      ,"No"       = "no"
    ) %>% factor( levels = c(
       "Yes"           
       ,"No"     
    )
    )
  )

d %>%
  group_by(spec_fund_relocation_fct,occupation_and_combat_fct) %>% 
  summarize(
    hromada_count = n_distinct(hromada_code)
    ,.groups = "drop"
  ) %>% 
  group_by(occupation_and_combat_fct) %>% 
  mutate(
    prop = hromada_count/sum(hromada_count)
    ,pct = scales::percent(prop, accuracy = 1)
  ) %>% 
  ggplot(aes(y=prop, x = spec_fund_relocation_fct, fill = occupation_and_combat_fct))+
  geom_col(position = position_dodge())+
  geom_text(aes(label = pct), hjust = .3, vjust = -.2, position = position_dodge(width = .9))+
  scale_y_continuous(labels = scales::percent_format(),expand = expansion(add = c(0,.1)))+
  ggplot2::scale_fill_viridis_d(begin = 0, end = .8, direction = -1, option = "plasma",guide= guide_legend(reverse=T))+
  labs(
    title = "Were expenditures from the special fund directed to other, current needs?"
    ,fill = "War exposure"
    ,y = "Percent of respondents in each group"
    ,x = NULL
  )
```

![](figure-png-iso/special-fund-1-1.png)<!-- -->


> Hromadas with higher level of fiscal self-sufficiency were less likely to redirect special funds for urgent needs.


```{.r .fold-hide}
(ds0 %>% filter(military_action=="no_combat") %>%
  mutate(
    `Special fund's expenditures were relocated` = case_when(special_fund_relocation=="no"~0,
                                                             special_fund_relocation=="yes"~1)
  ) %>% 
  ggplot(aes(x = own_income_prop_2021, y = `Special fund's expenditures were relocated`)) +
  geom_point() +
  geom_smooth(method = "lm", se=F) +
  theme_bw() +
  labs(title = 'Relation between financial autonomy and special fund realocation for non-combat hromadas') +
  xlab("share of own revenue in total budget in 2021"))
```

![](figure-png-iso/special-fund-own-income-1.png)<!-- -->


> The most frequently mentioned sectors for the fund to be redirected to were Defense, Utilities, Social protection and Education - these are the most likely to challenge local authorities due to war shocks.


```{.r .fold-hide}
ds0 %>% 
  filter(special_fund_relocation == "yes") %>% 
  select(hromada_name, `special_fund_relocation_needs/state_functions`:`special_fund_relocation_needs/healthcare`) %>% 
  pivot_longer(-c(hromada_name), names_to = "sector", values_to = "cut") %>% 
  mutate(
    sector = str_to_title(str_remove(sector, "sectors_"))
  ) %>% 
  group_by(sector) %>% 
  summarise(`Number of Hromadas` = sum(cut), .groups = "drop") %>% 
  filter(`Number of Hromadas` > 0)  %>% 
 mutate(sector = case_when(sector=="Special_fund_relocation_needs/Defense"~"Defense",
                           sector=="Special_fund_relocation_needs/Economic_activity"~"Economic Activity",
                           sector=="Special_fund_relocation_needs/Education"~"Education",
                           sector=="Special_fund_relocation_needs/Environment"~"Environment",
                           sector=="Special_fund_relocation_needs/Healthcare"~"Healthcare",
                           sector=="Special_fund_relocation_needs/Social_protection"~"Social Protection",
                           sector=="Special_fund_relocation_needs/Spirit_development"~"Spirit Development",
                           sector=="Special_fund_relocation_needs/State_functions"~"State Functions",
                           sector=="Special_fund_relocation_needs/Utilities"~"Utilities",
                           sector=="Special_fund_relocation_needs/Public_order"~"Public Order",
                           TRUE~sector)
 ) %>%
  ggplot(aes(x = `Number of Hromadas`, y = fct_reorder(sector, `Number of Hromadas`))) +
  geom_bar(stat = "identity") +
  theme_bw()+
  labs(
    title = "Sectors for which the funds of the special fund were redistributed"
    ,subtitle = "Data were collected during October-November of 2022"
    ,y = NULL
  )+
  geom_col(position = position_stack()
           , fill = "dodgerblue"
  )
```

![](figure-png-iso/special-fund-sectors-1.png)<!-- -->


> Hromadas did not report mass business relocation (every third mentioned no relocated companies at all). However, the most frequent destinations for relocation were naturally Western regions - Volyn, Invano-Frankivsk, Lviv, and Zakarpattia regions.


```{.r .fold-hide}
d <- ds0 %>% 
  select(relocated_companies_text) %>%
  mutate(relocated_companies = as.numeric(relocated_companies_text),
         relocated_companies_group = cut(relocated_companies,
                              breaks = c(0, 1, 2, 3, 4, 5, 6, 7, 100),
                              labels = c('None', '1', '2', '3', "4", "5", "6", "More than 6"),
                              right = T)) %>%
  na.omit()

d %>%
  ggplot(aes(x=relocated_companies_group, group = 1)) +
  geom_bar(aes(y = ..prop..), stat = 'count', fill = "dodgerblue") +
  geom_text(aes(label = scales::percent(..prop..), y = ..prop..), stat = 'count', vjust = -.4) +
  scale_y_continuous(labels = scales::percent_format(), limits = c(0, 0.4)) +
  labs(
    title = "How many enterprises were  relocated from other regions to the community as of October 2022?"
    ,y = NULL, x = NULL
  )
```

![](figure-png-iso/relocated-bussiness-1.png)<!-- -->


```{.r .fold-hide}
ds0 %>% 
  mutate(relocated_companies = as.numeric(relocated_companies_text)) %>%
      group_by(region_en,  oblast_name_en) %>% 
      summarize(`Relocated companies` = sum(relocated_companies, na.rm = TRUE)) %>% 
  filter(`Relocated companies`>0) %>%
    ggplot(aes(x = `Relocated companies`, y = fct_reorder(oblast_name_en, `Relocated companies`))) +
  geom_bar(stat = "identity") +
  theme_bw()+
  labs(
    title = "Relocated businesses by destination region"
    ,subtitle = "Data were collected during October-November of 2022"
    ,y = NULL
  )+
  geom_col(position = position_stack()
           , fill = "dodgerblue"
  )
```

![](figure-png-iso/relocated-bussiness-oblast-1.png)<!-- -->

> There is also a positive relation between fiscal autonomy and the number of relocated businesses.


```{.r .fold-hide}
ds0 %>% 
  mutate(relocated_companies = as.numeric(relocated_companies_text)) %>% 
  filter(relocated_companies>0) %>%
    ggplot(aes(y = relocated_companies, x = own_income_prop_2021 )) +
  geom_point() +
  geom_smooth(method = "lm", se=F) +
  theme_bw() +
  labs(title = 'Relation between financial autonomy and business relocation inflow') +
  xlab("share of own revenue in total budget in 2021")
```

![](figure-png-iso/relocated-companies-own-income-1.png)<!-- -->


```{.r .fold-hide}
ds0 %>% filter(military_action=="no_combat") %>%
  mutate(relocated_companies = as.numeric(relocated_companies_text)) %>% 
  filter(relocated_companies>0) %>%
    ggplot(aes(y = relocated_companies, x = prep_count )) +
  geom_point() +
  geom_smooth(method = "lm", se=F) +
  theme_bw() +
  labs(title = 'Relation between preparations and business relocation inflow')
```

![](figure-png-iso/relocated-companies-preparation-1.png)<!-- -->


```{.r .fold-hide}
(ds0 %>% 
   mutate(
    `Jobs created` = fct_recode(
      created_jobs,
       "Don't know" = "dk",
      "0-50" = "0_50_jobs",
      "51-100" = "51_100_jobs",
      "101-250" = "101_250_jobs"
    ) %>% factor( levels = c(
       "Don't know"           
       ,"0-50" ,
       "51-100",
       "101-250"
    )
    )
  ) %>%
   make_bi_freq_graph("Jobs created"))+
  labs(
    title = "How many jobs were created in the hromada thanks to the relocated enterprises?"
    ,subtitle = "Data were collected during October-November of 2022"
    ,y = NULL
  )
```

![](figure-png-iso/created-jobs-1.png)<!-- -->


```{.r .fold-hide}
ds0 %>% filter(bussiness_stimules_none == 0) %>%
  select(hromada_name, `bussiness_stimules/tax_benefits`:`bussiness_stimules/other`) %>% 
  pivot_longer(-c(hromada_name), names_to = "type", values_to = "done") %>% 
  mutate(
    type = str_to_title(str_remove(type, "types_"))
  ) %>% 
  group_by(type) %>% 
  summarise(`number of hromadas` = sum(done), .groups = "drop") %>% 
  filter(`number of hromadas`>0) %>% 
  mutate(type = case_when(type == "Bussiness_stimules/Education" ~ "Organized Educational Events",
                          type == "Bussiness_stimules/Free_rooms" ~ "Provided Premises for Free",
                          type == "Bussiness_stimules/Tax_benefits" ~ "Provided with Tax Benefits",
                          type == "Bussiness_stimules/Other" ~ "Other Methods",
  TRUE~type)
  ) %>%
  ggplot(aes(x = `number of hromadas`, y = fct_reorder(type, `number of hromadas`))) +
  geom_bar(stat = "identity", fill = "dodgerblue") +
  theme_bw()+
  geom_col(position = position_stack()
           , alpha = .5
           ,data =
  )+
  labs(
    title = "Which incentives have been used to support business in the community since February 24?"
    ,x = "Number of Hromadas", y = NULL, fill = NULL
  )+
  theme(
    panel.grid.major.y  = element_blank()
    ,panel.border = element_blank()
    ,panel.grid.major.x = element_line(color = "black")
  )
```

![](figure-png-iso/bussiness-stimules-1.png)<!-- -->

# 9. Humanitarian

> Too few answers to analyze - only 6

# 10. Reconstruction

> There are  38 communities (28% of all respondents) that had destructions from the hostilities.


```{.r .fold-hide}
ds0 %>% 
  count(region_en, is_damaged) %>% 
  ggplot(aes(x = region_en, y = n, fill = is_damaged)) +
  geom_bar(stat = "identity") +
  labs(
    title = "Were there any destructions in the hromada as a result of military actions?"
    ,y = "Number of Hromadas", x = NULL, fill = NULL
  ) +
  theme_bw()
```

![](figure-png-iso/damage-region-1.png)<!-- -->


> However, most of damaged hromadas report not mass destruction of housing of up to 10%  - 20% of all respondents.


```{.r .fold-hide}
(ds0 %>% 
   mutate(
    `Share of Housing Damaged` = as.factor(case_when(
       is_damaged == "no" ~ "No Damages",
      percent_damaged== "0_10_percent" ~ "0-10%",
      percent_damaged== "10_25_percent" ~ "10-25%",
      percent_damaged== "26_50_percent" ~ "26-50%",
      percent_damaged== "50_75_percent" ~ "50-75%",
      percent_damaged== "76_100_percent" ~ "76-100%"
    )) %>% factor( levels = c(
       "No Damages"           
       ,"0-10%" ,
       "10-25%",
       "26-50%",
       "50-75%",
       "76-100%"
    )
    )
  ) %>%
   make_bi_freq_graph("Share of Housing Damaged"))+
  labs(
    title = "What percentage of housing was damaged in your hromada?"
    ,subtitle = "Data was collected during October-November of 2022"
    ,y = "Number of communities", fill = NULL
  ) +
  guides(fill = "none")
```

![](figure-png-iso/damage-2-1.png)<!-- -->


> Some rear communities (nearly 15% of all rear communities-respondents) were also damaged - most likely due to the missile strikes.


```{.r .fold-hide}
ds0 %>% 
  count(occupation_and_combat_fct, is_damaged) %>% 
  ggplot() +
  aes(x = occupation_and_combat_fct, y=n,
             fill = is_damaged) +
  geom_bar(position = "fill", stat = "identity") +
  labs(
    title = "Were there any destructions in the hromada as a result of military actions?"
    ,y = "Share of Hromadas", x = NULL, fill = NULL
  ) +
  theme_bw()
```

![](figure-png-iso/damage-war-affected-1.png)<!-- -->


> On average, only half of hromadas with reported damages performed damage assesment for indivuduals and businesses. Less have done so for communal entities - only 38%.


```{.r .fold-hide}
d10 <- 
  ds0 %>% filter(is_damaged == "yes") %>%
  pivot_longer(
    cols = c(damage_evaluation_persons, damage_evaluation_communal, damage_evaluation_bussiness)
    ,names_to = "type_of"
    ,values_to = "response"
  ) %>% 
  group_by(type_of,response) %>% 
  summarize(
    count = n_distinct(hromada_code)
    ,.groups = "drop"
  ) %>% 
  group_by(type_of) %>% 
  mutate(
    prop = count/sum(count, rm.na = T)
    ,pct = scales::percent(prop, accuracy = 1)
  ) %>% 
  ungroup()  %>% mutate(type_of = case_when(type_of == "damage_evaluation_persons" ~ "For Individuals",
                                            type_of == "damage_evaluation_communal" ~ "For Communal Entities",
                                            type_of == "damage_evaluation_bussiness" ~ "For Businesses"))

g <- 
  d10 %>% 
  { 
  ggplot(
    data = (.) %>% mutate(response=factor(response))
    ,aes(x=prop, y = type_of, fill=response)
  )+
  geom_col(position = position_stack()
           , alpha = .7
           ,data =
  )+
  geom_text(aes(label = str_remove(pct,"\\%"))
            ,hjust = 1
            , size = 4
            ,color="black"
            ,position = position_stack()
            ,data = .
            )+
  scale_x_continuous(
    breaks = seq(.1,1,.1)
    ,labels = scales::percent_format()
    ,expand = expansion(add=c(-.000,-.0))
  )+
  labs(
    title = "Has your hromada performed damage assessment for?"
    ,x = "Percent of communities", y = NULL, fill = NULL
    
  )+
  theme(
    # panel.grid = element_blank()
    panel.grid.major.y  = element_blank()
    ,panel.border = element_blank()
    ,panel.grid.major.x = element_line(color = "grey")
  )
  }

g
```

![](figure-png-iso/damage-3-1.png)<!-- -->


> Moreover, out of hromadas with damages (and which were not occupied), only every fourth has a Recovery plan.


```{.r .fold-hide}
(ds0 %>% mutate(`Does hromada have a Recovery plan?` = as.factor(case_when(reconstruction_plan=="yes"~"Yes",
                                                                reconstruction_plan=="no"~"No"
                                                                ))) %>% filter(is_damaged=="yes") %>% make_bi_freq_graph("Does hromada have a Recovery plan?")) +
  labs(
    title = "Does your hromada have a Recovery plan?"
    ,subtitle = "among those who reported damages and were not occupied (n = 33)"
    ,y = "Number of communities", x = NULL
  ) +
  scale_x_discrete(labels=c('TRUE' = 'Yes', 'FALSE' = 'No')) +
  scale_fill_discrete(labels=c('TRUE' = 'Yes', 'FALSE' = 'No')) +
  guides(fill = "none")
```

![](figure-png-iso/damage-4-1.png)<!-- -->



```{.r .fold-hide}
ds13 <- ds0 %>% filter(!is.na(reconstruction_plan)) %>%
   mutate(
    `Share of Housing Damaged` = as.factor(case_when(
       is_damaged == "no" ~ "No Damages",
      percent_damaged== "0_10_percent" ~ "0-10%",
      percent_damaged== "10_25_percent" ~ "10-25%",
      percent_damaged== "26_50_percent" ~ "26-50%",
      percent_damaged== "50_75_percent" ~ "50-75%",
      percent_damaged== "76_100_percent" ~ "76-100%"
    )) %>% factor( levels = c(
       "No Damages"           
       ,"0-10%" ,
       "10-25%",
       "26-50%",
       "50-75%",
       "76-100%"
    )
    )
  ) %>% filter(is_damaged == "yes") %>%
  group_by(`Share of Housing Damaged`, reconstruction_plan) %>% 
  summarize(
    count = n()
    ,.groups = "drop"
  ) %>%
  ungroup() %>% group_by(`Share of Housing Damaged`) %>% mutate(
    prop = count/sum(count)
    ,pct = scales::percent(prop, accuracy = 1)
    )


g1 <- 
  ds13 %>% 
  { 
  ggplot(
    data = (.)
    ,aes(x=prop, y = `Share of Housing Damaged`, fill=reconstruction_plan)
  )+
  geom_col(position = position_stack()
           , alpha = .7
           ,data =
  )+
  geom_text(aes(label = count)
            ,hjust = 1
            , size = 3
            ,color="black"
            ,position = position_stack()
            ,data = .
            )+
  scale_x_continuous(
    breaks = seq(.1,1,.1)
    ,labels = scales::percent_format()
    ,expand = expansion(add=c(-.000,-.0))
  )+
  labs(
    title = "Does your hromada have a Recovery plan? (depending on the housing damage level)"
    ,subtitle = "among those who reported damages and were not occupied (n = 33)"
    ,x = "Percent of respondents", y = NULL, fill = NULL
    
  )+
  theme(
    # panel.grid = element_blank()
    panel.grid.major.y  = element_blank()
    ,panel.border = element_blank()
    ,panel.grid.major.x = element_line(color = "grey")
  )
  }

g1
```

![](figure-png-iso/damage-5-1.png)<!-- -->

> Housing reconstruction is being most actively performed in hromadas with relatively slight damages (50% of damaged housing has been rebuilt on average) and among those with the most mass destructions (30% of damaged housing has been rebuilt on average).


```{.r .fold-hide}
p <- ds0 %>%
  select(percent_reconstructed) %>%
  na.omit() %>%
  ggplot(aes(x=percent_reconstructed, group = 1)) +
  geom_bar(aes(y = ..prop..), stat = 'count', fill = "dodgerblue") +
  geom_text(aes(label = scales::percent(..prop..), y = ..prop..), stat = 'count', vjust = -.4, size = 3.4) +
  scale_y_continuous(labels = scales::percent_format(), limits = c(0, 0.55)) +
  scale_x_discrete(labels=c('0_10_percent' = "0-10%",
                            '10_25_percent' = "10-25%", 
                            '26_50_percent' = "26-50%", 
                            '51_75_percent' = "51-75%", 
                            '76_100_percent' = "76-100%")) +
  labs(
    title = "What part of the damaged housing stock has been repaired? (as of October 2022)",
    subtitle = "among those who reported damages and were not occupied (n = 33)"
    ,y = NULL, x = NULL
  )

p
```

![](figure-png-iso/damage-repaired-housing-1.png)<!-- -->


```r
ds_projects_type <- ds0 %>% 
  mutate(international_projects_number = as.numeric(international_projects)) %>% 
  group_by(type) %>%
  summarise(median = median(international_projects_number, na.rm =  T),
            mean = mean(international_projects_number, na.rm =  T),
            max = max(international_projects_number, na.rm =  T)) 

ds_projects_war <- ds0 %>% 
  mutate(international_projects_number = as.numeric(international_projects)) %>% 
  group_by(occupation_and_combat_fct) %>%
  summarise(median = median(international_projects_number, na.rm =  T),
            mean = mean(international_projects_number, na.rm =  T),
            max = max(international_projects_number, na.rm =  T)) 
```


```{.r .fold-hide}
d <- ds0 %>% 
  mutate(international_projects_number = as.numeric(international_projects)) %>% 
  select(hromada_full_name, international_projects_number, type, occupation_and_combat_fct) %>%
  mutate(international_projects_group = cut(international_projects_number,
                              breaks = c(0, 1, 2, 3, 4, 20),
                              labels = c('None', '1', '2', '3', 'More than 3'),
                              right = F)) %>%
  na.omit() 

p <- d %>%
  ggplot(aes(x=international_projects_group, group = 1)) +
  geom_bar(aes(y = ..prop..), stat = 'count', fill = "dodgerblue") +
  geom_text(aes(label = scales::percent(..prop..), y = ..prop..), stat = 'count', vjust = -.4) +
  scale_y_continuous(labels = scales::percent_format(), limits = c(0, 0.55)) +
  labs(
    title = "How many projects involving funds from international donors communities has implemented / is implementing since Feb 24?"
    ,y = NULL, x = NULL
  )

p
```

![](figure-png-iso/international-projects-1.png)<!-- -->

> These are hromadas in our survey that have the most implemented projects:


```{=html}
<div id="ipzijmdcev" style="padding-left:0px;padding-right:0px;padding-top:10px;padding-bottom:10px;overflow-x:auto;overflow-y:auto;width:auto;height:auto;">
<style>html {
  font-family: -apple-system, BlinkMacSystemFont, 'Segoe UI', Roboto, Oxygen, Ubuntu, Cantarell, 'Helvetica Neue', 'Fira Sans', 'Droid Sans', Arial, sans-serif;
}

#ipzijmdcev .gt_table {
  display: table;
  border-collapse: collapse;
  margin-left: auto;
  margin-right: auto;
  color: #333333;
  font-size: 16px;
  font-weight: normal;
  font-style: normal;
  background-color: #FFFFFF;
  width: auto;
  border-top-style: solid;
  border-top-width: 2px;
  border-top-color: #A8A8A8;
  border-right-style: none;
  border-right-width: 2px;
  border-right-color: #D3D3D3;
  border-bottom-style: solid;
  border-bottom-width: 2px;
  border-bottom-color: #A8A8A8;
  border-left-style: none;
  border-left-width: 2px;
  border-left-color: #D3D3D3;
}

#ipzijmdcev .gt_heading {
  background-color: #FFFFFF;
  text-align: center;
  border-bottom-color: #FFFFFF;
  border-left-style: none;
  border-left-width: 1px;
  border-left-color: #D3D3D3;
  border-right-style: none;
  border-right-width: 1px;
  border-right-color: #D3D3D3;
}

#ipzijmdcev .gt_caption {
  padding-top: 4px;
  padding-bottom: 4px;
}

#ipzijmdcev .gt_title {
  color: #333333;
  font-size: 125%;
  font-weight: initial;
  padding-top: 4px;
  padding-bottom: 4px;
  padding-left: 5px;
  padding-right: 5px;
  border-bottom-color: #FFFFFF;
  border-bottom-width: 0;
}

#ipzijmdcev .gt_subtitle {
  color: #333333;
  font-size: 85%;
  font-weight: initial;
  padding-top: 0;
  padding-bottom: 6px;
  padding-left: 5px;
  padding-right: 5px;
  border-top-color: #FFFFFF;
  border-top-width: 0;
}

#ipzijmdcev .gt_bottom_border {
  border-bottom-style: solid;
  border-bottom-width: 2px;
  border-bottom-color: #D3D3D3;
}

#ipzijmdcev .gt_col_headings {
  border-top-style: solid;
  border-top-width: 2px;
  border-top-color: #D3D3D3;
  border-bottom-style: solid;
  border-bottom-width: 2px;
  border-bottom-color: #D3D3D3;
  border-left-style: none;
  border-left-width: 1px;
  border-left-color: #D3D3D3;
  border-right-style: none;
  border-right-width: 1px;
  border-right-color: #D3D3D3;
}

#ipzijmdcev .gt_col_heading {
  color: #333333;
  background-color: #FFFFFF;
  font-size: 100%;
  font-weight: normal;
  text-transform: inherit;
  border-left-style: none;
  border-left-width: 1px;
  border-left-color: #D3D3D3;
  border-right-style: none;
  border-right-width: 1px;
  border-right-color: #D3D3D3;
  vertical-align: bottom;
  padding-top: 5px;
  padding-bottom: 6px;
  padding-left: 5px;
  padding-right: 5px;
  overflow-x: hidden;
}

#ipzijmdcev .gt_column_spanner_outer {
  color: #333333;
  background-color: #FFFFFF;
  font-size: 100%;
  font-weight: normal;
  text-transform: inherit;
  padding-top: 0;
  padding-bottom: 0;
  padding-left: 4px;
  padding-right: 4px;
}

#ipzijmdcev .gt_column_spanner_outer:first-child {
  padding-left: 0;
}

#ipzijmdcev .gt_column_spanner_outer:last-child {
  padding-right: 0;
}

#ipzijmdcev .gt_column_spanner {
  border-bottom-style: solid;
  border-bottom-width: 2px;
  border-bottom-color: #D3D3D3;
  vertical-align: bottom;
  padding-top: 5px;
  padding-bottom: 5px;
  overflow-x: hidden;
  display: inline-block;
  width: 100%;
}

#ipzijmdcev .gt_group_heading {
  padding-top: 8px;
  padding-bottom: 8px;
  padding-left: 5px;
  padding-right: 5px;
  color: #333333;
  background-color: #FFFFFF;
  font-size: 100%;
  font-weight: initial;
  text-transform: inherit;
  border-top-style: solid;
  border-top-width: 2px;
  border-top-color: #D3D3D3;
  border-bottom-style: solid;
  border-bottom-width: 2px;
  border-bottom-color: #D3D3D3;
  border-left-style: none;
  border-left-width: 1px;
  border-left-color: #D3D3D3;
  border-right-style: none;
  border-right-width: 1px;
  border-right-color: #D3D3D3;
  vertical-align: middle;
  text-align: left;
}

#ipzijmdcev .gt_empty_group_heading {
  padding: 0.5px;
  color: #333333;
  background-color: #FFFFFF;
  font-size: 100%;
  font-weight: initial;
  border-top-style: solid;
  border-top-width: 2px;
  border-top-color: #D3D3D3;
  border-bottom-style: solid;
  border-bottom-width: 2px;
  border-bottom-color: #D3D3D3;
  vertical-align: middle;
}

#ipzijmdcev .gt_from_md > :first-child {
  margin-top: 0;
}

#ipzijmdcev .gt_from_md > :last-child {
  margin-bottom: 0;
}

#ipzijmdcev .gt_row {
  padding-top: 8px;
  padding-bottom: 8px;
  padding-left: 5px;
  padding-right: 5px;
  margin: 10px;
  border-top-style: solid;
  border-top-width: 1px;
  border-top-color: #D3D3D3;
  border-left-style: none;
  border-left-width: 1px;
  border-left-color: #D3D3D3;
  border-right-style: none;
  border-right-width: 1px;
  border-right-color: #D3D3D3;
  vertical-align: middle;
  overflow-x: hidden;
}

#ipzijmdcev .gt_stub {
  color: #333333;
  background-color: #FFFFFF;
  font-size: 100%;
  font-weight: initial;
  text-transform: inherit;
  border-right-style: solid;
  border-right-width: 2px;
  border-right-color: #D3D3D3;
  padding-left: 5px;
  padding-right: 5px;
}

#ipzijmdcev .gt_stub_row_group {
  color: #333333;
  background-color: #FFFFFF;
  font-size: 100%;
  font-weight: initial;
  text-transform: inherit;
  border-right-style: solid;
  border-right-width: 2px;
  border-right-color: #D3D3D3;
  padding-left: 5px;
  padding-right: 5px;
  vertical-align: top;
}

#ipzijmdcev .gt_row_group_first td {
  border-top-width: 2px;
}

#ipzijmdcev .gt_summary_row {
  color: #333333;
  background-color: #FFFFFF;
  text-transform: inherit;
  padding-top: 8px;
  padding-bottom: 8px;
  padding-left: 5px;
  padding-right: 5px;
}

#ipzijmdcev .gt_first_summary_row {
  border-top-style: solid;
  border-top-color: #D3D3D3;
}

#ipzijmdcev .gt_first_summary_row.thick {
  border-top-width: 2px;
}

#ipzijmdcev .gt_last_summary_row {
  padding-top: 8px;
  padding-bottom: 8px;
  padding-left: 5px;
  padding-right: 5px;
  border-bottom-style: solid;
  border-bottom-width: 2px;
  border-bottom-color: #D3D3D3;
}

#ipzijmdcev .gt_grand_summary_row {
  color: #333333;
  background-color: #FFFFFF;
  text-transform: inherit;
  padding-top: 8px;
  padding-bottom: 8px;
  padding-left: 5px;
  padding-right: 5px;
}

#ipzijmdcev .gt_first_grand_summary_row {
  padding-top: 8px;
  padding-bottom: 8px;
  padding-left: 5px;
  padding-right: 5px;
  border-top-style: double;
  border-top-width: 6px;
  border-top-color: #D3D3D3;
}

#ipzijmdcev .gt_striped {
  background-color: rgba(128, 128, 128, 0.05);
}

#ipzijmdcev .gt_table_body {
  border-top-style: solid;
  border-top-width: 2px;
  border-top-color: #D3D3D3;
  border-bottom-style: solid;
  border-bottom-width: 2px;
  border-bottom-color: #D3D3D3;
}

#ipzijmdcev .gt_footnotes {
  color: #333333;
  background-color: #FFFFFF;
  border-bottom-style: none;
  border-bottom-width: 2px;
  border-bottom-color: #D3D3D3;
  border-left-style: none;
  border-left-width: 2px;
  border-left-color: #D3D3D3;
  border-right-style: none;
  border-right-width: 2px;
  border-right-color: #D3D3D3;
}

#ipzijmdcev .gt_footnote {
  margin: 0px;
  font-size: 90%;
  padding-left: 4px;
  padding-right: 4px;
  padding-left: 5px;
  padding-right: 5px;
}

#ipzijmdcev .gt_sourcenotes {
  color: #333333;
  background-color: #FFFFFF;
  border-bottom-style: none;
  border-bottom-width: 2px;
  border-bottom-color: #D3D3D3;
  border-left-style: none;
  border-left-width: 2px;
  border-left-color: #D3D3D3;
  border-right-style: none;
  border-right-width: 2px;
  border-right-color: #D3D3D3;
}

#ipzijmdcev .gt_sourcenote {
  font-size: 90%;
  padding-top: 4px;
  padding-bottom: 4px;
  padding-left: 5px;
  padding-right: 5px;
}

#ipzijmdcev .gt_left {
  text-align: left;
}

#ipzijmdcev .gt_center {
  text-align: center;
}

#ipzijmdcev .gt_right {
  text-align: right;
  font-variant-numeric: tabular-nums;
}

#ipzijmdcev .gt_font_normal {
  font-weight: normal;
}

#ipzijmdcev .gt_font_bold {
  font-weight: bold;
}

#ipzijmdcev .gt_font_italic {
  font-style: italic;
}

#ipzijmdcev .gt_super {
  font-size: 65%;
}

#ipzijmdcev .gt_footnote_marks {
  font-style: italic;
  font-weight: normal;
  font-size: 75%;
  vertical-align: 0.4em;
}

#ipzijmdcev .gt_asterisk {
  font-size: 100%;
  vertical-align: 0;
}

#ipzijmdcev .gt_indent_1 {
  text-indent: 5px;
}

#ipzijmdcev .gt_indent_2 {
  text-indent: 10px;
}

#ipzijmdcev .gt_indent_3 {
  text-indent: 15px;
}

#ipzijmdcev .gt_indent_4 {
  text-indent: 20px;
}

#ipzijmdcev .gt_indent_5 {
  text-indent: 25px;
}
</style>
<table class="gt_table">
  
  <thead class="gt_col_headings">
    <tr>
      <th class="gt_col_heading gt_columns_bottom_border gt_left" rowspan="1" colspan="1" scope="col" id="Hromada Name">Hromada Name</th>
      <th class="gt_col_heading gt_columns_bottom_border gt_right" rowspan="1" colspan="1" scope="col" id="Number of projects">Number of projects</th>
    </tr>
  </thead>
  <tbody class="gt_table_body">
    <tr><td headers="Hromada Name" class="gt_row gt_left">Тлумацька міська громада</td>
<td headers="Number of projects" class="gt_row gt_right">12</td></tr>
    <tr><td headers="Hromada Name" class="gt_row gt_left">Коломийська міська громада</td>
<td headers="Number of projects" class="gt_row gt_right">12</td></tr>
    <tr><td headers="Hromada Name" class="gt_row gt_left">Бобринецька міська громада</td>
<td headers="Number of projects" class="gt_row gt_right">9</td></tr>
    <tr><td headers="Hromada Name" class="gt_row gt_left">Покровська селищна громада</td>
<td headers="Number of projects" class="gt_row gt_right">7</td></tr>
    <tr><td headers="Hromada Name" class="gt_row gt_left">Прилуцька міська громада</td>
<td headers="Number of projects" class="gt_row gt_right">7</td></tr>
    <tr><td headers="Hromada Name" class="gt_row gt_left">Ясінянська селищна громада</td>
<td headers="Number of projects" class="gt_row gt_right">5</td></tr>
    <tr><td headers="Hromada Name" class="gt_row gt_left">Надвірнянська міська громада</td>
<td headers="Number of projects" class="gt_row gt_right">5</td></tr>
    <tr><td headers="Hromada Name" class="gt_row gt_left">Новороздільська міська громада</td>
<td headers="Number of projects" class="gt_row gt_right">5</td></tr>
    <tr><td headers="Hromada Name" class="gt_row gt_left">Лубенська міська громада</td>
<td headers="Number of projects" class="gt_row gt_right">5</td></tr>
    <tr><td headers="Hromada Name" class="gt_row gt_left">Тростянецька міська громада</td>
<td headers="Number of projects" class="gt_row gt_right">5</td></tr>
    <tr><td headers="Hromada Name" class="gt_row gt_left">Гусятинська селищна громада</td>
<td headers="Number of projects" class="gt_row gt_right">5</td></tr>
  </tbody>
  
  
</table>
</div>
```


```{.r .fold-hide}
d1 <- d %>% count(international_projects_group, type) %>%
  group_by(type) %>%
  mutate(prop = n/sum(n)
         ,pct = scales::percent(prop, accuracy = 1))

p <- d1 %>%
  ggplot() +
  aes(y = prop, x=type, fill = international_projects_group, label = pct) +
  geom_bar(stat = 'identity') +
  geom_text(position = position_fill(vjust = .4), size = 4) +
  scale_fill_viridis_d(option = "plasma") +
  scale_y_continuous(labels = scales::percent_format()) +
  labs(
    title = "How many projects involving funds from international donors communities has implemented / is implementing since Feb 24?"
    ,y = NULL, x = NULL, fill = "Number of projects"
  )

p
```

![](figure-png-iso/international-projects-type-1.png)<!-- -->


```{.r .fold-hide}
d1 <- d %>% count(international_projects_group, occupation_and_combat_fct) %>%
  group_by(occupation_and_combat_fct) %>%
  mutate(prop = n/sum(n)
         ,pct = scales::percent(prop, accuracy = 1))

p <- d1 %>%
  ggplot() +
  aes(y = prop, x=occupation_and_combat_fct, fill = international_projects_group, label = pct) +
  geom_bar(stat = 'identity') +
  geom_text(position = position_fill(vjust = .4), size = 4) +
  scale_fill_viridis_d(option = "plasma") +
  scale_y_continuous(labels = scales::percent_format()) +
  labs(
    title = "How many projects involving funds from international donors communities has implemented / is implementing since Feb 24?"
    ,y = NULL, x = NULL, fill = "Number of projects"
  )

p
```

![](figure-png-iso/international-projects-war-1.png)<!-- -->


# 11. Current Challenges

> Only 19% communities have no spending on equipment/refurbishment of shelters for schools from the community budget.


```{.r .hide}
d <- ds0 %>%
  select(hromada_code, finance_school_shelters_coded, type, occupation_and_combat_fct)

d %>%
  ggplot(aes(x = finance_school_shelters_coded, y = type, col = type)) +
  geom_boxplot(width = .3, outlier.shape = NA) +
  geom_jitter(height = .1) +
  scale_x_continuous(labels = scales::label_number(suffix = "K", scale = 1e-3)) +
  labs(title = "Money spent on the equipment/refurbishing of shelters for schools from the community budget",
       x = NULL, y = NULL, color = NULL) +
  guides(color = "none")
```

![](figure-png-iso/school-shelters-1.png)<!-- -->

> The quality of answers for questions about missed school days since February 24th, including virtual learning, has been poor . However, half of the sample, 50.5%, haven't missed any school days due to the switch to virtual learning.


```{.r .hide}
d <- ds0 %>%
  select(hromada_code, no_school_days_coded, type) %>%
  mutate(no_school_days_coded = as.numeric(no_school_days_coded))

d %>%
  ggplot(aes(x = no_school_days_coded, y = type, color = type)) +
  geom_boxplot(width = .3, outlier.shape = NA) +
  geom_jitter(height = .1) +
  labs(title = "Missed school days since Feb 24",
       x = NULL, y = NULL, color = NULL) +
  guides(color = "none")
```

![](figure-png-iso/school-days-without-1.png)<!-- -->


## 11.1 Heating season



```{.r .fold-hide}
d <- ds1_winter_prep %>% 
  select(hromada_name, all_of(prep_for_winter)) %>% 
  pivot_longer(
    -hromada_name
    ,names_to = "action"
    ,values_to = "response"
  ) %>% 
  group_by(action) %>% 
  summarise(n = sum(response, na.rm = T), .groups = "drop", na = sum(!is.na(response))) %>%
  mutate(freq = n/na)
  
d %>%
  ggplot(aes(y = freq, x = fct_reorder(action, freq, .desc = T))) +
  geom_col(fill = "dodgerblue") +
  geom_label(aes(label = scales::percent(freq)))  + 
  theme_bw() +
  theme(axis.title.y = element_blank(),
        axis.title.x = element_blank(),
        axis.text.x = element_text(size = 7)) +
  labs(title = 'Which of the following measures were implemented to prepare the community for the heating season?', 
       subtitle = "excluding occupied hromadas",) +
  scale_y_continuous(labels = scales::percent) +
  scale_x_discrete(labels=c('reserves' = "Stocks and sources of \nsolid fuel for the operation of \nboiler houses have been created",
                            'info_campaign' = "An information campaign on preparation \nof residents for the heating season \nhas been created",
                            'count_power_sources' = "Backup/autonomous power sources \nhave been counted and evaluated, \nand locations for new energy-generating \nequipment have been identified",
                            'count_heaters_need' = "The required number of electric \nheaters for low-income and vulnerable \npeople has been determined",
                            'solid_fuel_boiler' = "A plan for buying a modular solid-fuel boiler \nroom has been developed")
                   )
```

![](figure-png-iso/heating-1-1.png)<!-- -->


```{.r .fold-hide}
ds1_winter_prep %>% 
  count(winter_prep_count) %>% 
  filter(!is.na(winter_prep_count)) %>% 
  mutate(freq = n/sum(n)) %>%
  ggplot(aes(x = factor(winter_prep_count), y = freq)) +
  geom_bar(stat = "identity", fill = "blue") +
  geom_label(aes(label = scales::percent(freq)))  + 
  scale_y_continuous(labels = scales::percent) +
  labs(
    title = "Number of measures implemented to prepare hromada for the heating season"
    ,y = "Share of hromadas", x = NULL, fill = NULL
  ) 
```

![](figure-png-iso/heating-2-1.png)<!-- -->

## 11.2 Problem Involvement

```{.r .fold-hide}
d <- ds1_problem %>%
  select(hromada_code, problem_additive_index, region_en)

d_mean <- ds1_problem %>%
  group_by(region_en) %>%
  summarise(mean = mean(problem_additive_index, na.rm = T))


d %>%
  ggplot(aes(x = problem_additive_index)) +
  geom_density(fill="dodgerblue") +
  facet_wrap(region_en ~ ., nrow = 5) +
  geom_vline(data = d_mean, aes(xintercept = mean), color = "red", lwd = 1.2) +
  labs(title = "Problem Involvement Index",
       subtitle = "Red lines show mean value for community type",
       x = NULL, y = NULL, color = NULL) +
  guides(color = "none")
```

![](figure-png-iso/problem-models-1.png)<!-- -->

```{.r .fold-hide}
d %>%
  ggplot(aes(x = problem_additive_index, y = region_en, color = region_en)) +
  geom_boxplot(width = .3, outlier.shape = NA) +
  geom_jitter(height = .1) +
  labs(title = "Problem Involvement Index",
       x = NULL, y = NULL, color = NULL) +
  guides(color = "none")
```

![](figure-png-iso/problem-models-2.png)<!-- -->

```{.r .fold-hide}
d <- ds1_problem %>% select(contains('index'), region_en, hromada_code,
                            occupation_and_combat_fct, -index) %>%
  pivot_longer(-c(region_en, occupation_and_combat_fct, hromada_code), 
               names_to = 'engagement', values_to = 'index')

ds1_problem %>% select(contains('index'), -index, -problem_additive_index) %>% 
  summarise(across(everything(.), ~mean(.x, na.rm = T))) %>%
  mutate(group = "all") %>%
  pivot_longer(-group, names_to = 'engagement', values_to = "index") %>%
  mutate(engagement = factor(engagement,
                             levels = c('problem_info_index', 'problem_consultation_index',
                                        'problem_proposition_index', 'problem_system_index',
                                        'problem_feedback_index', 'problem_execution_index'))) %>%
  ggplot(aes(x = engagement, y = index, fill = engagement)) +
    geom_col()+
    geom_text(aes(label = round(index, 2)), hjust = .6, vjust = -.6) + 
    scale_y_continuous(expand = expansion(add = c(0,1))) +
    scale_x_discrete(labels=c('problem_info_index' = "Information",
                              'problem_consultation_index' = "Consultation", 
                              'problem_proposition_index' = "Involvement", 
                              'problem_system_index' = "Systematic Exchange", 
                              'problem_feedback_index' = "Feedback",
                              'problem_execution_index' = "Execution")) +
    scale_fill_viridis_d(begin = 0, end = .8, direction = -1, 
                                  option = "plasma",guide= guide_legend(reverse=T))+
  labs(
    title = "How hromadas engaged different actors in solving critical problems?"
    ,fill = NULL
    ,x = "Types of engagement"
    , y = "Index value"
  ) + 
  guides(fill = "none") 
```

![](figure-png-iso/problem-models-3.png)<!-- -->

# 12. Deoccupied hromadas


```{.r .fold-hide}
d <- ds0 %>% 
   filter(!is.na(occupation)) %>%
   mutate(
    `Share of Housing Damaged` = as.factor(case_when(
       is_damaged == "no" ~ "No Damages",
      percent_damaged== "0_10_percent" ~ "0-10%",
      percent_damaged== "10_25_percent" ~ "10-25%",
      percent_damaged== "26_50_percent" ~ "26-50%",
      percent_damaged== "50_75_percent" ~ "50-75%",
      percent_damaged== "76_100_percent" ~ "76-100%"
    )) %>% factor( levels = c(
       "No Damages"           
       ,"0-10%" ,
       "10-25%",
       "26-50%",
       "50-75%",
       "76-100%"
    ))) %>%
  group_by(`Share of Housing Damaged`,occupation_fct, .drop = F) %>% 
  summarize(
    hromada_count = n_distinct(hromada_code)
    ,.groups = "drop") %>% 
  group_by(occupation_fct) %>% 
  mutate(
    prop = hromada_count/sum(hromada_count)
    ,pct = scales::percent(prop, accuracy = 1)
  )

g <- d %>%
  ggplot(aes(y=prop, x = `Share of Housing Damaged`, fill = occupation_fct))+
  geom_col(position = position_dodge())+
  geom_text(aes(label = pct), hjust = .3, vjust = -.2, position = position_dodge(width = .9))+
  scale_y_continuous(labels = scales::percent_format(),expand = expansion(add = c(0,.1)))+
  ggplot2::scale_fill_viridis_d(begin = 0, end = .8, direction = -1, option = "plasma",guide= guide_legend(reverse=T))+
  labs(
    title = "What percentage of housing was damaged in your hromada?"
    ,fill = NULL
    ,y = "Percent of respondents in each group"
    ,x = NULL
  )

g
```

![](figure-png-iso/damage-deoccup-1.png)<!-- -->


```{.r .fold-hide}
d <- ds0 %>% 
   filter(!is.na(occupation) & !is.na(percent_reconstructed)) %>%
  group_by(percent_reconstructed,occupation_fct, .drop = F) %>% 
  summarize(
    hromada_count = n_distinct(hromada_code)
    ,.groups = "drop") %>% 
  group_by(occupation_fct) %>% 
  mutate(
    prop = hromada_count/sum(hromada_count)
    ,pct = scales::percent(prop, accuracy = 1)
  )

g <- d %>%
  ggplot(aes(y=prop, x = percent_reconstructed, fill = occupation_fct))+
  geom_col(position = position_dodge())+
  geom_text(aes(label = pct), hjust = .3, vjust = -.2, position = position_dodge(width = .9))+
  scale_y_continuous(labels = scales::percent_format(),expand = expansion(add = c(0,.1)))+
  ggplot2::scale_fill_viridis_d(begin = 0, end = .8, direction = -1, option = "plasma",guide= guide_legend(reverse=T))+
  scale_fill_discrete(labels = c("Rear communities",
                                 "Deoccupied communities")) +
    scale_x_discrete(labels=c('0_10_percent' = "0-10%",
                            '10_25_percent' = "10-25%", 
                            '26_50_percent' = "26-50%", 
                            '51_75_percent' = "51-75%", 
                            '76_100_percent' = "76-100%")) +
  labs(
    title = "What part of the damaged housing stock has been repaired? (as of October 2022)"
    ,subtitle = "among those who reported damages and were not occupied or deoccupied for a long time (n = 33)"
    ,fill = NULL
    ,y = "Percent of respondents in each group"
    ,x = NULL
  )

g
```

![](figure-png-iso/damage-deoccup-repair-1.png)<!-- -->


```{.r .hide}
d <- ds0 %>%
  select(hromada_code, finance_school_shelters_coded, occupation_fct) %>%
  mutate(finance_school_shelters_coded = as.numeric(finance_school_shelters_coded))

g <- d %>%
  filter(!is.na(finance_school_shelters_coded)) %>%
  ggplot(aes(x = finance_school_shelters_coded, y = occupation_fct, color = occupation_fct)) +
  geom_boxplot(width = .3, outlier.shape = NA) +
coord_cartesian(xlim = quantile(d$finance_school_shelters_coded, c(0, 0.9), na.rm = T)) +
  scale_x_continuous(labels = scales::label_number(suffix = "K", scale = 1e-3)) +
  geom_jitter(height = .1) +
  labs(title = "Money spent on the equipment/refurbishing of shelters for schools from the community budget",
       subtitle = "Values greater than 90th percentile removed",
       x = NULL, y = NULL, color = NULL) +
  guides(color = "none") 

g 
```

![](figure-png-iso/damage-deoccup-finance-shelter-1-1.png)<!-- -->


```{.r .hide}
d <- ds0 %>%
  filter(!is.na(occupation)) %>%
  select(hromada_name, occupation_fct, all_of(skills)) %>% 
  pivot_longer(
    -c(hromada_name,occupation_fct)
    ,names_to = "action"
    ,values_to = "response"
  ) %>% 
  group_by(action, occupation_fct) %>% 
  summarise(n = sum(response, na.rm = T), .groups = "drop", na = sum(!is.na(response))) %>%
  mutate(freq = n/na)

g <- d %>%
  ggplot(aes(y = freq, x = fct_reorder(action, freq, .desc = T), fill = occupation_fct)) +
  geom_col(position = position_dodge())+
  geom_text(aes(label = scales::percent(freq, 1)), hjust = .4, 
             vjust = -.3, position = position_dodge(width = .9), size = 3.2) + 
  theme_bw() +
  theme(axis.title.y = element_blank(),
        axis.title.x = element_blank(),
        axis.text.x = element_text(size = 7)) +
  labs(title = 'What skills do executives need to address current community challenges?', 
       fill = NULL) +
  scale_y_continuous(labels = scales::percent, limits = c(0, 0.9)) +
  scale_x_discrete(labels=c('skills_needed/fundraising' = "Fundraising \n(attracting additional funds \nto the community through \ngrants, investment projects)",
                            'skills_needed/project_management' = "Project management", 
                            'skills_needed/longterm_planning' = "Long-term planning", 
                            'skills_needed/crisis_planning' = "Crisis planning and management", 
                            'skills_needed/data_analysis' = "Collection and analysis \nof data for decision-making", 
                            "skills_needed/human_resourse" = "Lack of human resources", 
                            "skills_needed/other" = "Other")
                   )

g 
```

![](figure-png-iso/deoccup-skills-1.png)<!-- -->



```r
meta_survey %>% filter(group=="preparation") %>% pull(name)
```

```
 [1] "prep_first_aid_water"            "prep_first_aid_fuel"            
 [3] "prep_reaction_plan"              "prep_evacuation_plan"           
 [5] "prep_reaction_plan_oth_hromadas" "prep_reaction_plan_oda"         
 [7] "prep_dftg_creation"              "prep_national_resistance"       
 [9] "prep_starosta_meeting"           "prep_communal_meetiing"         
[11] "prep_online_map"                 "prep_shelter_list"              
[13] "prep_notification_check"         "prep_backup"                    
[15] "prep_partly_backup"             
```

# Session Information {#session-info}

For the sake of documentation and reproducibility, the current report was rendered in the following environment. Click the line below to expand.

<details>

<summary>Environment </summary>


```
- Session info -----------------------------------------------------------------------------------
 setting  value
 version  R version 4.2.2 (2022-10-31 ucrt)
 os       Windows 10 x64 (build 22621)
 system   x86_64, mingw32
 ui       RTerm
 language (EN)
 collate  Ukrainian_Ukraine.utf8
 ctype    Ukrainian_Ukraine.1251
 tz       Europe/Helsinki
 date     2023-02-15
 pandoc   2.19.2 @ C:/Program Files/RStudio/resources/app/bin/quarto/bin/tools/ (via rmarkdown)

- Packages ---------------------------------------------------------------------------------------
 ! package       * version date (UTC) lib source
 D archive         1.1.5   2022-05-06 [1] CRAN (R 4.2.2)
   assertthat      0.2.1   2019-03-21 [1] CRAN (R 4.2.2)
   backports       1.4.1   2021-12-13 [1] CRAN (R 4.2.0)
   beeswarm      * 0.4.0   2021-06-01 [1] CRAN (R 4.2.0)
   bit             4.0.5   2022-11-15 [1] CRAN (R 4.2.2)
   bit64           4.0.5   2020-08-30 [1] CRAN (R 4.2.2)
   broom           1.0.1   2022-08-29 [1] CRAN (R 4.2.2)
   bslib           0.4.1   2022-11-02 [1] CRAN (R 4.2.2)
   cachem          1.0.6   2021-08-19 [1] CRAN (R 4.2.2)
   callr           3.7.3   2022-11-02 [1] CRAN (R 4.2.2)
   cellranger      1.1.0   2016-07-27 [1] CRAN (R 4.2.2)
   cli             3.4.1   2022-09-23 [1] CRAN (R 4.2.2)
   codetools       0.2-18  2020-11-04 [2] CRAN (R 4.2.2)
   colorspace      2.0-3   2022-02-21 [1] CRAN (R 4.2.2)
   crayon          1.5.2   2022-09-29 [1] CRAN (R 4.2.2)
   crosstalk       1.2.0   2021-11-04 [1] CRAN (R 4.2.2)
   curl            4.3.3   2022-10-06 [1] CRAN (R 4.2.2)
   DBI             1.1.3   2022-06-18 [1] CRAN (R 4.2.2)
   dbplyr          2.2.1   2022-06-27 [1] CRAN (R 4.2.2)
   devtools        2.4.5   2022-10-11 [1] CRAN (R 4.2.2)
   dichromat     * 2.0-0.1 2022-05-02 [1] CRAN (R 4.2.0)
   digest          0.6.31  2022-12-11 [1] CRAN (R 4.2.2)
   dplyr         * 1.1.0   2023-01-29 [1] CRAN (R 4.2.2)
   DT              0.26    2022-10-19 [1] CRAN (R 4.2.2)
   ellipsis        0.3.2   2021-04-29 [1] CRAN (R 4.2.2)
   evaluate        0.18    2022-11-07 [1] CRAN (R 4.2.2)
   explore         1.0.0   2022-11-11 [1] CRAN (R 4.2.2)
   fansi           1.0.3   2022-03-24 [1] CRAN (R 4.2.2)
   farver          2.1.1   2022-07-06 [1] CRAN (R 4.2.2)
   fastDummies   * 1.6.3   2020-11-29 [1] CRAN (R 4.2.2)
   fastmap         1.1.0   2021-01-25 [1] CRAN (R 4.2.2)
   forcats       * 0.5.2   2022-08-19 [1] CRAN (R 4.2.2)
   fs              1.5.2   2021-12-08 [1] CRAN (R 4.2.2)
   gargle          1.2.1   2022-09-08 [1] CRAN (R 4.2.2)
   generics        0.1.3   2022-07-05 [1] CRAN (R 4.2.2)
   GGally          2.1.2   2021-06-21 [1] CRAN (R 4.2.2)
   ggbeeswarm      0.7.1   2022-12-16 [1] CRAN (R 4.2.2)
   ggplot2       * 3.4.0   2022-11-04 [1] CRAN (R 4.2.2)
   glue            1.6.2   2022-02-24 [1] CRAN (R 4.2.2)
   googledrive     2.0.0   2021-07-08 [1] CRAN (R 4.2.2)
   googlesheets4   1.0.1   2022-08-13 [1] CRAN (R 4.2.2)
   gridExtra       2.3     2017-09-09 [1] CRAN (R 4.2.2)
   gt            * 0.8.0   2022-11-16 [1] CRAN (R 4.2.2)
   gtable          0.3.1   2022-09-01 [1] CRAN (R 4.2.2)
   haven           2.5.1   2022-08-22 [1] CRAN (R 4.2.2)
   highr           0.9     2021-04-16 [1] CRAN (R 4.2.2)
   hms             1.1.2   2022-08-19 [1] CRAN (R 4.2.2)
   htmltools       0.5.4   2022-12-07 [1] CRAN (R 4.2.2)
   htmlwidgets     1.5.4   2021-09-08 [1] CRAN (R 4.2.2)
   httpuv          1.6.6   2022-09-08 [1] CRAN (R 4.2.2)
   httr            1.4.4   2022-08-17 [1] CRAN (R 4.2.2)
   import          1.3.0   2022-05-23 [1] CRAN (R 4.2.2)
   janitor         2.1.0   2021-01-05 [1] CRAN (R 4.2.2)
   jquerylib       0.1.4   2021-04-26 [1] CRAN (R 4.2.2)
   jsonlite        1.8.4   2022-12-06 [1] CRAN (R 4.2.2)
   kableExtra      1.3.4   2021-02-20 [1] CRAN (R 4.2.2)
   knitr         * 1.41    2022-11-18 [1] CRAN (R 4.2.2)
   labeling        0.4.2   2020-10-20 [1] CRAN (R 4.2.0)
   labelled      * 2.10.0  2022-09-14 [1] CRAN (R 4.2.2)
   later           1.3.0   2021-08-18 [1] CRAN (R 4.2.2)
   lattice         0.20-45 2021-09-22 [2] CRAN (R 4.2.2)
   lifecycle       1.0.3   2022-10-07 [1] CRAN (R 4.2.2)
   lubridate     * 1.9.0   2022-11-06 [1] CRAN (R 4.2.2)
   magrittr        2.0.3   2022-03-30 [1] CRAN (R 4.2.2)
   Matrix        * 1.5-1   2022-09-13 [2] CRAN (R 4.2.2)
   memoise         2.0.1   2021-11-26 [1] CRAN (R 4.2.2)
   mgcv            1.8-41  2022-10-21 [2] CRAN (R 4.2.2)
   mime            0.12    2021-09-28 [1] CRAN (R 4.2.0)
   miniUI          0.1.1.1 2018-05-18 [1] CRAN (R 4.2.2)
   mitools         2.4     2019-04-26 [1] CRAN (R 4.2.2)
   modelr          0.1.10  2022-11-11 [1] CRAN (R 4.2.2)
   munsell         0.5.0   2018-06-12 [1] CRAN (R 4.2.2)
   nlme            3.1-160 2022-10-10 [2] CRAN (R 4.2.2)
   pacman          0.5.1   2019-03-11 [1] CRAN (R 4.2.2)
   pillar          1.8.1   2022-08-19 [1] CRAN (R 4.2.2)
   pkgbuild        1.4.0   2022-11-27 [1] CRAN (R 4.2.2)
   pkgconfig       2.0.3   2019-09-22 [1] CRAN (R 4.2.2)
   pkgload         1.3.2   2022-11-16 [1] CRAN (R 4.2.2)
   plyr            1.8.8   2022-11-11 [1] CRAN (R 4.2.2)
   prettyunits     1.1.1   2020-01-24 [1] CRAN (R 4.2.2)
   processx        3.8.0   2022-10-26 [1] CRAN (R 4.2.2)
   profvis         0.3.7   2020-11-02 [1] CRAN (R 4.2.2)
   promises        1.2.0.1 2021-02-11 [1] CRAN (R 4.2.2)
   ps              1.7.2   2022-10-26 [1] CRAN (R 4.2.2)
   purrr         * 0.3.5   2022-10-06 [1] CRAN (R 4.2.2)
   R6              2.5.1   2021-08-19 [1] CRAN (R 4.2.2)
   ragg            1.2.4   2022-10-24 [1] CRAN (R 4.2.2)
   RColorBrewer  * 1.1-3   2022-04-03 [1] CRAN (R 4.2.0)
   Rcpp            1.0.9   2022-07-08 [1] CRAN (R 4.2.2)
   readr         * 2.1.3   2022-10-01 [1] CRAN (R 4.2.2)
   readxl        * 1.4.1   2022-08-17 [1] CRAN (R 4.2.2)
   remotes         2.4.2   2021-11-30 [1] CRAN (R 4.2.2)
   reprex          2.0.2   2022-08-17 [1] CRAN (R 4.2.2)
   reshape         0.8.9   2022-04-12 [1] CRAN (R 4.2.2)
   rlang           1.0.6   2022-09-24 [1] CRAN (R 4.2.2)
   rmarkdown       2.18    2022-11-09 [1] CRAN (R 4.2.2)
   rstudioapi      0.14    2022-08-22 [1] CRAN (R 4.2.2)
   rvest           1.0.3   2022-08-19 [1] CRAN (R 4.2.2)
   sass            0.4.4   2022-11-24 [1] CRAN (R 4.2.2)
   scales          1.2.1   2022-08-20 [1] CRAN (R 4.2.2)
   sessioninfo     1.2.2   2021-12-06 [1] CRAN (R 4.2.2)
   shiny           1.7.3   2022-10-25 [1] CRAN (R 4.2.2)
   snakecase       0.11.0  2019-05-25 [1] CRAN (R 4.2.2)
   stringi         1.7.8   2022-07-11 [1] CRAN (R 4.2.1)
   stringr       * 1.5.0   2022-12-02 [1] CRAN (R 4.2.2)
   survey        * 4.1-1   2021-07-19 [1] CRAN (R 4.2.2)
   survival      * 3.4-0   2022-08-09 [2] CRAN (R 4.2.2)
   svglite         2.1.0   2022-02-03 [1] CRAN (R 4.2.2)
   systemfonts     1.0.4   2022-02-11 [1] CRAN (R 4.2.2)
   testit          0.13    2021-04-14 [1] CRAN (R 4.2.2)
   textshaping     0.3.6   2021-10-13 [1] CRAN (R 4.2.2)
   tibble        * 3.1.8   2022-07-22 [1] CRAN (R 4.2.2)
   tidyr         * 1.2.1   2022-09-08 [1] CRAN (R 4.2.2)
   tidyselect      1.2.0   2022-10-10 [1] CRAN (R 4.2.2)
   tidyverse     * 1.3.2   2022-07-18 [1] CRAN (R 4.2.2)
   timechange    * 0.1.1   2022-11-04 [1] CRAN (R 4.2.2)
   tzdb            0.3.0   2022-03-28 [1] CRAN (R 4.2.2)
   urlchecker      1.0.1   2021-11-30 [1] CRAN (R 4.2.2)
   usethis         2.1.6   2022-05-25 [1] CRAN (R 4.2.2)
   utf8            1.2.2   2021-07-24 [1] CRAN (R 4.2.2)
   vctrs           0.5.2   2023-01-23 [1] CRAN (R 4.2.2)
   vipor           0.4.5   2017-03-22 [1] CRAN (R 4.2.2)
   viridisLite     0.4.1   2022-08-22 [1] CRAN (R 4.2.2)
   vroom           1.6.0   2022-09-30 [1] CRAN (R 4.2.2)
   webshot         0.5.4   2022-09-26 [1] CRAN (R 4.2.2)
   withr           2.5.0   2022-03-03 [1] CRAN (R 4.2.2)
   xfun            0.35    2022-11-16 [1] CRAN (R 4.2.2)
   xml2            1.3.3   2021-11-30 [1] CRAN (R 4.2.2)
   xtable          1.8-4   2019-04-21 [1] CRAN (R 4.2.2)
   yaml            2.3.6   2022-10-18 [1] CRAN (R 4.2.2)

 [1] C:/Users/Valentyn Hatsko/AppData/Local/R/win-library/4.2
 [2] C:/Program Files/R/R-4.2.2/library

 D -- DLL MD5 mismatch, broken installation.

--------------------------------------------------------------------------------------------------
```

</details>



Report rendered by Valentyn Hatsko at 2023-02-15, 16:53 +0200 in 34 seconds.
