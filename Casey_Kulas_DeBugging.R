## Casey and Kulas ran into an error with this chunk on 2/20/24
## Casey or Kulas will take a look at this .R file and debug here

#```{r getData, echo=FALSE, warning=FALSE, message=FALSE}
# Importing Data from Qualtrics.

temp <- read.csv("Qualtrics/qualtrics_pilot_data.csv", header=TRUE, na.strings="", skip = 1) %>% 
  dplyr::slice(-1) %>% 
  janitor::clean_names()

################ Cleaning up data and combining the four condition into one ################
data <- temp %>% 
  mutate(Cond1 = rowSums(is.na(temp[18:53])), # Counts how many na's occur between columns 18 to 53. Fix columns if any columns are removed during cleaning process. 
         Cond2 = rowSums(is.na(temp[54:89])),
         Cond3 = rowSums(is.na(temp[90:125])),
         Cond4 = rowSums(is.na(temp[126:161]))) %>% 
  mutate(Condition = case_when(Cond1 < 36 ~ 1, # Determing what is the condition, if they have less than 36 NA, then that is there condition. 
                               Cond2 < 36 ~ 2,
                               Cond3 < 36 ~ 3,
                               Cond4 < 36 ~ 4))

# Splitting the Conditions
split.data <- split(data, f = data$Condition)

cond1 <- split.data[["1"]] %>% 
  select(duration_in_seconds, 
         matches("c1"), # select columns that only have c1 in the name
         what_is_the_title_of_the_job_you_were_thinking_about_while_responding_to_this_survey:any_general_comments_or_reactions_you_d_care_to_share_with_the_scale_development_team, 
         Condition) %>% 
  rename_with(~substr(.,6, nchar(.)), # removes the first six characters a text
              starts_with("c", ignore.case = FALSE)) # only looks at columns that have a lower case c

cond2 <- split.data[["2"]] %>% 
  select(duration_in_seconds, matches("c2"), what_is_the_title_of_the_job_you_were_thinking_about_while_responding_to_this_survey:any_general_comments_or_reactions_you_d_care_to_share_with_the_scale_development_team, Condition) %>% 
  rename_with(~substr(.,6, nchar(.)), starts_with("c", ignore.case = FALSE))
cond3 <- split.data[["3"]] %>% 
  select(duration_in_seconds, matches("c3"), what_is_the_title_of_the_job_you_were_thinking_about_while_responding_to_this_survey:any_general_comments_or_reactions_you_d_care_to_share_with_the_scale_development_team, Condition) %>% 
  rename_with(~substr(.,6, nchar(.)), starts_with("c", ignore.case = FALSE))
cond4 <- split.data[["4"]] %>% 
  select(duration_in_seconds, matches("c4"), what_is_the_title_of_the_job_you_were_thinking_about_while_responding_to_this_survey:any_general_comments_or_reactions_you_d_care_to_share_with_the_scale_development_team, Condition) %>% 
  rename_with(~substr(.,6, nchar(.)), starts_with("c", ignore.case = FALSE))

# Reranging all columns to be in the same order. Using condition 1 ordering. 

col_order <- c("duration_in_seconds", "i_enjoy_thinking_about_work_even_when_i_m_not_at_work", "most_days_i_feel_happiest_when_the_workday_is_soon_to_be_complete", "i_am_happiest_when_i_am_immersed_in_a_project", "i_love_starting_my_workday", "i_enjoy_spending_time_completing_my_job_tasks", "most_days_i_feel_enthusiastic_about_starting_my_work_day", "i_feel_motivated_to_go_beyond_what_is_asked_of_me", "this_job_drains_my_energy", "i_am_proud_to_be_a_member_of_this_organization", "i_feel_supported_by_my_supervisor_when_i_fail_at_a_task", "i_feel_proud_of_my_accomplishments_within_this_organization", "my_job_makes_me_feel_like_i_m_part_of_something_meaningful", "i_devote_more_time_than_is_expected_of_me", "i_have_to_be_reminded_to_take_breaks_while_i_m_at_work", "i_never_miss_a_work_deadline", "i_never_allow_distractions_to_interfere_with_my_work", "when_work_is_slow_i_find_ways_to_be_productive", "i_express_enthusiasm_for_my_job_while_at_work", "i_try_my_best_to_perform_well_at_work", "if_i_notice_my_energy_level_is_low_i_take_corrective_steps_to_re_energize", "i_make_valued_contributions_to_the_organization", "i_embrace_challenging_situations_at_work", "i_speak_positively_about_this_organization_to_others", "this_organization_provides_the_resources_necessary_for_me_to_successfully_perform_my_job", "i_devote_my_full_attention_to_my_work_tasks_throughout_the_day", "thinking_about_work_saps_my_energy", "i_would_rather_direct_my_focus_toward_a_work_task_than_a_personal_task", "i_m_able_to_maintain_good_levels_of_energy_throughout_the_workday", "i_plan_my_future_with_this_company", "i_believe_this_company_cares_about_my_career_goals", "i_often_think_about_finding_another_job", "this_organization_challenges_me_to_work_at_my_full_potential", "i_m_able_to_concentrate_on_my_work_without_distractions", "i_have_a_hard_time_detaching_mentally_from_my_work", "time_passes_quickly_while_i_m_working", "i_find_it_difficult_to_mentally_disconnect_from_work", "what_is_the_title_of_the_job_you_were_thinking_about_while_responding_to_this_survey", "how_many_hours_do_you_typically_work_per_week_in_this_job", "how_long_have_you_been_with_this_organization", "any_general_comments_or_reactions_you_d_care_to_share_with_the_scale_development_team", "Condition")

cond1.red <- cond1[, col_order] # rearrange columns in order to follow condition 1
cond2.red <- cond2[, col_order]
cond3.red <- cond3[, col_order]
cond4.red <- cond4[, col_order]

together <- rbind(cond1.red, cond2.red, cond3.red, cond4.red) %>% 
  mutate(across(c("most_days_i_feel_happiest_when_the_workday_is_soon_to_be_complete","this_job_drains_my_energy","thinking_about_work_saps_my_energy","i_often_think_about_finding_another_job"), 
                ~case_when(. == 6 ~ 1, # Recoding items
                           . == 5 ~ 2,
                           . == 4 ~ 3,
                           . == 3 ~ 4,
                           . == 2 ~ 5,
                           . == 1 ~ 6)))

i <- c(1:37)
together[ , i] <- apply(together[ , i], 2,            # Specify own function within apply
                        function(x) as.numeric(as.character(x)))

#write.csv(together, "together.csv")

############## SIOPmi - Creating data frame that only has the items & Condition #########################
SIOPmi <- together %>% 
  select(i_enjoy_thinking_about_work_even_when_i_m_not_at_work:i_find_it_difficult_to_mentally_disconnect_from_work,Condition) %>% rename(
    Item_1 = i_m_able_to_concentrate_on_my_work_without_distractions,
    Item_2 = i_have_a_hard_time_detaching_mentally_from_my_work,
    Item_3 = time_passes_quickly_while_i_m_working,
    Item_4 = i_find_it_difficult_to_mentally_disconnect_from_work,
    Item_5 = i_enjoy_thinking_about_work_even_when_i_m_not_at_work,
    Item_6 = most_days_i_feel_happiest_when_the_workday_is_soon_to_be_complete,
    Item_7 = i_am_happiest_when_i_am_immersed_in_a_project,
    Item_8 = i_love_starting_my_workday,
    Item_9 = i_devote_more_time_than_is_expected_of_me,
    Item_10 = i_have_to_be_reminded_to_take_breaks_while_i_m_at_work,
    Item_11 = i_never_miss_a_work_deadline,
    Item_12 = i_never_allow_distractions_to_interfere_with_my_work,
    Item_13 = i_devote_my_full_attention_to_my_work_tasks_throughout_the_day,
    Item_14 = thinking_about_work_saps_my_energy,
    Item_15 = i_would_rather_direct_my_focus_toward_a_work_task_than_a_personal_task,
    Item_16 = i_m_able_to_maintain_good_levels_of_energy_throughout_the_workday,
    Item_17 = i_enjoy_spending_time_completing_my_job_tasks,
    Item_18 = most_days_i_feel_enthusiastic_about_starting_my_work_day,
    Item_19 = i_feel_motivated_to_go_beyond_what_is_asked_of_me,
    Item_20 = this_job_drains_my_energy,
    Item_21 = when_work_is_slow_i_find_ways_to_be_productive,
    Item_22 = i_express_enthusiasm_for_my_job_while_at_work,
    Item_23 = i_try_my_best_to_perform_well_at_work,
    Item_24 = if_i_notice_my_energy_level_is_low_i_take_corrective_steps_to_re_energize,
    Item_25 = i_plan_my_future_with_this_company,
    Item_26 = i_believe_this_company_cares_about_my_career_goals,
    Item_27 = i_often_think_about_finding_another_job,
    Item_28 = this_organization_challenges_me_to_work_at_my_full_potential,
    Item_29 = i_am_proud_to_be_a_member_of_this_organization,
    Item_30 = i_feel_supported_by_my_supervisor_when_i_fail_at_a_task,
    Item_31 = i_feel_proud_of_my_accomplishments_within_this_organization,
    Item_32 = my_job_makes_me_feel_like_i_m_part_of_something_meaningful,
    Item_33 = i_make_valued_contributions_to_the_organization,
    Item_34 = i_embrace_challenging_situations_at_work,
    Item_35 = i_speak_positively_about_this_organization_to_others,
    Item_36 = this_organization_provides_the_resources_necessary_for_me_to_successfully_perform_my_job
  ) %>% 
  select(Item_1,Item_2,Item_3,Item_4,Item_5,Item_6,Item_7,Item_8,Item_9,Item_10,Item_11,Item_12,Item_13,Item_14,Item_15,Item_16,Item_17,
         Item_18,Item_19,Item_20,Item_21,Item_22,Item_23,Item_24,Item_25,Item_26,Item_27,Item_28,Item_29,Item_30,Item_31,Item_32,Item_33,
         Item_34,Item_35,Item_36,Condition) %>% 
  mutate(across(c("Item_1":"Item_36"),~as.numeric(.)))

##### split data into four conditions #####
split_data <- split(SIOPmi, f = SIOPmi$Condition)
Cond1_Model <- split_data[["1"]]
Cond2_Model <- split_data[["2"]]
Cond3_Model <- split_data[["3"]]
Cond4_Model <- split_data[["4"]]

# Reneta's 18-Items______________________________________________________


######################################################################
######################################################################
####################### New data 10/12

new.att <- read.csv("Qualtrics/Engagement+(Attitudinal)_October+12,+2021_08.02.csv", header=TRUE, na.strings="", skip = 1) %>% 
  dplyr::slice(-1) %>% # Removing the first row 
  janitor::clean_names() # Cleaning column names

new.sub <- read.csv("Qualtrics/Engagement+(Substantive)_October+12,+2021_08.01.csv", header=TRUE, na.strings="", skip = 1) %>% 
  dplyr::slice(-1) %>% 
  janitor::clean_names()

newdata.att <- new.att %>% 
  select(i_am_able_to_concentrate_on_my_work_without_getting_distracted:i_speak_positively_about_this_organization_to_others)

newdata.sub <- new.sub %>% 
  select(i_am_able_to_concentrate_on_my_work_without_getting_distracted:i_speak_positively_about_this_organization_to_others)


names(newdata.att) <- c("Item_1","Item_3","Item_4","Item_14","Item_16","Item_25","Item_26","Item_28","Item_5","Item_8","Item_17","Item_19","Item_31","Item_32","Item_10","Item_11","Item_21","Item_22","Item_34","Item_35") # creating new names for columns

names(newdata.sub) <- c("Item_1","Item_3","Item_4","Item_5","Item_8","Item_10","Item_11","Item_14","Item_16","Item_17","Item_19","Item_21","Item_22","Item_25","Item_26","Item_28","Item_31","Item_32","Item_34","Item_35")

use.att <- newdata.att[,c(1,2,9,10,15,16,4,5,11,12,17,18,7,8,13,14,19,20)]
use.sub <- newdata.sub[,-c(3,14)]
use.att$Condition <- "Cond5"
use.sub$Condition <- "Cond6"

siop.pilot <- SIOPmi[,c(1,3,5,8,10,11,14,16,17,19,21,22,26,28,31,32,34,35,37)] # selecting items that are related to the 18 items from Reneta's survey

siop <- as.data.frame(rbind(siop.pilot,use.att,use.sub)) %>% 
  mutate(across(c("Item_1":"Item_35"),~as.numeric(.))) %>% # Change items to numeric value
  mutate(Item_14 = 7 - Item_14) # Recoding variables 
