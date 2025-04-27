# --- Step 1 (FINAL VERSION - EXCLUDING 'EA' TOI ENTIRELY) ---

# Make sure dplyr is loaded
library(dplyr)

# Define TOI lists *WITHOUT* EA
# All TOIs relevant to this analysis pipeline, EXCLUDING EA
relevant_tois <- c(
  "FPEn", "FPEn2", "FPEn3", "FPEn4", # Before Enemy Elim
  "FPEt",                           # Before Player Elim by Enemy
  "PEn", "PEn2", "PEn3", "PEn4",    # After Player Elim by NME?
  "PEt"                             # After Player Elim by Enemy?
)
# TOIs representing time BEFORE the key event time, EXCLUDING EA
before_event_tois <- c("FPEn", "FPEn2", "FPEn3", "FPEn4", "FPEt")
# TOIs representing time AFTER the key event time (Unchanged)
after_event_tois <- c("PEn", "PEn2", "PEn3", "PEn4", "PEt")

print("--- Starting Step 1: Processing data and EXCLUDING 'EA' TOI ---")
# Start with the ORIGINAL dataframe 'ds2' (make sure it's the one with corrected Rine data)
if (!exists("ds2")) {
  stop("Error: Original dataframe 'ds2' not found. Please load your data.")
}

ds2_processed_revised <- ds2 %>%
  # Group by the identifiers (incl. TOI) to find key event info *per TOI label* initially
  group_by(Recording, Participant, TOI, Interval) %>%
  # Find key event info using temporary names
  mutate(
    .key_indices = list(which(Start <= 5000 & Stop > 5000)),
    .key_event_found_for_toi = length(.key_indices[[1]]) > 0,
    .key_event_Start_for_toi = dplyr::if_else(.key_event_found_for_toi, Start[.key_indices[[1]][1]], NA_real_),
    .key_event_Stop_for_toi = dplyr::if_else(.key_event_found_for_toi, Stop[.key_indices[[1]][1]], NA_real_),
    .key_event_Type_for_toi = dplyr::if_else(.key_event_found_for_toi, Event_type[.key_indices[[1]][1]], NA_character_)
  ) %>%
  ungroup() %>%
  # Group by the core interval identifier
  group_by(Recording, Participant, Interval) %>%
  # Check if key event was found for *any* TOI label in this interval
  # Propagate a consistent Key Event Start/Stop time for the whole interval
  mutate(
    .any_key_event_found = any(.key_event_found_for_toi),
    .group_KeyEventStart = first(na.omit(.key_event_Start_for_toi)),
    .group_KeyEventStop = first(na.omit(.key_event_Stop_for_toi)),
    .group_KeyEventType = first(na.omit(.key_event_Type_for_toi[which(!is.na(.key_event_Start_for_toi))]))
  ) %>%
  # Keep only intervals where a key event was found under at least one TOI
  filter(.any_key_event_found) %>%
  ungroup() %>%
  # --- Apply row-wise filters ---
  # Filter for only the relevant TOIs (which NOW EXCLUDES EA)
  filter(TOI %in% relevant_tois) %>%
  # Filter for Validity
  filter(Validity == "Whole") %>%
  # Apply the conditional time window filter (using group-wide times)
  filter(
    (TOI %in% before_event_tois & Stop <= .group_KeyEventStop) |
      (TOI %in% after_event_tois & Start >= .group_KeyEventStart)
  ) %>%
  # --- Final steps: Rename and Select ---
  rename(
    KeyEventStart = .group_KeyEventStart,
    KeyEventStop = .group_KeyEventStop,
    KeyEventType = .group_KeyEventType
  ) %>%
  # Select desired columns
  # Make sure original ds2 columns don't already include KeyEventStart etc.
  select(any_of(colnames(ds2)), KeyEventStart, KeyEventStop, KeyEventType) %>%
  # Remove any remaining temporary helper columns
  select(-any_of(c(".key_indices", ".key_event_found_for_toi", ".any_key_event_found",
                   ".key_event_Start_for_toi", ".key_event_Stop_for_toi", ".key_event_Type_for_toi")))

# --- End of Step 1 (EXCLUDING EA) ---

# --- Checks ---
print(paste("Original rows:", nrow(ds2)))
print(paste("Processed rows (Step 1 - EA Excluded):", nrow(ds2_processed_revised)))
print("Remaining TOIs after Step 1 (EA should be gone):")
print(sort(unique(ds2_processed_revised$TOI))) # EA should not be listed here

print(paste("Processed rows (Corrected Step 1):", nrow(ds2_processed_revised)))
print("Remaining TOIs after Step 1:")
print(sort(unique(ds2_processed_revised$TOI)))
print("Unique Validity values remaining:")
print(unique(ds2_processed_revised$Validity))
key_time_consistency <- ds2_processed_revised %>%
  group_by(Recording, Participant, Interval) %>%
  summarise(
    N_Unique_Start = n_distinct(KeyEventStart, na.rm = TRUE),
    N_Unique_Stop = n_distinct(KeyEventStop, na.rm = TRUE),
    .groups = 'drop'
  ) %>%
  filter(N_Unique_Start > 1 | N_Unique_Stop > 1)

print("Checking consistency of KeyEventStart/Stop within core intervals:")
if(nrow(key_time_consistency) > 0) {
  print("Inconsistencies found for:")
  print(key_time_consistency)
} else {
  print("No inconsistencies found. KeyEventStart/Stop are consistent within each Recording/Participant/Interval group.")
}
# --- !! Replace placeholders with actual values from your data !! ---
check_rec <- "Luk2"
check_part <- "LTest2"
check_int <- 2
check_toi_before <- "FPEn" # Or EA, FPEn2, FPEt etc.

spot_check_before <- ds2_processed_revised %>%
  filter(Recording == check_rec, Participant == check_part, Interval == check_int, TOI == check_toi_before)

if(nrow(spot_check_before) > 0) {
  max_stop_time <- max(spot_check_before$Stop, na.rm = TRUE)
  key_stop_time <- spot_check_before$KeyEventStop[1] # Should be same for all rows in group
  print(paste("Spot Check (BEFORE):", check_rec, check_part, check_int, check_toi_before))
  print(paste("  Max Stop Time found:", max_stop_time))
  print(paste("  KeyEventStop:", key_stop_time))
  print(paste("  Is Max Stop <= KeyEventStop?", max_stop_time <= key_stop_time)) # Should be TRUE
} else {
  print(paste("Spot check failed: No data found for", check_rec, check_part, check_int, check_toi_before))
}

# --- !! Replace placeholders with actual values from your data !! ---
check_rec_after <- "Rog2"
check_part_after <- "Roger"
check_int_after <- 2
check_toi_after <- "PEn" # Or PEn2, PEt etc.

spot_check_after <- ds2_processed_revised %>%
  filter(Recording == check_rec_after, Participant == check_part_after, Interval == check_int_after, TOI == check_toi_after)

if(nrow(spot_check_after) > 0) {
  min_start_time <- min(spot_check_after$Start, na.rm = TRUE)
  key_start_time <- spot_check_after$KeyEventStart[1] # Should be same for all rows in group
  print(paste("Spot Check (AFTER):", check_rec_after, check_part_after, check_int_after, check_toi_after))
  print(paste("  Min Start Time found:", min_start_time))
  print(paste("  KeyEventStart:", key_start_time))
  print(paste("  Is Min Start >= KeyEventStart?", min_start_time >= key_start_time)) # Should be TRUE
} else {
  print(paste("Spot check failed: No data found for", check_rec_after, check_part_after, check_int_after, check_toi_after))
}

str(ds2_processed_revised)
head(ds2_processed_revised)

# # Optional: Check the number of rows and remaining TOIs
# print(paste("Original rows:", nrow(ds2)))
# print(paste("Processed rows (Corrected Step 1):", nrow(ds2_processed_revised)))
# print("Remaining TOIs after Step 1:")
# print(sort(unique(ds2_processed_revised$TOI)))
# # Check if KeyEventStart/Stop exist and have values
# # print(summary(ds2_processed_revised$KeyEventStart))
# # print(summary(ds2_processed_revised$KeyEventStop))
# # Optional: Check the number of rows and remaining TOIs
# print(paste("Original rows:", nrow(ds2)))
# print(paste("Processed rows (Adjusted Step 1):", nrow(ds2_processed_revised)))
# print("Remaining TOIs after Step 1:")
# print(unique(ds2_processed_revised$TOI))

# --- Re-run Step 2: Unified Temporal Sequencing ---

# Make sure libraries are loaded
library(dplyr)
library(ggplot2) # Although not used here, good to have for subsequent plotting

# Start with the validated result from Step 1
# Input: ds2_processed_revised
# Output: ds2_sequenced_unified
ds2_sequenced_unified <- ds2_processed_revised %>%
  # Arrange by time within groups
  arrange(Recording, Participant, TOI, Interval, Start) %>%
  # Group by the CORRECTED identifiers
  group_by(Recording, Participant, TOI, Interval) %>%
  mutate(
    # --- Calculate Unified FixationSequence ---
    FixationSequence = {
      fix_seq <- rep(NA_integer_, n())
      is_fix <- Event_type == "Fixation"
      is_key_event_row <- (Start == KeyEventStart & Stop == KeyEventStop)
      key_fix_indices <- which(is_fix & is_key_event_row)
      fix_before_indices <- which(is_fix & Stop <= KeyEventStart & !is_key_event_row)
      fix_after_indices <- which(is_fix & Start > KeyEventStart & !is_key_event_row)
      if (length(fix_before_indices) > 0) {
        fix_seq[fix_before_indices] <- -rank(-Stop[fix_before_indices], ties.method = "first")
      }
      if (length(fix_after_indices) > 0) {
        fix_seq[fix_after_indices] <- rank(Start[fix_after_indices], ties.method = "first")
      }
      if (length(key_fix_indices) > 0) {
        fix_seq[key_fix_indices] <- 0
      }
      fix_seq
    }, # End FixationSequence
    
    # --- Calculate Unified SaccadeSequence ---
    SaccadeSequence = {
      sacc_seq <- rep(NA_integer_, n())
      is_sacc <- Event_type == "Saccade"
      is_key_event_row <- (Start == KeyEventStart & Stop == KeyEventStop)
      key_sacc_indices <- which(is_sacc & is_key_event_row)
      sacc_before_indices <- which(is_sacc & Stop <= KeyEventStart & !is_key_event_row)
      sacc_after_indices <- which(is_sacc & Start > KeyEventStart & !is_key_event_row)
      if (length(sacc_before_indices) > 0) {
        sacc_seq[sacc_before_indices] <- -rank(-Stop[sacc_before_indices], ties.method = "first")
      }
      if (length(sacc_after_indices) > 0) {
        sacc_seq[sacc_after_indices] <- rank(Start[sacc_after_indices], ties.method = "first")
      }
      if (length(key_sacc_indices) > 0) {
        sacc_seq[key_sacc_indices] <- 0
      }
      sacc_seq
    } # End SaccadeSequence
  ) %>%
  ungroup()

# Optional: Check sequence results
# print(head(ds2_sequenced_unified %>% select(Recording, Participant, TOI, Interval, Start, Event_type, FixationSequence, SaccadeSequence), 40))
# summary(ds2_sequenced_unified$FixationSequence)
# summary(ds2_sequenced_unified$SaccadeSequence)

# --- End of Re-run Step 2 ---

# # Optional: Check the result
# print(head(ds2_sequenced_unified %>% select(Recording, Participant, TOI, Interval, Start, Stop, Event_type, KeyEventStart, KeyEventStop, FixationSequence, SaccadeSequence), 40))
# summary(ds2_sequenced_unified$FixationSequence)
# summary(ds2_sequenced_unified$SaccadeSequence)

# --- Re-run Step 3: Filter by row count ---

# Input: ds2_sequenced_unified (from re-running Step 2)
# Output: ds2_final_processed (the final cleaned dataset)
ds2_final_processed <- ds2_sequenced_unified %>%
  # Group by the CORRECTED identifiers to count rows within each processed interval/TOI group
  group_by(Recording, Participant, TOI, Interval) %>%
  # Add a temporary column with the count of rows in each group
  add_count(name = "interval_row_count") %>%
  # Keep only those groups where the count is 10 or more
  filter(interval_row_count >= 10) %>%
  # Optional: remove the temporary count column
  select(-interval_row_count) %>%
  # Ungroup
  ungroup()

# --- End of Re-run Step 3 ---

# Check final counts
print(paste("Rows after sequencing:", nrow(ds2_sequenced_unified)))
print(paste("Rows after row count filter:", nrow(ds2_final_processed)))

processed_intervals_seq <- ds2_sequenced_unified %>% distinct(Recording, Participant, TOI, Interval)
processed_intervals_final <- ds2_final_processed %>% distinct(Recording, Participant, TOI, Interval)
print(paste("Intervals/TOIs before count filter:", nrow(processed_intervals_seq)))
print(paste("Intervals/TOIs after count filter:", nrow(processed_intervals_final)))

# View the final data structure
# str(ds2_final_processed)
# Check final counts to see the effect of this filter
print(paste("Rows before count filter:", nrow(ds2_sequenced_unified)))
print(paste("Rows after count filter:", nrow(ds2_final_processed)))

# Check how many intervals remain
processed_intervals_seq <- ds2_sequenced_unified %>% distinct(Recording, Participant, TOI, Interval)
processed_intervals_final <- ds2_final_processed %>% distinct(Recording, Participant, TOI, Interval)
print(paste("Intervals before count filter:", nrow(processed_intervals_seq)))
print(paste("Intervals after count filter:", nrow(processed_intervals_final)))

# View the first few rows of the final processed data
# head(ds2_final_processed)
# str(ds2_final_processed)
# 
# 
# print("Summary of Duration for Fixations:")
# summary(ds2_final_processed$Duration[ds2_final_processed$Event_type == "Fixation"])
# print(paste("Number of NA Durations for Fixations:", sum(is.na(ds2_final_processed$Duration[ds2_final_processed$Event_type == "Fixation"]))))
# 
# print("Summary of Saccade_amplitude for Saccades:")
# summary(ds2_final_processed$Saccade_amplitude[ds2_final_processed$Event_type == "Saccade"])
# print(paste("Number of NA Saccade_amplitudes for Saccades:", sum(is.na(ds2_final_processed$Saccade_amplitude[ds2_final_processed$Event_type == "Saccade"]))))
# 
# print("Unique Participants remaining:")
# print(unique(ds2_final_processed$Participant))
# print("Unique TOIs remaining:")
# print(unique(ds2_final_processed$TOI))
# # See how many intervals per participant/TOI combination
# print("Count of intervals per Participant/TOI:")
# print(ds2_final_processed %>% distinct(Recording, Participant, TOI, Interval) %>% count(Recording, Participant, TOI), n = 40)
# 

############################################################################################################################################################################################################################## C A L C U L A T I O N S  

# --- Calculation 1: Averages per specific Recording, Participant, and TOI ---

# Make sure dplyr is loaded
library(dplyr)

# Calculate Average Fixation Duration
fixation_summary_per_toi <- ds2_final_processed %>%
  filter(Event_type == "Fixation") %>%
  group_by(Recording, Participant, TOI) %>%
  summarise(
    AvgFixationDuration = mean(Duration, na.rm = TRUE),
    N_Fixations = n(),
    .groups = 'drop'
  )

# Calculate Average Saccade Amplitude
saccade_summary_per_toi <- ds2_final_processed %>%
  filter(Event_type == "Saccade") %>%
  group_by(Recording, Participant, TOI) %>%
  summarise(
    AvgSaccadeAmplitude = mean(Saccade_amplitude, na.rm = TRUE),
    N_Saccades = n(),
    .groups = 'drop'
  )

# Optional: Join results
final_summary_per_toi <- fixation_summary_per_toi %>%
  full_join(saccade_summary_per_toi, by = c("Recording", "Participant", "TOI"))

# Display results (shows detail for each specific TOI like FPEn, FPEn2...)
print("--- Summary: Averages per Recording, Participant, and specific TOI ---")
# print(final_summary_per_toi, n = 50) # Print more rows if needed
head(final_summary_per_toi, n = 50)

# --- End of Calculation 1 ---


# --- Calculation 2: Combined Averages for FPEn1-4 ---

# Define the TOIs to combine
all_fpen_tois <- c("FPEn", "FPEn2", "FPEn3", "FPEn4")

# Calculate combined averages
combined_fpen_summary <- ds2_final_processed %>%
  # Filter for the relevant TOIs
  filter(TOI %in% all_fpen_tois) %>%
  # Group by Recording and Participant only
  group_by(Recording, Participant) %>%
  # Summarise across the combined TOIs
  summarise(
    AvgFixDuration_All_FPEn = mean(Duration[Event_type == "Fixation"], na.rm = TRUE),
    N_Fix_All_FPEn = sum(Event_type == "Fixation"),
    AvgSaccAmplitude_All_FPEn = mean(Saccade_amplitude[Event_type == "Saccade"], na.rm = TRUE),
    N_Sacc_All_FPEn = sum(Event_type == "Saccade"),
    .groups = 'drop'
  )

# Display results
print("--- Summary: Combined Averages for all FPEn TOIs (FPEn1-4) per Rec/Part ---")
# print(combined_fpen_summary, n = 50)
head(combined_fpen_summary)

# --- End of Calculation 2 ---


# --- Calculation 3: Combined Averages for PEn1-4 ---

# Define the TOIs to combine
all_pen_tois <- c("PEn", "PEn2", "PEn3", "PEn4")

# Calculate combined averages
combined_pen_summary <- ds2_final_processed %>%
  # Filter for the relevant TOIs
  filter(TOI %in% all_pen_tois) %>%
  # Group by Recording and Participant only
  group_by(Recording, Participant) %>%
  # Summarise across the combined TOIs
  summarise(
    AvgFixDuration_All_PEn = mean(Duration[Event_type == "Fixation"], na.rm = TRUE),
    N_Fix_All_PEn = sum(Event_type == "Fixation"),
    AvgSaccAmplitude_All_PEn = mean(Saccade_amplitude[Event_type == "Saccade"], na.rm = TRUE),
    N_Sacc_All_PEn = sum(Event_type == "Saccade"),
    .groups = 'drop'
  )

# Display results
print("--- Summary: Combined Averages for all PEn TOIs (PEn1-4) per Rec/Part ---")
# print(combined_pen_summary, n = 50)
head(combined_pen_summary)

# --- End of Calculation 3 ---


# --- Calculation 4: Combined Averages for Before vs After Event TOIs ---

# Define the groups based on your previous confirmation
all_before_tois_combined <- c("EA", "FPEn", "FPEn2", "FPEn3", "FPEn4", "FPEt")
all_after_tois_combined <- c("PEn", "PEn2", "PEn3", "PEn4", "PEt")

# Calculate averages per group
before_after_summary <- ds2_final_processed %>%
  # Keep only rows belonging to these defined groups
  filter(TOI %in% c(all_before_tois_combined, all_after_tois_combined)) %>%
  # Create the grouping variable based on TOI lists
  mutate(TimeWindowGroup = case_when(
    TOI %in% all_before_tois_combined ~ "BeforeEvent",
    TOI %in% all_after_tois_combined ~ "AfterEvent"
  )) %>%
  # Group by Recording, Participant, and the new TimeWindowGroup
  group_by(Recording, Participant, TimeWindowGroup) %>%
  # Summarise within these groups
  summarise(
    AvgFixDuration = mean(Duration[Event_type=="Fixation"], na.rm = TRUE),
    N_Fix = sum(Event_type=="Fixation"),
    AvgSaccAmplitude = mean(Saccade_amplitude[Event_type=="Saccade"], na.rm = TRUE),
    N_Sacc = sum(Event_type=="Saccade"),
    .groups = 'drop'
  ) %>%
  # Arrange for easier comparison
  arrange(Recording, Participant, TimeWindowGroup)

# Display results
print("--- Summary: Combined Averages for Before vs After Event TOIs per Rec/Part ---")
# print(before_after_summary, n = 50)
head(before_after_summary)

# --- End of Calculation 4 ---


# --- Calculation 5: Summary for FPEt (per Recording, Participant) ---

# Make sure dplyr is loaded
library(dplyr)

# Assuming ds2_final_processed is the up-to-date final dataset

combined_fpet_summary <- ds2_final_processed %>%
  # Filter specifically for the FPEt TOI
  filter(TOI == "FPEt") %>%
  # Group by Recording and Participant
  group_by(Recording, Participant) %>%
  # Summarise metrics within this group
  summarise(
    AvgFixDuration_FPEt = mean(Duration[Event_type == "Fixation"], na.rm = TRUE),
    N_Fix_FPEt = sum(Event_type == "Fixation"),
    AvgSaccAmplitude_FPEt = mean(Saccade_amplitude[Event_type == "Saccade"], na.rm = TRUE),
    N_Sacc_FPEt = sum(Event_type == "Saccade"),
    .groups = 'drop' # Drop grouping after summarise
  )

# --- Calculation 6: Summary for PEt (per Recording, Participant) ---

combined_pet_summary <- ds2_final_processed %>%
  # Filter specifically for the PEt TOI
  filter(TOI == "PEt") %>%
  # Group by Recording and Participant
  group_by(Recording, Participant) %>%
  # Summarise metrics within this group
  summarise(
    AvgFixDuration_PEt = mean(Duration[Event_type == "Fixation"], na.rm = TRUE),
    N_Fix_PEt = sum(Event_type == "Fixation"),
    AvgSaccAmplitude_PEt = mean(Saccade_amplitude[Event_type == "Saccade"], na.rm = TRUE),
    N_Sacc_PEt = sum(Event_type == "Saccade"),
    .groups = 'drop' # Drop grouping after summarise
  )

# --- Display Results ---
print("--- Summary: Averages for FPEt per Rec/Part ---")
# print(fpet_summary, n = Inf) # Uncomment to print all rows
head(fpet_summary)

print("--- Summary: Averages for PEt per Rec/Part ---")
# print(pet_summary, n = Inf) # Uncomment to print all rows
head(pet_summary)

# --- End of FPEt/PEt Summaries ---


# --- Calculate Overall Averages for Four TOI Categories ---

# Make sure dplyr is loaded
library(dplyr)

# Define TOI lists again for clarity and calculation
all_fpen_tois <- c("FPEn", "FPEn2", "FPEn3", "FPEn4")
all_pen_tois <- c("PEn", "PEn2", "PEn3", "PEn4")

# --- Calculate for Combined FPEnX ---
summary_overall_fpenx <- ds2_final_processed %>%
  filter(TOI %in% all_fpen_tois) %>%
  # Group only by Event_type to get overall average
  group_by(Event_type) %>%
  summarise(
    # Calculate metric only for the relevant Event_type
    AvgDuration = if_else(Event_type[1] == "Fixation", mean(Duration, na.rm = TRUE), NA_real_),
    AvgAmplitude = if_else(Event_type[1] == "Saccade", mean(Saccade_amplitude, na.rm = TRUE), NA_real_),
    N = n(), # Total count of fixations or saccades in this category
    .groups = 'drop'
  ) %>%
  mutate(TOI_Category = "Combined_FPEnX") %>%
  # Remove rows where the primary metric is NA (e.g., AvgAmplitude for Fixation rows)
  filter(!(Event_type == "Fixation" & is.na(AvgDuration)), !(Event_type == "Saccade" & is.na(AvgAmplitude)))

# --- Calculate for Combined PEnX ---
summary_overall_penx <- ds2_final_processed %>%
  filter(TOI %in% all_pen_tois) %>%
  group_by(Event_type) %>%
  summarise(
    AvgDuration = if_else(Event_type[1] == "Fixation", mean(Duration, na.rm = TRUE), NA_real_),
    AvgAmplitude = if_else(Event_type[1] == "Saccade", mean(Saccade_amplitude, na.rm = TRUE), NA_real_),
    N = n(),
    .groups = 'drop'
  ) %>%
  mutate(TOI_Category = "Combined_PEnX") %>%
  filter(!(Event_type == "Fixation" & is.na(AvgDuration)), !(Event_type == "Saccade" & is.na(AvgAmplitude)))

# --- Calculate for FPEt ---
summary_overall_fpet <- ds2_final_processed %>%
  filter(TOI == "FPEt") %>%
  group_by(Event_type) %>%
  summarise(
    AvgDuration = if_else(Event_type[1] == "Fixation", mean(Duration, na.rm = TRUE), NA_real_),
    AvgAmplitude = if_else(Event_type[1] == "Saccade", mean(Saccade_amplitude, na.rm = TRUE), NA_real_),
    N = n(),
    .groups = 'drop'
  ) %>%
  mutate(TOI_Category = "FPEt") %>%
  filter(!(Event_type == "Fixation" & is.na(AvgDuration)), !(Event_type == "Saccade" & is.na(AvgAmplitude)))

# --- Calculate for PEt ---
summary_overall_pet <- ds2_final_processed %>%
  filter(TOI == "PEt") %>%
  group_by(Event_type) %>%
  summarise(
    AvgDuration = if_else(Event_type[1] == "Fixation", mean(Duration, na.rm = TRUE), NA_real_),
    AvgAmplitude = if_else(Event_type[1] == "Saccade", mean(Saccade_amplitude, na.rm = TRUE), NA_real_),
    N = n(),
    .groups = 'drop'
  ) %>%
  mutate(TOI_Category = "PEt") %>%
  filter(!(Event_type == "Fixation" & is.na(AvgDuration)), !(Event_type == "Saccade" & is.na(AvgAmplitude)))

# --- Combine results into one table ---
overall_summary_four_categories <- bind_rows(
  summary_overall_fpenx,
  summary_overall_penx,
  summary_overall_fpet,
  summary_overall_pet
) %>%
  # Select and arrange columns for final presentation
  select(TOI_Category, Event_type, AvgDuration, AvgAmplitude, N) %>%
  arrange(TOI_Category, Event_type)

# --- Display the final summary table ---
print("--- Overall Averages for Four TOI Categories ---")
print(overall_summary_four_categories)

# --- End of Overall Averages Calculation ---

# --- Calculate Averages for New TOI Combinations ---

# Make sure dplyr is loaded
library(dplyr)

# Define the TOI lists for the new combinations
# Assuming the X goes up to 4 based on previous context
fpenx_plus_penx_tois <- c("FPEn", "FPEn2", "FPEn3", "FPEn4", "PEn", "PEn2", "PEn3", "PEn4")
fpet_plus_pet_tois <- c("FPEt", "PEt")


# --- Combination 1: FPEnX + PEnX ---

# 1a. Averages per Recording & Participant
summary_fpenx_penx_per_participant <- ds2_final_processed %>%
  filter(TOI %in% fpenx_plus_penx_tois) %>%
  group_by(Recording, Participant) %>%
  summarise(
    AvgFixDuration = mean(Duration[Event_type == "Fixation"], na.rm = TRUE),
    N_Fix = sum(Event_type == "Fixation"),
    AvgSaccAmplitude = mean(Saccade_amplitude[Event_type == "Saccade"], na.rm = TRUE),
    N_Sacc = sum(Event_type == "Saccade"),
    .groups = 'drop'
  )

# 1b. Averages Across All Participants
summary_fpenx_penx_overall <- ds2_final_processed %>%
  filter(TOI %in% fpenx_plus_penx_tois) %>%
  group_by(Event_type) %>%
  summarise(
    AvgDuration = if_else(Event_type[1] == "Fixation", mean(Duration, na.rm = TRUE), NA_real_),
    AvgAmplitude = if_else(Event_type[1] == "Saccade", mean(Saccade_amplitude, na.rm = TRUE), NA_real_),
    N = n(),
    .groups = 'drop'
  ) %>%
  mutate(TOI_Combination = "FPEnX + PEnX") %>%
  filter(!(Event_type == "Fixation" & is.na(AvgDuration)), !(Event_type == "Saccade" & is.na(AvgAmplitude))) %>%
  select(TOI_Combination, Event_type, AvgDuration, AvgAmplitude, N)


# --- Combination 2: FPEt + PEt ---

# 2a. Averages per Recording & Participant
summary_fpet_pet_per_participant <- ds2_final_processed %>%
  filter(TOI %in% fpet_plus_pet_tois) %>%
  group_by(Recording, Participant) %>%
  summarise(
    AvgFixDuration = mean(Duration[Event_type == "Fixation"], na.rm = TRUE),
    N_Fix = sum(Event_type == "Fixation"),
    AvgSaccAmplitude = mean(Saccade_amplitude[Event_type == "Saccade"], na.rm = TRUE),
    N_Sacc = sum(Event_type == "Saccade"),
    .groups = 'drop'
  )

# 2b. Averages Across All Participants
summary_fpet_pet_overall <- ds2_final_processed %>%
  filter(TOI %in% fpet_plus_pet_tois) %>%
  group_by(Event_type) %>%
  summarise(
    AvgDuration = if_else(Event_type[1] == "Fixation", mean(Duration, na.rm = TRUE), NA_real_),
    AvgAmplitude = if_else(Event_type[1] == "Saccade", mean(Saccade_amplitude, na.rm = TRUE), NA_real_),
    N = n(),
    .groups = 'drop'
  ) %>%
  mutate(TOI_Combination = "FPEt + PEt") %>%
  filter(!(Event_type == "Fixation" & is.na(AvgDuration)), !(Event_type == "Saccade" & is.na(AvgAmplitude))) %>%
  select(TOI_Combination, Event_type, AvgDuration, AvgAmplitude, N)


# --- Display Results ---

print("--- Summary for Combined FPEnX + PEnX (Per Rec/Part) ---")
head(summary_fpenx_penx_per_participant)
print("--- Overall Summary for Combined FPEnX + PEnX ---")
print(summary_fpenx_penx_overall)

print("------------------------------------------------------")

print("--- Summary for Combined FPEt + PEt (Per Rec/Part) ---")
head(summary_fpet_pet_per_participant)
print("--- Overall Summary for Combined FPEt + PEt ---")
print(summary_fpet_pet_overall)

# --- End of New Combination Calculations ---
#

# --- Prepare Overall Summary Data for Plotting ---
library(dplyr)
library(tidyr) # For pivot_longer

# Define TOI lists
all_fpen_tois <- c("FPEn", "FPEn2", "FPEn3", "FPEn4")
all_pen_tois <- c("PEn", "PEn2", "PEn3", "PEn4")
all_before_tois_combined <- c("EA", "FPEn", "FPEn2", "FPEn3", "FPEn4", "FPEt")
all_after_tois_combined <- c("PEn", "PEn2", "PEn3", "PEn4", "PEt")

# Helper function to calculate overall summary for a given filter & label
calculate_overall_summary <- function(data, toi_filter_expr, category_label) {
  data %>%
    filter({{ toi_filter_expr }}) %>% # Use {{}} for non-standard evaluation
    group_by(Event_type) %>%
    summarise(
      AvgDuration = if_else(Event_type[1] == "Fixation", mean(Duration, na.rm = TRUE), NA_real_),
      AvgAmplitude = if_else(Event_type[1] == "Saccade", mean(Saccade_amplitude, na.rm = TRUE), NA_real_),
      N = n(),
      .groups = 'drop'
    ) %>%
    mutate(Category = category_label) %>%
    # Pivot to long format: one row per metric per category
    pivot_longer(
      cols = c(AvgDuration, AvgAmplitude),
      names_to = "MetricType",
      values_to = "AvgMetric",
      values_drop_na = TRUE # Drop the NA metric (e.g., AvgAmplitude for Fixations)
    ) %>%
    select(Category, Event_type, MetricType, AvgMetric, N)
}

# Calculate summaries for each category of interest using ds2_final_processed
# Make sure ds2_final_processed is your up-to-date dataframe
summary_ovr_ea <- calculate_overall_summary(ds2_final_processed, TOI == "EA", "EA")
summary_ovr_fpenx <- calculate_overall_summary(ds2_final_processed, TOI %in% all_fpen_tois, "Combined_FPEnX")
summary_ovr_penx <- calculate_overall_summary(ds2_final_processed, TOI %in% all_pen_tois, "Combined_PEnX")
summary_ovr_fpet <- calculate_overall_summary(ds2_final_processed, TOI == "FPEt", "FPEt")
summary_ovr_pet <- calculate_overall_summary(ds2_final_processed, TOI == "PEt", "PEt")
summary_ovr_before <- calculate_overall_summary(ds2_final_processed, TOI %in% all_before_tois_combined, "Combined_Before")
summary_ovr_after <- calculate_overall_summary(ds2_final_processed, TOI %in% all_after_tois_combined, "Combined_After")

# Combine all summaries into one dataframe
all_overall_summaries <- bind_rows(
  summary_ovr_ea, summary_ovr_fpenx, summary_ovr_penx,
  summary_ovr_fpet, summary_ovr_pet,
  summary_ovr_before, summary_ovr_after
) %>%
  # Optional: Make Category a factor for controlled plot order
  mutate(Category = factor(Category, levels = c("EA", "Combined_FPEnX", "FPEt", "Combined_Before",
                                                "Combined_PEnX", "PEt", "Combined_After")))

print("--- Combined Overall Summary Table for Plotting ---")
print(all_overall_summaries)

# --- Plotting the Overall Summaries ---
library(ggplot2)

plot_overall_summaries <- ggplot(all_overall_summaries, aes(x = Category, y = AvgMetric, fill = Category)) +
  # Use geom_col for pre-summarized data (or geom_bar(stat="identity"))
  geom_col(show.legend = FALSE) +
  # Use facet_wrap to create separate plots for Fixation and Saccade metrics
  # Define clearer labels for the facets
  facet_wrap(~ MetricType, scales = "free_y", labeller = labeller(MetricType = c(AvgDuration="Average Fixation Duration (ms)", AvgAmplitude="Average Saccade Amplitude (deg)"))) +
  labs(
    title = "Overall Average Metrics by TOI Category",
    x = "TOI Category / Group",
    y = "Average Value"
  ) +
  theme_minimal(base_size = 12) +
  # Rotate x-axis labels for better readability
  theme(axis.text.x = element_text(angle = 45, hjust = 1, vjust = 1),
        strip.text = element_text(size = 11)) # Adjust facet title size

# Display the plot
print(plot_overall_summaries)

# --- End of Summary Visualization ---





## --- Create plot_data_prep (FINAL VERSION - Exclude Seq 0 from Before Categories) ---

# Make sure libraries are loaded
library(dplyr)
library(tidyr)

# Assumes ds2_sequenced_unified exists (output of Step 2, containing sequence columns)

# --- Step A: Define TOI Categories based on Before/After logic ---
before_event_categories <- c("Combined_FPEnX", "FPEt", "EA") # EA was also 'before'
after_event_categories <- c("Combined_PEnX", "PEt")

# Define TOI lists for assigning categories
all_fpen_tois <- c("FPEn", "FPEn2", "FPEn3", "FPEn4")
all_pen_tois <- c("PEn", "PEn2", "PEn3", "PEn4")
# All TOIs needed for the 4 final plot groups + EA if included above
tois_for_plotting <- unique(c(all_fpen_tois, all_pen_tois, "FPEt", "PEt", if ("EA" %in% before_event_categories) "EA" else NULL))

# --- Step B: Create intermediate table with TOI Category and Unified Sequence/Metric ---
plot_data_intermediate <- ds2_sequenced_unified %>%
  filter(TOI %in% tois_for_plotting) %>%
  mutate(TOI_Category = factor(case_when(
    TOI %in% all_fpen_tois ~ "Combined_FPEnX",
    TOI %in% all_pen_tois ~ "Combined_PEnX",
    TOI == "FPEt" ~ "FPEt",
    TOI == "PEt" ~ "PEt",
    TOI == "EA" ~ "EA"
  ), levels = c("Combined_FPEnX", "FPEt", "EA", "Combined_PEnX", "PEt"))) %>% # Adjusted levels
  filter(!is.na(TOI_Category)) %>% # Remove rows not assigned a category
  mutate(
    Sequence = case_when(
      Event_type == "Fixation" ~ FixationSequence,
      Event_type == "Saccade" ~ SaccadeSequence,
      TRUE ~ NA_integer_
    ),
    Metric = case_when(
      Event_type == "Fixation" ~ Duration,
      Event_type == "Saccade" ~ Saccade_amplitude,
      TRUE ~ NA_real_
    )
  ) %>%
  filter(!is.na(Sequence), !is.na(Metric))

# --- Step C: Apply MODIFIED FILTER - Enforce Sequence Sign (Seq < 0 for Before) ---
print("Applying filter to enforce Sequence sign (Seq < 0 for 'Before', Seq >= 0 for 'After')...")
plot_data_prep <- plot_data_intermediate %>%
  filter(
    # Keep rows where the Category is 'Before' AND Sequence is strictly negative
    (TOI_Category %in% before_event_categories[before_event_categories %in% levels(TOI_Category)] & Sequence < 0) |
      # Keep rows where the Category is 'After' AND Sequence is zero or positive
      (TOI_Category %in% after_event_categories[after_event_categories %in% levels(TOI_Category)] & Sequence >= 0)
  ) %>%
  # --- Step D: Filter for desired sequence plotting range (-5 to +5) ---
  filter(Sequence >= -5 & Sequence <= 5) %>%
  
  # --- Step E: Keep only essential columns ---
  # Selecting only columns needed downstream avoids carrying unused ones
  select(Participant, Recording, TOI, TOI_Category, Event_type, Sequence, Metric)


# --- Run Checks Immediately After Creation ---
print("Created REVISED 'plot_data_prep'. Checking dimensions:")
print(dim(plot_data_prep))
print("Checking Sequence ranges per TOI Category AFTER filtering:")
# MaxSeq for Before types should now be -1
print(plot_data_prep %>%
        group_by(TOI_Category) %>%
        summarise(MinSeq=min(Sequence), MaxSeq=max(Sequence), N_Rows = n(), .groups = 'drop')
)

print("-------------------------------------------")
print("Checking if Sequence == 0 exists at all AFTER filtering:")
if (any(plot_data_prep$Sequence == 0)) {
  print("Sequence == 0 data FOUND. Belongs to TOI Categories:")
  print(plot_data_prep %>% filter(Sequence == 0) %>% distinct(TOI_Category))
} else {
  print("CORRECT: No data with Sequence == 0 found for 'Before' TOIs, and potentially none for 'After' TOIs either.")
}
# --- End of REVISED plot_data_prep Creation and Checks ---




# --- Prepare Data & Create Faceted Sequence Plot per Recording ---
library(dplyr)
library(ggplot2)

# Re-calculate the per-recording averages (if not already in environment)
# Assumes plot_data_prep exists from previous plotting steps
avg_data_per_recording <- plot_data_prep %>%
  group_by(Recording, Event_type, Sequence) %>%
  summarise(
    AvgMetric = mean(Metric, na.rm = TRUE),
    N = n(),
    .groups = 'drop'
  ) %>%
  # Add a clearer metric name column for facet labels
  mutate(MetricName = case_when(
    Event_type == "Fixation" ~ "Avg Fixation Duration (ms)",
    Event_type == "Saccade" ~ "Avg Saccade Amplitude (deg)"
  ))

# Create the plot using facet_grid
plot_sequence_faceted_grid <- ggplot(avg_data_per_recording,
                                     aes(x = Sequence, y = AvgMetric, color = Event_type)) +
  geom_line(na.rm = TRUE, linewidth = 0.8) +
  geom_point(na.rm = TRUE, size = 1.5) + # Slightly smaller points
  # Facet by MetricName (creates rows) and Recording (creates columns)
  # scales="free_y" allows each metric (row) to have its own y-axis scale
  # switch="y" moves the row labels (MetricName) to the left side
  facet_grid(MetricName ~ Recording, scales = "free_y", switch = "y") +
  scale_x_continuous(breaks = c(-5:-1, 1:5)) + # Keep original breaks
  # Define colors manually
  scale_color_manual(name = "Metric Type", values = c("Fixation" = "dodgerblue", "Saccade" = "firebrick")) +
  labs(
    title = "Sequence Plot per Recording",
    subtitle = "Avg Duration (top row) & Avg Amplitude (bottom row) vs. Sequence relative to Key Event",
    x = "Sequence relative to Key Event",
    y = NULL # Y-axis label is effectively handled by the facet strip on the left
  ) +
  theme_minimal(base_size = 11) +
  theme(
    panel.grid.minor.x = element_blank(),
    # Adjust facet label appearance
    strip.text.y.left = element_text(angle = 0, size=10, face = "bold"), # Metric names (rows)
    strip.text.x = element_text(size=9), # Recording names (columns)
    strip.background = element_blank(), # Remove facet background for cleaner look
    axis.text.x = element_text(size=8), # Adjust x-axis text size if needed
    legend.position = "none" # Hide legend as color is redundant with row facets
  )

# Display the plot
print(plot_sequence_faceted_grid)

# --- End of Faceted Sequence Plot ---


# --- Generate Overlay Sequence Plot per Participant ---

# Make sure libraries are loaded
library(dplyr)
library(ggplot2)
# library(purrr) # An alternative to loops using map functions

# Assumes 'plot_data_prep' dataframe exists and is up-to-date
# plot_data_prep contains columns: Participant, Recording, TOI, Event_type, Sequence, Metric

# --- 1. Get list of unique participants ---
participant_list <- unique(plot_data_prep$Participant)

print(paste("Found", length(participant_list), "participants to generate plots for."))
print(paste("Participants:", paste(participant_list, collapse=", ")))


# --- Optional: Create a directory to save plots ---
# If you have many participants, saving is better than printing all to console.
# Adjust the path as needed.
plot_dir <- "participant_sequence_plots_overlay"
if (!dir.exists(plot_dir)) {
  dir.create(plot_dir)
  print(paste("Created directory for plots:", plot_dir))
} else {
  print(paste("Directory for plots already exists:", plot_dir))
}


# --- 2. Loop through each participant ---
for (current_participant_id in participant_list) {
  
  print(paste("--- Processing participant:", current_participant_id, "---"))
  
  # --- a. Filter and Calculate Averages for Current Participant ---
  avg_data_current_participant <- plot_data_prep %>%
    filter(Participant == current_participant_id) %>%
    # Calculate AvgMetric per Event_type and Sequence for this participant
    group_by(Event_type, Sequence) %>%
    summarise(
      AvgMetric = mean(Metric, na.rm = TRUE),
      N = n(), # Number of observations for this point
      .groups = 'drop' # Drop grouping
    )
  
  # Check if enough data points exist after averaging
  if (nrow(avg_data_current_participant) < 2) {
    print(paste("  Skipping participant", current_participant_id, "- insufficient data points after averaging."))
    next # Go to the next participant in the loop
  }
  
  # Separate fixation and saccade data for this participant
  fix_data_single <- avg_data_current_participant %>% filter(Event_type == "Fixation")
  sacc_data_single <- avg_data_current_participant %>% filter(Event_type == "Saccade")
  
  # Check if BOTH fixation and saccade data exist for this participant to create overlay
  if (nrow(fix_data_single) == 0 || nrow(sacc_data_single) == 0) {
    print(paste("  Skipping participant", current_participant_id, "- missing either fixation or saccade average data for sequences -5:5."))
    next # Go to the next participant
  }
  
  # --- b. Determine Scaling Factor for Current Participant ---
  range_fix <- range(fix_data_single$AvgMetric, na.rm = TRUE)
  range_sacc <- range(sacc_data_single$AvgMetric, na.rm = TRUE)
  scaling_factor <- 1 # Default scaling factor
  
  # Calculate scaling factor only if ranges are valid (finite and different min/max)
  if (all(is.finite(range_fix)) && all(is.finite(range_sacc)) && diff(range_fix)[1] > 0 && diff(range_sacc)[1] > 0) {
    scaling_factor <- diff(range_fix)[1] / diff(range_sacc)[1]
    print(paste("  Scaling factor calculated:", round(scaling_factor, 2)))
  } else {
    print(paste("  Warning: Could not determine valid metric ranges for scaling. Using default scaling=1 for participant", current_participant_id))
    # You could choose to 'next' here if you don't want plots with default scaling
  }
  
  # --- c. Create the Overlay Plot for Current Participant ---
  # Uses the same ggplot logic as response #48
  plot_overlay_current_participant <- ggplot() +
    # Fixation line/points (Left Axis)
    geom_line(data = fix_data_single, aes(x = Sequence, y = AvgMetric, color = "Fixation Duration"), linewidth = 1, na.rm = TRUE) +
    geom_point(data = fix_data_single, aes(x = Sequence, y = AvgMetric, color = "Fixation Duration"), size = 3, na.rm = TRUE) +
    # Scaled Saccade line/points (Right Axis)
    geom_line(data = sacc_data_single, aes(x = Sequence, y = AvgMetric * scaling_factor, color = "Saccade Amplitude"), linewidth = 1, na.rm = TRUE) +
    geom_point(data = sacc_data_single, aes(x = Sequence, y = AvgMetric * scaling_factor, color = "Saccade Amplitude"), size = 3, na.rm = TRUE) +
    # Define the primary (left) and secondary (right) axes
    scale_y_continuous(
      name = "Average Fixation Duration (ms)", # Primary axis label
      sec.axis = sec_axis(
        trans = ~ . / scaling_factor, # Inverse transform for secondary axis labels
        name = "Average Saccade Amplitude (degrees)" # Secondary axis label
      )
    ) +
    # X-axis and plot aesthetics
    scale_x_continuous(breaks = c(-5:-1, 1:5), limits = c(-5.5, 5.5)) +
    scale_color_manual(values = c("Fixation Duration" = "dodgerblue", "Saccade Amplitude" = "firebrick")) +
    labs(
      title = paste("Sequence Plot (Participant:", current_participant_id, ")"),
      x = "Sequence relative to Key Event",
      color = "Metric" # Legend title
    ) +
    theme_minimal(base_size = 14) +
    theme(
      panel.grid.minor.x = element_blank(),
      axis.title.y.left = element_text(color = "dodgerblue"),
      axis.text.y.left = element_text(color = "dodgerblue"),
      axis.title.y.right = element_text(color = "firebrick"),
      axis.text.y.right = element_text(color = "firebrick"),
      legend.position = "bottom"
    )
  
  # --- d. Display or Save the Plot ---
  
  # Option A: Display plot directly in RStudio plot pane
   print(plot_overlay_current_participant)
  
  # Option B: Save plot to the directory created earlier (recommended for many plots)
  plot_filename <- file.path(plot_dir, paste0("sequence_overlay_", current_participant_id, ".png"))
  ggsave(
    filename = plot_filename,
    plot = plot_overlay_current_participant,
    width = 8, height = 6, units = "in", dpi = 300 # Adjust size/resolution as needed
  )
  print(paste("  Saved plot to:", plot_filename))
  
  
} # --- End of loop through participants ---

print("--- Finished processing all participants. ---")

# --- End of Per-Participant Overlay Plot Code ---


# --- Generate Overlay Sequence Plot per Recording ---

# Make sure libraries are loaded
library(dplyr)
library(ggplot2)

# Assumes 'plot_data_prep' exists and is up-to-date
# plot_data_prep contains: Participant, Recording, TOI, Event_type, Sequence, Metric

# --- 1. Get list of unique recordings ---
recording_list <- unique(plot_data_prep$Recording)

print(paste("Found", length(recording_list), "recordings to generate plots for."))
print(paste("Recordings:", paste(recording_list, collapse=", ")))


# --- Optional: Create a directory to save plots ---
plot_dir_rec <- "recording_sequence_plots_overlay"
if (!dir.exists(plot_dir_rec)) {
  dir.create(plot_dir_rec)
  print(paste("Created directory for plots:", plot_dir_rec))
} else {
  print(paste("Directory for plots already exists:", plot_dir_rec))
}


# --- 2. Loop through each recording ---
for (current_recording_id in recording_list) {
  
  print(paste("--- Processing recording:", current_recording_id, "---"))
  
  # --- a. Filter and Calculate Averages for Current Recording ---
  # Filter for the current recording, then average across participants within it
  avg_data_current_recording <- plot_data_prep %>%
    filter(Recording == current_recording_id) %>%
    # Calculate AvgMetric per Event_type and Sequence for this recording
    group_by(Event_type, Sequence) %>%
    summarise(
      AvgMetric = mean(Metric, na.rm = TRUE),
      N = n(),
      .groups = 'drop'
    )
  
  # Check if sufficient data exists
  if (nrow(avg_data_current_recording) < 2) {
    print(paste("  Skipping recording", current_recording_id, "- insufficient data points after averaging."))
    next # Skip to the next recording
  }
  
  # Separate fixation and saccade data for this recording
  fix_data_rec <- avg_data_current_recording %>% filter(Event_type == "Fixation")
  sacc_data_rec <- avg_data_current_recording %>% filter(Event_type == "Saccade")
  
  # Check if BOTH fixation and saccade average data exist
  if (nrow(fix_data_rec) == 0 || nrow(sacc_data_rec) == 0) {
    print(paste("  Skipping recording", current_recording_id, "- missing either fixation or saccade average data for sequences -5:5."))
    next # Skip to the next recording
  }
  
  # --- b. Determine Scaling Factor for Current Recording ---
  range_fix <- range(fix_data_rec$AvgMetric, na.rm = TRUE)
  range_sacc <- range(sacc_data_rec$AvgMetric, na.rm = TRUE)
  scaling_factor <- 1 # Default
  
  if (all(is.finite(range_fix)) && all(is.finite(range_sacc)) && diff(range_fix)[1] > 0 && diff(range_sacc)[1] > 0) {
    scaling_factor <- diff(range_fix)[1] / diff(range_sacc)[1]
    print(paste("  Scaling factor calculated:", round(scaling_factor, 2)))
  } else {
    print(paste("  Warning: Could not determine valid ranges for scaling. Using default scaling=1 for recording", current_recording_id))
  }
  
  # --- c. Create the Overlay Plot for Current Recording ---
  plot_overlay_current_recording <- ggplot() +
    # Fixation data (Left Axis)
    geom_line(data = fix_data_rec, aes(x = Sequence, y = AvgMetric, color = "Fixation Duration"), linewidth = 1, na.rm = TRUE) +
    geom_point(data = fix_data_rec, aes(x = Sequence, y = AvgMetric, color = "Fixation Duration"), size = 3, na.rm = TRUE) +
    # Scaled Saccade data (Right Axis)
    geom_line(data = sacc_data_rec, aes(x = Sequence, y = AvgMetric * scaling_factor, color = "Saccade Amplitude"), linewidth = 1, na.rm = TRUE) +
    geom_point(data = sacc_data_rec, aes(x = Sequence, y = AvgMetric * scaling_factor, color = "Saccade Amplitude"), size = 3, na.rm = TRUE) +
    # Axes definition
    scale_y_continuous(
      name = "Average Fixation Duration (ms)",
      sec.axis = sec_axis(
        trans = ~ . / scaling_factor,
        name = "Average Saccade Amplitude (degrees)"
      )
    ) +
    # X-axis, colors, labels, theme
    scale_x_continuous(breaks = c(-5:-1, 1:5), limits = c(-5.5, 5.5)) +
    scale_color_manual(values = c("Fixation Duration" = "dodgerblue", "Saccade Amplitude" = "firebrick")) +
    labs(
      title = paste("Sequence Plot (Recording:", current_recording_id, ")"),
      subtitle = "Overlay of Avg Fixation Duration and Avg Saccade Amplitude",
      x = "Sequence relative to Key Event",
      color = "Metric"
    ) +
    theme_minimal(base_size = 14) +
    theme(
      panel.grid.minor.x = element_blank(),
      axis.title.y.left = element_text(color = "dodgerblue"),
      axis.text.y.left = element_text(color = "dodgerblue"),
      axis.title.y.right = element_text(color = "firebrick"),
      axis.text.y.right = element_text(color = "firebrick"),
      legend.position = "bottom"
    )
  
  # --- d. Save the Plot (Recommended) ---
  plot_filename <- file.path(plot_dir_rec, paste0("sequence_overlay_", current_recording_id, ".png"))
  ggsave(
    filename = plot_filename,
    plot = plot_overlay_current_recording,
    width = 8, height = 6, units = "in", dpi = 300
  )
  print(paste("  Saved plot to:", plot_filename))
  
  # Optional: Also print to console if desired
  # print(plot_overlay_current_recording)
  
} # --- End of loop through recordings ---

print("--- Finished processing all recordings. ---")

# --- End of Per-Recording Overlay Plot Code ---


# --- Generate Overlay Sequence Plots for Combined TOI Groups --- ääääääääääääääääääääääääääääääääääääääääääääääääääääääääääääääääääääääääääääääääääääääääääääääääääääääääääääääääääääääääääääääääääääääääääääääääääääääääääääääääääääääää E L I M I N A T I O N  VS E L I M I N A T E D 

# Make sure libraries are loaded
library(dplyr)
library(ggplot2)

# Assumes 'plot_data_prep' dataframe exists and is up-to-date from previous steps

# Define TOI lists for the combinations
fpenx_plus_penx_tois <- c("FPEn", "FPEn2", "FPEn3", "FPEn4", "PEn", "PEn2", "PEn3", "PEn4")
fpet_plus_pet_tois <- c("FPEt", "PEt")

# --- 1. Calculate Overall Averages for FPEnX + PEnX ---
print("Calculating overall averages for FPEnX + PEnX group...")
avg_fpenx_penx_overall <- plot_data_prep %>%
  filter(TOI %in% fpenx_plus_penx_tois) %>%
  # Group only by Event_type and Sequence for overall average
  group_by(Event_type, Sequence) %>%
  summarise(
    AvgMetric = mean(Metric, na.rm = TRUE),
    N = n(), # Total observations across all participants/recordings
    .groups = 'drop'
  )

# --- 2. Calculate Overall Averages for FPEt + PEt ---
print("Calculating overall averages for FPEt + PEt group...")
avg_fpet_pet_overall <- plot_data_prep %>%
  filter(TOI %in% fpet_plus_pet_tois) %>%
  # Group only by Event_type and Sequence for overall average
  group_by(Event_type, Sequence) %>%
  summarise(
    AvgMetric = mean(Metric, na.rm = TRUE),
    N = n(),
    .groups = 'drop'
  )

# --- 3. Reusable Function to Create Overlay Plot ---
#    (Takes averaged data and a title)
create_combined_overlay_plot <- function(avg_data, plot_title) {
  
  # Check if data is sufficient
  if (!inherits(avg_data, "data.frame") || nrow(avg_data) < 2) {
    warning(paste("Skipping plot:", plot_title, "- insufficient data points after averaging."), call. = FALSE)
    return(NULL) # Return NULL if no plot can be made
  }
  
  fix_data <- avg_data %>% filter(Event_type == "Fixation")
  sacc_data <- avg_data %>% filter(Event_type == "Saccade")
  
  # Check if BOTH fixation and saccade data exist
  if (nrow(fix_data) == 0 || nrow(sacc_data) == 0) {
    warning(paste("Skipping plot:", plot_title, "- missing either fixation or saccade average data."), call. = FALSE)
    return(NULL)
  }
  
  # Determine Scaling Factor
  range_fix <- range(fix_data$AvgMetric, na.rm = TRUE)
  range_sacc <- range(sacc_data$AvgMetric, na.rm = TRUE)
  scaling_factor <- 1 # Default
  
  if (all(is.finite(range_fix)) && all(is.finite(range_sacc)) && diff(range_fix)[1] > 0 && diff(range_sacc)[1] > 0) {
    scaling_factor <- diff(range_fix)[1] / diff(range_sacc)[1]
    print(paste("  Scaling factor for", plot_title, ":", round(scaling_factor, 2)))
  } else {
    print(paste("  Warning: Could not determine valid ranges for scaling in", plot_title,". Using default scaling=1."))
  }
  
  # Create the plot
  p <- ggplot() +
    geom_line(data = fix_data, aes(x = Sequence, y = AvgMetric, color = "Fixation Duration"), linewidth = 1, na.rm = TRUE) +
    geom_point(data = fix_data, aes(x = Sequence, y = AvgMetric, color = "Fixation Duration"), size = 3, na.rm = TRUE) +
    geom_line(data = sacc_data, aes(x = Sequence, y = AvgMetric * scaling_factor, color = "Saccade Amplitude"), linewidth = 1, na.rm = TRUE) +
    geom_point(data = sacc_data, aes(x = Sequence, y = AvgMetric * scaling_factor, color = "Saccade Amplitude"), size = 3, na.rm = TRUE) +
    scale_y_continuous(
      name = "Average Fixation Duration (ms)",
      sec.axis = sec_axis( trans = ~ . / scaling_factor, name = "Average Saccade Amplitude (degrees)")
    ) +
    scale_x_continuous(breaks = c(-5:-1, 1:5), limits = c(-5.5, 5.5)) +
    scale_color_manual(values = c("Fixation Duration" = "dodgerblue", "Saccade Amplitude" = "firebrick")) +
    labs(
      title = plot_title,
      subtitle = "Overlay of Avg Fixation Duration and Avg Saccade Amplitude (Overall Avg)",
      x = "Sequence relative to Key Event",
      color = "Metric"
    ) +
    theme_minimal(base_size = 14) +
    theme(
      panel.grid.minor.x = element_blank(),
      axis.title.y.left = element_text(color = "dodgerblue"),
      axis.text.y.left = element_text(color = "dodgerblue"),
      axis.title.y.right = element_text(color = "firebrick"),
      axis.text.y.right = element_text(color = "firebrick"),
      legend.position = "bottom"
    )
  
  return(p)
}


# --- 4. Generate and Display the Plots ---

# Plot for FPEnX + PEnX (Overall)
plot_overlay_fpenx_penx <- create_combined_overlay_plot(
  avg_fpenx_penx_overall,
  "Overall Sequence Plot (Combined FPEnX + PEnX)"
)
if (!is.null(plot_overlay_fpenx_penx)) {
  print(plot_overlay_fpenx_penx)
}

# Plot for FPEt + PEt (Overall)
plot_overlay_fpet_pet <- create_combined_overlay_plot(
  avg_fpet_pet_overall,
  "Overall Sequence Plot (Combined FPEt + PEt)"
)
if (!is.null(plot_overlay_fpet_pet)) {
  print(plot_overlay_fpet_pet)
}

# --- End of Combined TOI Group Sequence Plots ---


# --- Generate Overlay Sequence Plot for FPEn + PEn ---

# Make sure libraries are loaded
library(dplyr)
library(ggplot2)

# Assumes 'plot_data_prep' dataframe exists and is up-to-date
# Assumes the 'create_combined_overlay_plot' function from the previous response is already defined

# Define the specific TOIs for this combination
fpen_plus_pen_tois <- c("FPEn", "PEn")

# --- 1. Calculate Overall Averages for FPEn + PEn ---
print("Calculating overall averages for FPEn + PEn group...")
avg_fpen_pen_overall <- plot_data_prep %>%
  # Filter only for the first instance TOIs: FPEn and PEn
  filter(TOI %in% fpen_plus_pen_tois) %>%
  # Group only by Event_type and Sequence for overall average
  group_by(Event_type, Sequence) %>%
  summarise(
    AvgMetric = mean(Metric, na.rm = TRUE),
    N = n(), # Total observations across all participants/recordings
    .groups = 'drop'
  )

# --- 2. Generate and Display the Plot ---
# Reuse the plotting function defined previously

print("Generating plot for FPEn + PEn...")
# Plot for FPEn + PEn (Overall)
plot_overlay_fpen_pen <- create_combined_overlay_plot(
  avg_fpen_pen_overall, # Use the newly calculated average data
  "Overall Sequence Plot (Combined FPEn + PEn)" # Updated plot title
)

# Display the plot if it was created successfully
if (!is.null(plot_overlay_fpen_pen)) {
  print(plot_overlay_fpen_pen)
} else {
  print("Plot for FPEn + PEn could not be generated (check if sufficient data exists for these TOIs in the sequence range).")
}

# --- End of FPEn + PEn Sequence Plot ---





# --- Alt A: Separate Plots for Fix/Sacc per Participant, comparing 4 TOI Groups ---

# Make sure libraries are loaded
library(dplyr)
library(ggplot2)

# --- Separate Plots for Fix/Sacc per Recording, comparing 4 TOI Groups ---

# Make sure libraries are loaded
library(dplyr)
library(ggplot2)

# Assumes 'plot_data_prep' dataframe exists and is up-to-date
# Requires plot_data_prep created using the code from response #77/83
# (which correctly excludes Seq 0 from "Before" TOIs)

# --- 1. Define TOI Categories and Prepare Averaged Data PER RECORDING ---
print("Calculating averages PER RECORDING for 4 TOI categories...")

# Define TOI lists
all_fpen_tois <- c("FPEn", "FPEn2", "FPEn3", "FPEn4")
all_pen_tois <- c("PEn", "PEn2", "PEn3", "PEn4")
tois_for_4_groups <- c(all_fpen_tois, all_pen_tois, "FPEt", "PEt") # TOIs needed

# Check if plot_data_prep exists
if (!exists("plot_data_prep")) {
  stop("Error: 'plot_data_prep' not found. Please run the code to create it first (e.g., from response #77/83).")
}


# Add TOI Category column based on original TOI, filter, and calculate averages PER RECORDING
avg_data_4groups_per_recording <- plot_data_prep %>%
  filter(TOI %in% tois_for_4_groups) %>%
  # Create the TOI Category identifier
  mutate(TOI_Category = factor(case_when(
    TOI %in% all_fpen_tois ~ "Combined_FPEnX",
    TOI %in% all_pen_tois ~ "Combined_PEnX",
    TOI == "FPEt" ~ "FPEt",
    TOI == "PEt" ~ "PEt"
  ), levels = c("Combined_FPEnX", "FPEt", "Combined_PEnX", "PEt"))) %>%
  filter(!is.na(TOI_Category)) %>% # Ensure category assigned
  # *** Group by Recording (NOT Participant) ***
  group_by(Recording, TOI_Category, Event_type, Sequence) %>%
  summarise(AvgMetric = mean(Metric, na.rm = TRUE), N = n(), .groups = 'drop')


# Check if data generation was successful
if(nrow(avg_data_4groups_per_recording) == 0){
  stop("No average data generated. Check input 'plot_data_prep' and TOI filters.")
} else {
  print("Average data calculation per Recording complete.")
}


# --- 2. Get Recording List ---
# Get unique recordings from the averaged data
recording_list_4groups <- unique(avg_data_4groups_per_recording$Recording)
print(paste("Found", length(recording_list_4groups), "recordings with data for the 4 TOI groups."))


# --- 3. Plotting Functions (Accept Recording ID for Title) ---

# Plot Fixation Durations for 4 TOI groups
plot_fix_4groups_rec <- function(recording_data, current_rec_id) {
  plot_data <- recording_data %>% filter(Event_type == "Fixation")
  if(nrow(plot_data) == 0) return(NULL) # Return NULL if no data for this metric
  
  ggplot(plot_data,
         aes(x = Sequence, y = AvgMetric,
             color = TOI_Category,    # Color by TOI Category
             linetype = TOI_Category # Use linetype too
         )) +
    geom_line(na.rm = TRUE, linewidth = 1) +
    geom_point(na.rm = TRUE, size = 2.5) +
    # Use scale_x_continuous to define breaks and limits
    scale_x_continuous(breaks = -5:5, limits = c(-5.5, 5.5)) + # Show -5 to +5, excluding 0 if not present
    scale_color_brewer(palette = "Dark2", name = "TOI Group") +
    scale_linetype_manual(values=c("solid", "dashed", "dotted", "dotdash"), name="TOI Group") +
    labs(title = paste("Fixation Duration Sequence (Recording:", current_rec_id, ")"), # Use Recording ID
         subtitle = "Comparing TOI Categories",
         x = "Sequence relative to Key Event", y = "Avg Fixation Duration (ms)") +
    theme_minimal(base_size = 12) +
    theme(legend.position = "bottom")
}

# Plot Saccade Amplitudes for 4 TOI groups
plot_sacc_4groups_rec <- function(recording_data, current_rec_id) {
  plot_data <- recording_data %>% filter(Event_type == "Saccade")
  if(nrow(plot_data) == 0) return(NULL) # Return NULL if no data
  
  ggplot(plot_data,
         aes(x = Sequence, y = AvgMetric, color = TOI_Category, linetype = TOI_Category)) +
    geom_line(na.rm = TRUE, linewidth = 1) +
    geom_point(na.rm = TRUE, size = 2.5) +
    scale_x_continuous(breaks = -5:5, limits = c(-5.5, 5.5)) + # Show -5 to +5
    scale_color_brewer(palette = "Dark2", name = "TOI Group") +
    scale_linetype_manual(values=c("solid", "dashed", "dotted", "dotdash"), name="TOI Group") +
    labs(title = paste("Saccade Amplitude Sequence (Recording:", current_rec_id, ")"), # Use Recording ID
         subtitle = "Comparing TOI Categories",
         x = "Sequence relative to Key Event", y = "Avg Saccade Amplitude (degrees)") +
    theme_minimal(base_size = 12) +
    theme(legend.position = "bottom")
}


# --- 4. Loop Through Recordings and PRINT Plots to Output Pane ---
print("--- Generating plots PER RECORDING (Separate for Fix/Sacc) ---")

# Iterate through each unique Recording ID
for(rec_id in recording_list_4groups) {
  print(paste("--- Processing recording:", rec_id, "---"))
  # Filter the averaged data for only the current recording
  recording_avg_data <- avg_data_4groups_per_recording %>% filter(Recording == rec_id)
  
  # Check if data exists for this recording before plotting
  if(nrow(recording_avg_data) > 0) {
    # Create the fixation plot
    p_fix <- plot_fix_4groups_rec(recording_avg_data, rec_id)
    # Create the saccade plot
    p_sacc <- plot_sacc_4groups_rec(recording_avg_data, rec_id)
    
    # Print the plots to the R output pane if they were created successfully
    if(!is.null(p_fix)) {
      print(p_fix) # Display fixation plot
      print("Displayed Fixation plot.")
      # Sys.sleep(1) # Optional pause
    } else {
      print(paste("  No fixation data to plot for recording", rec_id))
    }
    if(!is.null(p_sacc)) {
      print(p_sacc) # Display saccade plot
      print("Displayed Saccade plot.")
      # Sys.sleep(1) # Optional pause
    } else {
      print(paste("  No saccade data to plot for recording", rec_id))
    }
  } else {
    print(paste("  Skipping recording", rec_id, "- no averaged data found for any category."))
  }
} # End of loop
print("--- Finished generating plots ---")

# --- End of Plotting Code per Recording ---

# --- Overlay Plot: 4 TOI Categories x 2 Metrics, Per Recording ---

# Make sure libraries are loaded
library(dplyr)
library(ggplot2)

# --- Corrected Section 1: Calculate Averaged Data Per Recording (EXCLUDING EA) ---

# Make sure libraries are loaded
library(dplyr)
library(ggplot2)

# Assumes 'plot_data_prep' exists from response #77 or similar

print("Calculating averages per recording for 4 TOI categories (Ensuring EA is excluded)...")

# Define TOI lists for the 4 groups *only*
all_fpen_tois <- c("FPEn", "FPEn2", "FPEn3", "FPEn4")
all_pen_tois <- c("PEn", "PEn2", "PEn3", "PEn4")
# Use this list directly for filtering - DOES NOT INCLUDE EA
tois_for_4_groups <- c(all_fpen_tois, all_pen_tois, "FPEt", "PEt")

# Check if plot_data_prep exists
if (!exists("plot_data_prep")) {
  stop("Error: 'plot_data_prep' not found. Please run the code to create it first (e.g., from response #77).")
}

# Add TOI Category column based on original TOI, filter, and calculate averages
avg_data_4groups_per_recording <- plot_data_prep %>%
  # Filter for ONLY the TOIs making up the 4 target categories
  # THIS FILTER NOW EXCLUDES EA
  filter(TOI %in% tois_for_4_groups) %>%
  # Create the TOI Category identifier (NO EA case needed)
  mutate(TOI_Category = factor(case_when(
    TOI %in% all_fpen_tois ~ "Combined_FPEnX",
    TOI %in% all_pen_tois ~ "Combined_PEnX",
    TOI == "FPEt" ~ "FPEt",
    TOI == "PEt" ~ "PEt"
    # No 'TRUE ~ NA' or EA case needed because of the filter above
  ), levels = c("Combined_FPEnX", "FPEt", "Combined_PEnX", "PEt"))) %>% # Factor levels only for the 4 groups
  # Group to calculate averages needed for the plot
  group_by(Recording, TOI_Category, Event_type, Sequence) %>%
  summarise(AvgMetric = mean(Metric, na.rm = TRUE), N = n(), .groups = 'drop')

# Check if data generation was successful
if(nrow(avg_data_4groups_per_recording) == 0){
  stop("No average data generated. Check input 'plot_data_prep' and TOI filters.")
} else {
  print("Average data calculation complete (EA excluded).")
  print("Checking TOI Categories included in average data:")
  # This should now only list the 4 intended categories
  print(unique(avg_data_4groups_per_recording$TOI_Category))
}

# --- End of Corrected Section 1 ---

# NOW continue with the rest of the code from response #79:
# --- 2. Get Recording List ---
# --- 3. Loop Through Recordings and Create Complex Overlay Plots ---
# (Including defining the plotting functions and the loop itself)
# Make sure the scale_linetype_manual also only defines 4 types:
# scale_linetype_manual(name = "TOI Category", values = c("Combined_FPEnX" = "solid", "FPEt" = "dashed", "Combined_PEnX" = "dotted", "PEt" = "dotdash"))


# --- 2. Get Recording List ---
recording_list_4groups <- unique(avg_data_4groups_per_recording$Recording)
print(paste("Found", length(recording_list_4groups), "recordings to generate plots for."))


# --- 3. Loop Through Recordings and Create Complex Overlay Plots ---
print("--- Generating overlay plots per recording (will display in plot pane) ---")
print("--- WARNING: These plots contain up to 8 lines and may be visually complex! ---")

for(rec_id in recording_list_4groups) {
  print(paste("--- Processing recording:", rec_id, "---"))
  # Filter the averaged data for the current recording
  recording_avg_data <- avg_data_4groups_per_recording %>% filter(Recording == rec_id)
  
  # Check if data exists for this recording
  if(nrow(recording_avg_data) < 2) { # Need at least 2 points overall for a meaningful plot
    print(paste("  Skipping recording", rec_id, "- insufficient averaged data points."))
    next # Skip to the next recording in the loop
  }
  
  # Separate Fixation and Saccade data *for this recording* to calculate scaling factor
  fix_data_rec <- recording_avg_data %>% filter(Event_type == "Fixation")
  sacc_data_rec <- recording_avg_data %>% filter(Event_type == "Saccade")
  
  # Check if both types exist to allow for overlay scaling
  if (nrow(fix_data_rec) == 0 || nrow(sacc_data_rec) == 0) {
    print(paste("  Skipping recording", rec_id, "- missing either fixation or saccade average data needed for overlay scaling."))
    next # Skip to the next recording
  }
  
  # --- Calculate Scaling Factor specific to this Recording ---
  range_fix <- range(fix_data_rec$AvgMetric, na.rm = TRUE)
  range_sacc <- range(sacc_data_rec$AvgMetric, na.rm = TRUE)
  scaling_factor <- 1 # Default scaling factor
  
  # Calculate only if ranges are valid and non-zero
  if (all(is.finite(range_fix)) && all(is.finite(range_sacc)) && diff(range_fix)[1] > 0 && diff(range_sacc)[1] > 0) {
    scaling_factor <- diff(range_fix)[1] / diff(range_sacc)[1]
    print(paste("  Scaling factor calculated:", round(scaling_factor, 2)))
  } else {
    print(paste("  Warning: Could not determine valid ranges for scaling. Using default scaling=1 for recording", rec_id))
  }
  
  # --- Create the Complex Overlay Plot ---
  # Use color for Metric (Fix/Sacc) and linetype for TOI Category (up to 4 levels)
  # Ensure TOI_Category is a factor for consistent linetype mapping
  plot_complex_overlay <- ggplot(data = recording_avg_data %>% mutate(TOI_Category=factor(TOI_Category)),
                                 aes(x = Sequence)) +
    
    # Layer 1: Plot Fixation Durations (Primary Axis)
    geom_line(data = . %>% filter(Event_type == "Fixation"),
              aes(y = AvgMetric, color = "Fixation Duration", linetype = TOI_Category),
              linewidth = 0.8, na.rm = TRUE) +
    
    # Layer 2: Plot *Transformed* Saccade Amplitudes (Secondary Axis)
    geom_line(data = . %>% filter(Event_type == "Saccade"),
              aes(y = AvgMetric * scaling_factor, color = "Saccade Amplitude", linetype = TOI_Category),
              linewidth = 0.8, na.rm = TRUE) +
    
    # Optional: Add points (makes it even busier)
    # geom_point(aes(y = ifelse(Event_type == "Fixation", AvgMetric, AvgMetric * scaling_factor),
    #                color = Event_type, shape = TOI_Category), size = 2, na.rm = TRUE) +
    
    # Layer 3: Define Axes and Scales
    scale_y_continuous(
      name = "Average Fixation Duration (ms)", # Left axis label
      # Define secondary axis for Saccade Amplitude
      sec.axis = sec_axis(
        trans = ~ . / scaling_factor, # Inverse transform to get original saccade values
        name = "Average Saccade Amplitude (degrees)" # Right axis label
      )
    ) +
    scale_x_continuous(breaks = c(-5:-1, 1:5), limits = c(-5.5, 5.5)) + # Show relevant sequence ticks
    # Define aesthetics manually
    scale_color_manual(name = "Metric", values = c("Fixation Duration" = "dodgerblue", "Saccade Amplitude" = "firebrick")) +
    # Define linetypes for the TOI categories
    scale_linetype_manual(name = "TOI Category", values = c("Combined_FPEnX" = "solid", "FPEt" = "dashed", "Combined_PEnX" = "dotted", "PEt" = "dotdash", "EA"="longdash")) + # Added EA just in case
    # scale_shape_manual(name = "TOI Category", values = c(16, 17, 15, 18, 8)) + # Define shapes if using points
    
    # Layer 4: Labels and Theme
    labs(
      title = paste("Sequence Plot Comparison (Recording:", rec_id, ")"),
      subtitle = "Overlay: Fix Dur (Left Axis) & Sacc Amp (Right Axis) for TOI Categories",
      x = "Sequence relative to Key Event"
    ) +
    theme_minimal(base_size = 12) +
    theme(
      panel.grid.minor.x = element_blank(),
      axis.title.y.left = element_text(color = "dodgerblue", size=10),
      axis.text.y.left = element_text(color = "dodgerblue"),
      axis.title.y.right = element_text(color = "firebrick", size=10),
      axis.text.y.right = element_text(color = "firebrick"),
      legend.position = "bottom", # Legend needed to decode linetypes
      legend.box = "vertical",
      legend.key.width = unit(1.5, "cm") # Make legend linetypes easier to see
    )
  
  # --- Print the plot to the R output pane ---
  print(plot_complex_overlay)
  print(paste("Displayed complex overlay plot for recording", rec_id))
  Sys.sleep(1) # Pause briefly so plots don't overwrite too fast
  
} # End of loop
print("--- Finished generating plots ---")

# --- End of Complex Overlay Plot Code ---

########################################################################################################################################################################### D A T A Q U A L I T Y

# --- Check for Unrealistic Values ---

# Make sure libraries are loaded
library(dplyr)
library(ggplot2)

# Use the final processed data BEFORE calculating the final averages
# Assumes ds2_final_processed exists and is up-to-date

print("--- Checking Fixation Durations ---")
# Filter for fixation data
fixation_data_check <- ds2_final_processed %>% filter(Event_type == "Fixation")

if(nrow(fixation_data_check) > 0) {
  print("Summary Statistics for Fixation Duration:")
  print(summary(fixation_data_check$Duration))
  
  # Optional: Count potentially problematic values based on common thresholds
  print(paste("Number of Fixations < 80ms:", sum(fixation_data_check$Duration < 80, na.rm = TRUE)))
  print(paste("Number of Fixations > 1500ms:", sum(fixation_data_check$Duration > 1500, na.rm = TRUE)))
  
  # Histogram for Fixation Duration
  plot_hist_fix_dur <- ggplot(fixation_data_check, aes(x = Duration)) +
    geom_histogram(bins = 50, fill = "dodgerblue", color = "white") +
    labs(title = "Distribution of Fixation Durations", x = "Duration (ms)", y = "Count") +
    theme_minimal()
  print(plot_hist_fix_dur) # Display the histogram
  
  } else {
  print("No fixation data found in ds2_final_processed to check.")
}


print("--- Checking Saccade Amplitudes ---")
# Filter for saccade data
saccade_data_check <- ds2_final_processed %>% filter(Event_type == "Saccade")

if(nrow(saccade_data_check) > 0) {
  print("Summary Statistics for Saccade Amplitude:")
  print(summary(saccade_data_check$Saccade_amplitude))
  
  # Optional: Count potentially problematic values
  # Adjust thresholds based on your setup/expectations
  print(paste("Number of Saccades < 1 degree:", sum(saccade_data_check$Saccade_amplitude < 1, na.rm = TRUE)))
  print(paste("Number of Saccades > 40 degrees:", sum(saccade_data_check$Saccade_amplitude > 40, na.rm = TRUE)))
  
  # Histogram for Saccade Amplitude
  plot_hist_sacc_amp <- ggplot(saccade_data_check, aes(x = Saccade_amplitude)) +
    geom_histogram(bins = 50, fill = "firebrick", color = "white") +
    labs(title = "Distribution of Saccade Amplitudes", x = "Amplitude (degrees)", y = "Count") +
    theme_minimal()
  print(plot_hist_sacc_amp) # Display the histogram
  
} else {
  print("No saccade data found in ds2_final_processed to check.")
}

# --- End of Checks ---




 # O U T L I E R S 

# --- Create Outlier-Filtered Dataset ---

# Make sure libraries are loaded
library(dplyr)

# Start with the fully processed data (EA should already be excluded)
# Assumes ds2_final_processed is available and up-to-date
if (!exists("ds2_final_processed")) {
  stop("Error: 'ds2_final_processed' not found. Please ensure previous steps are complete.")
}

print("--- Applying 3 SD outlier filter per Recording ---")

# 1. Calculate Mean and SD per Recording for Fixation Duration
fixation_stats_per_rec <- ds2_final_processed %>%
  filter(Event_type == "Fixation") %>%
  group_by(Recording) %>%
  summarise(
    mean_dur = mean(Duration, na.rm = TRUE),
    sd_dur = sd(Duration, na.rm = TRUE),
    .groups = 'drop'
  ) %>%
  # Handle recordings with insufficient data to calculate SD (assign infinite bounds)
  mutate(sd_dur = ifelse(is.na(sd_dur) | sd_dur == 0, Inf, sd_dur))

# 2. Calculate Mean and SD per Recording for Saccade Amplitude
saccade_stats_per_rec <- ds2_final_processed %>%
  filter(Event_type == "Saccade") %>%
  group_by(Recording) %>%
  summarise(
    mean_amp = mean(Saccade_amplitude, na.rm = TRUE),
    sd_amp = sd(Saccade_amplitude, na.rm = TRUE),
    .groups = 'drop'
  ) %>%
  mutate(sd_amp = ifelse(is.na(sd_amp) | sd_amp == 0, Inf, sd_amp))

# 3. Join stats back and filter ds2_final_processed
ds2_filtered_outliers <- ds2_final_processed %>%
  left_join(fixation_stats_per_rec, by = "Recording") %>%
  left_join(saccade_stats_per_rec, by = "Recording") %>%
  # Apply the filter conditions
  filter(
    # Keep Fixations within Mean +/- 3*SD of their Recording
    (Event_type == "Fixation" & !is.na(mean_dur) & !is.na(sd_dur) &
       Duration >= (mean_dur - 3 * sd_dur) &
       Duration <= (mean_dur + 3 * sd_dur)) |
      # Keep Saccades within Mean +/- 3*SD of their Recording
      (Event_type == "Saccade" & !is.na(mean_amp) & !is.na(sd_amp) &
         Saccade_amplitude >= (mean_amp - 3 * sd_amp) &
         Saccade_amplitude <= (mean_amp + 3 * sd_amp)) |
      # Keep other event types if they somehow exist
      (!Event_type %in% c("Fixation", "Saccade"))
  ) %>%
  # Remove the temporary stat columns
  select(-any_of(c("mean_dur", "sd_dur", "mean_amp", "sd_amp")))


# --- Check results of filtering ---
print(paste("Rows before 3SD filter:", nrow(ds2_final_processed)))
print(paste("Rows after 3SD filter:", nrow(ds2_filtered_outliers)))
if(nrow(ds2_filtered_outliers) < nrow(ds2_final_processed)){
  print(paste("Removed", nrow(ds2_final_processed) - nrow(ds2_filtered_outliers), "potential outliers."))
} else {
  print("No rows removed by 3SD filter.")
}
print("--- IMPORTANT: Using 'ds2_filtered_outliers' for subsequent calculations/plots ---")

# --- End of Outlier Filtering ---


# --- Calculate Mean & Median per Recording for 4 TOI Categories (Using Filtered Data) ---

# Make sure libraries are loaded
library(dplyr)

# Assumes 'ds2_filtered_outliers' exists from the previous block
# Assumes 'plot_data_prep' exists for mapping TOIs to Categories (or redefine lists)

print("Calculating Mean AND Median averages per recording for 4 TOI categories using FILTERED data...")

# Define TOI lists if not already available
all_fpen_tois <- c("FPEn", "FPEn2", "FPEn3", "FPEn4")
all_pen_tois <- c("PEn", "PEn2", "PEn3", "PEn4")
tois_for_4_groups <- c(all_fpen_tois, all_pen_tois, "FPEt", "PEt")

# Check if input data exists
if (!exists("ds2_filtered_outliers")) {
  stop("Error: 'ds2_filtered_outliers' not found. Please run the outlier filtering code first.")
}
if (!exists("plot_data_prep")) { # plot_data_prep needed for Sequence mapping
  stop("Error: 'plot_data_prep' not found. Needed for Sequence mapping. Please ensure it was created.")
}

# --- Create TOI_Category and merge sequence info ---
# We need to join sequence info from plot_data_prep back to the filtered data
# Requires a unique row identifier or joining on multiple columns.
# Let's recalculate averages directly from plot_data_prep after filtering it.

print("Filtering plot_data_prep to remove outliers...")
# Create a unique ID in the original data used for plot_data_prep if not present
# This assumes plot_data_prep was made from ds2_final_processed originally
# We need IDs to match rows kept in ds2_filtered_outliers
# This approach is getting complex - simpler to recalculate plot_data_prep from filtered data

# --- Alternative: Recreate plot_data_prep from filtered data ---
print("Recreating plot_data_prep using outlier-filtered data...")
plot_data_prep_filtered <- ds2_filtered_outliers %>% # Start with outlier-filtered data
  # Keep relevant columns needed for plotting or filtering
  select(Participant, Recording, TOI, Event_type, Duration, Saccade_amplitude, FixationSequence, SaccadeSequence) %>%
  # Filter for TOIs relevant to sequence plotting (if needed, but sequence exists for all relevant TOIs now)
  filter(TOI %in% tois_for_4_groups) %>% # Filter only for the 4 groups
  # Create unified Sequence and Metric columns
  mutate(
    Sequence = case_when(
      Event_type == "Fixation" ~ FixationSequence,
      Event_type == "Saccade" ~ SaccadeSequence,
      TRUE ~ NA_integer_
    ),
    Metric = case_when(
      Event_type == "Fixation" ~ Duration,
      Event_type == "Saccade" ~ Saccade_amplitude,
      TRUE ~ NA_real_
    )
  ) %>%
  # Filter for the sequence range and valid metrics
  # Use the corrected logic: Seq < 0 for Before, Seq >= 0 for After
  mutate(TOI_Category = factor(case_when( # Define Category first
    TOI %in% all_fpen_tois ~ "Combined_FPEnX",
    TOI %in% all_pen_tois ~ "Combined_PEnX",
    TOI == "FPEt" ~ "FPEt",
    TOI == "PEt" ~ "PEt"
  ), levels = c("Combined_FPEnX", "FPEt", "Combined_PEnX", "PEt"))) %>%
  filter(!is.na(TOI_Category) & !is.na(Sequence) & !is.na(Metric)) %>%
  # Define Before/After based on Category for filtering sequence signs
  mutate(TimeWindow = ifelse(TOI_Category %in% c("Combined_FPEnX", "FPEt"), "Before", "After")) %>%
  filter(
    (TimeWindow == "Before" & Sequence < 0) | # Exclude 0 for Before
      (TimeWindow == "After" & Sequence >= 0)   # Include 0 for After (if present)
  ) %>%
  filter(Sequence >= -5 & Sequence <= 5) %>% # Apply range
  select(Participant, Recording, TOI, TOI_Category, Event_type, Sequence, Metric) # Keep essential columns

print("Dimensions of filtered plot_data_prep:")
print(dim(plot_data_prep_filtered))

# --- Calculate Mean and Median Averages ---
print("Calculating Mean and Median averages per recording...")
avg_med_data_4groups_per_recording <- plot_data_prep_filtered %>%
  # Group by Recording, Category, Event Type, Sequence
  group_by(Recording, TOI_Category, Event_type, Sequence) %>%
  summarise(
    MeanMetric = mean(Metric, na.rm = TRUE), # Calculate Mean
    MedianMetric = median(Metric, na.rm = TRUE), # Calculate Median
    N = n(),
    .groups = 'drop' # Drop grouping
  )

# Check results
if(nrow(avg_med_data_4groups_per_recording) == 0){
  stop("No average/median data generated. Check filters.")
} else {
  print("Mean and Median average data calculation complete.")
  # print(head(avg_med_data_4groups_per_recording))
}
# --- End of Average/Median Calculation ---




# --- Plotting Functions (Modified to Show N) ---

# Make sure ggplot2 is loaded
library(ggplot2)

# Plot Fixation (Mean or Median) for 4 TOI groups, showing N
plot_fix_4groups_rec_stat_N <- function(data, current_rec_id, stat_to_plot = "MeanMetric") {
  metric_label <- ifelse(stat_to_plot == "MeanMetric", "Mean", "Median")
  plot_data <- data %>% filter(Event_type == "Fixation")
  if(nrow(plot_data) == 0 || !stat_to_plot %in% names(plot_data)) return(NULL)
  
  ggplot(plot_data,
         aes(x = Sequence, y = .data[[stat_to_plot]], # Use .data[[]] to use variable name
             color = TOI_Category, linetype = TOI_Category)) +
    geom_line(na.rm = TRUE, linewidth = 1) +
    geom_point(na.rm = TRUE, size = 2.5) +
    # --- ADDED geom_text layer ---
    geom_text(aes(label = N), # Map the 'N' column to the text label
              na.rm = TRUE,      # Don't plot labels for NA points
              vjust = -0.9,      # Adjust vertical position slightly above the point
              hjust = 0.5,      # Center horizontally
              size = 3,          # Adjust text size as needed
              color = "gray30",  # Set text color
              check_overlap = FALSE) + # Allow overlap for now, can set to TRUE if needed
    # --- End of added layer ---
    scale_x_continuous(breaks = -5:5, limits = c(-5.5, 5.5)) +
    scale_color_brewer(palette = "Dark2", name = "TOI Group") +
    scale_linetype_manual(values=c("solid", "dashed", "dotted", "dotdash"), name="TOI Group") +
    labs(title = paste("Fixation Duration Sequence (Recording:", current_rec_id, ")"),
         subtitle = paste("Comparing TOI Categories using", metric_label, "(N shown per point)"), # Updated subtitle
         x = "Sequence relative to Key Event", y = paste(metric_label,"Duration (ms)")) +
    theme_minimal(base_size = 12) +
    theme(legend.position = "bottom")
}

# Plot Saccade (Mean or Median) for 4 TOI groups, showing N
plot_sacc_4groups_rec_stat_N <- function(data, current_rec_id, stat_to_plot = "MeanMetric") {
  metric_label <- ifelse(stat_to_plot == "MeanMetric", "Mean", "Median")
  plot_data <- data %>% filter(Event_type == "Saccade")
  if(nrow(plot_data) == 0 || !stat_to_plot %in% names(plot_data)) return(NULL)
  
  ggplot(plot_data,
         aes(x = Sequence, y = .data[[stat_to_plot]], color = TOI_Category, linetype = TOI_Category)) +
    geom_line(na.rm = TRUE, linewidth = 1) +
    geom_point(na.rm = TRUE, size = 2.5) +
    # --- ADDED geom_text layer ---
    geom_text(aes(label = N), # Map the 'N' column to the text label
              na.rm = TRUE,
              vjust = -0.9,      # Adjust vertical position
              hjust = 0.5,      # Center horizontally
              size = 3,          # Adjust text size
              color = "gray30", # Set text color
              check_overlap = FALSE) +
    # --- End of added layer ---
    scale_x_continuous(breaks = -5:5, limits = c(-5.5, 5.5)) +
    scale_color_brewer(palette = "Dark2", name = "TOI Group") +
    scale_linetype_manual(values=c("solid", "dashed", "dotted", "dotdash"), name="TOI Group") +
    labs(title = paste("Saccade Amplitude Sequence (Recording:", current_rec_id, ")"),
         subtitle = paste("Comparing TOI Categories using", metric_label,"(N shown per point)"), # Updated subtitle
         x = "Sequence relative to Key Event", y = paste(metric_label, "Amplitude (degrees)")) +
    theme_minimal(base_size = 12) +
    theme(legend.position = "bottom")
}
# --- End of Modified Plotting Functions ---

# --- Loop Through Recordings and PRINT Plots (Median and Mean, WITH N) ---

# Assumes avg_med_data_4groups_per_recording exists from response #87 calculation step
if (!exists("avg_med_data_4groups_per_recording")) {
  stop("Error: 'avg_med_data_4groups_per_recording' not found. Please run the calculation step first (from response #87).")
}

# Get Recording List from the averaged data
recording_list_final_N <- unique(avg_med_data_4groups_per_recording$Recording)
print(paste("Found", length(recording_list_final_N), "recordings to generate plots for."))

print("--- Generating MEDIAN plots per recording (with N) ---")
for(rec_id in recording_list_final_N) {
  print(paste("--- Processing recording (Median):", rec_id, "---"))
  recording_avg_med_data <- avg_med_data_4groups_per_recording %>% filter(Recording == rec_id)
  
  if(nrow(recording_avg_med_data) > 0) {
    # Call the NEW plotting functions ending in _N
    p_fix_med <- plot_fix_4groups_rec_stat_N(recording_avg_med_data, rec_id, stat_to_plot = "MedianMetric")
    p_sacc_med <- plot_sacc_4groups_rec_stat_N(recording_avg_med_data, rec_id, stat_to_plot = "MedianMetric")
    if(!is.null(p_fix_med)) { print(p_fix_med); print("Displayed Median Fixation plot (with N).") } else { print("No median fixation data.")}
    if(!is.null(p_sacc_med)) { print(p_sacc_med); print("Displayed Median Saccade plot (with N).") } else { print("No median saccade data.")}
    Sys.sleep(0.5)
  } else { print(paste("  Skipping recording", rec_id)) }
}

print("--- Generating MEAN plots per recording (using FILTERED data, with N) ---")
for(rec_id in recording_list_final_N) {
  print(paste("--- Processing recording (Mean):", rec_id, "---"))
  recording_avg_med_data <- avg_med_data_4groups_per_recording %>% filter(Recording == rec_id)
  
  if(nrow(recording_avg_med_data) > 0) {
    # Call the NEW plotting functions ending in _N
    p_fix_mean <- plot_fix_4groups_rec_stat_N(recording_avg_med_data, rec_id, stat_to_plot = "MeanMetric")
    p_sacc_mean <- plot_sacc_4groups_rec_stat_N(recording_avg_med_data, rec_id, stat_to_plot = "MeanMetric")
    if(!is.null(p_fix_mean)) { print(p_fix_mean); print("Displayed Mean Fixation plot (with N).") } else { print("No mean fixation data.")}
    if(!is.null(p_sacc_mean)) { print(p_sacc_mean); print("Displayed Mean Saccade plot (with N).") } else { print("No mean saccade data.")}
    Sys.sleep(0.5)
  } else { print(paste("  Skipping recording", rec_id)) }
}
print("--- Finished generating plots ---")

# --- End of Plotting Code with N ---



# --- Generate Overall Sequence Plots (Across All Rec/Part), comparing 4 TOI Groups ---

# Make sure libraries are loaded
library(dplyr)
library(ggplot2)

# --- 1. Calculate Overall Averages & Medians for 4 TOI Categories ---
# Assumes 'plot_data_prep_filtered' exists from response #87, step 2
# This data is outlier-filtered and has correct sequence signs enforced

print("Calculating overall Mean AND Median averages for 4 TOI categories...")

# Check if input data exists
if (!exists("plot_data_prep_filtered")) {
  stop("Error: 'plot_data_prep_filtered' not found. Please run the code to create it first (e.g., from response #87, section 2).")
}

# Calculate averages AND medians across all recordings and participants
avg_med_data_4groups_overall <- plot_data_prep_filtered %>%
  # Group by TOI_Category, Event_type, Sequence ONLY for overall aggregation
  group_by(TOI_Category, Event_type, Sequence) %>%
  summarise(
    MeanMetric = mean(Metric, na.rm = TRUE),    # Calculate Mean
    MedianMetric = median(Metric, na.rm = TRUE), # Calculate Median
    N = n(), # Total observations across all recordings/participants
    .groups = 'drop' # Drop grouping
  ) %>%
  # Ensure TOI_Category factor levels are set for plotting order
  mutate(TOI_Category = factor(TOI_Category, levels = c("Combined_FPEnX", "FPEt", "Combined_PEnX", "PEt")))

# Check results
if(nrow(avg_med_data_4groups_overall) == 0){
  stop("No overall average/median data generated. Check input 'plot_data_prep_filtered'.")
} else {
  print("Overall Mean and Median average data calculation complete.")
  # print(head(avg_med_data_4groups_overall))
}


# --- 2. Plotting Functions (Adapted for Overall Data, Showing N) ---

# Plot Fixation (Mean or Median) for 4 TOI groups - OVERALL VERSION
plot_fix_4groups_overall_stat_N <- function(overall_avg_data, stat_to_plot = "MeanMetric") {
  metric_label <- ifelse(stat_to_plot == "MeanMetric", "Mean", "Median")
  # Filter for fixation data
  plot_data <- overall_avg_data %>% filter(Event_type == "Fixation")
  # Check if data exists for this metric
  if(nrow(plot_data) == 0 || !stat_to_plot %in% names(plot_data)) {
    warning(paste("No Overall", metric_label,"Fixation data to plot."), call. = FALSE)
    return(NULL)
  }
  
  ggplot(plot_data,
         aes(x = Sequence, y = .data[[stat_to_plot]], # Use .data[[...]]
             color = TOI_Category, linetype = TOI_Category)) +
    geom_line(na.rm = TRUE, linewidth = 1) +
    geom_point(na.rm = TRUE, size = 2.5) +
    geom_text(aes(label = N), na.rm = TRUE, vjust = -0.9, hjust = 0.5, size = 3, color = "gray30", check_overlap = TRUE) +
    scale_x_continuous(breaks = -5:5, limits = c(-5.5, 5.5)) + # Include relevant breaks
    scale_color_brewer(palette = "Dark2", name = "TOI Group") +
    scale_linetype_manual(values=c("solid", "dashed", "dotted", "dotdash"), name="TOI Group") +
    labs(title = paste("Overall Fixation Duration Sequence (Across All Recordings)"),
         subtitle = paste("Comparing TOI Categories using", metric_label, "(N shown per point)"),
         x = "Sequence relative to Key Event", y = paste(metric_label,"Duration (ms)")) +
    theme_minimal(base_size = 12) +
    theme(legend.position = "bottom")
}

# Plot Saccade (Mean or Median) for 4 TOI groups - OVERALL VERSION
plot_sacc_4groups_overall_stat_N <- function(overall_avg_data, stat_to_plot = "MeanMetric") {
  metric_label <- ifelse(stat_to_plot == "MeanMetric", "Mean", "Median")
  # Filter for saccade data
  plot_data <- overall_avg_data %>% filter(Event_type == "Saccade")
  # Check if data exists for this metric
  if(nrow(plot_data) == 0 || !stat_to_plot %in% names(plot_data)) {
    warning(paste("No Overall", metric_label,"Saccade data to plot."), call. = FALSE)
    return(NULL)
  }
  
  ggplot(plot_data,
         aes(x = Sequence, y = .data[[stat_to_plot]], color = TOI_Category, linetype = TOI_Category)) +
    geom_line(na.rm = TRUE, linewidth = 1) +
    geom_point(na.rm = TRUE, size = 2.5) +
    geom_text(aes(label = N), na.rm = TRUE, vjust = -0.9, hjust = 0.5, size = 3, color = "gray30", check_overlap = TRUE) +
    scale_x_continuous(breaks = -5:5, limits = c(-5.5, 5.5)) +
    scale_color_brewer(palette = "Dark2", name = "TOI Group") +
    scale_linetype_manual(values=c("solid", "dashed", "dotted", "dotdash"), name="TOI Group") +
    labs(title = paste("Overall Saccade Amplitude Sequence (Across All Recordings)"),
         subtitle = paste("Comparing TOI Categories using", metric_label,"(N shown per point)"),
         x = "Sequence relative to Key Event", y = paste(metric_label, "Amplitude (degrees)")) +
    theme_minimal(base_size = 12) +
    theme(legend.position = "bottom")
}

# --- 3. Generate and Print Overall Plots (Median and Mean with N) ---

print("--- Generating OVERALL MEDIAN plots (across all recordings/participants) ---")
# Create the Median plots using the overall data
p_fix_overall_med <- plot_fix_4groups_overall_stat_N(avg_med_data_4groups_overall, stat_to_plot = "MedianMetric")
p_sacc_overall_med <- plot_sacc_4groups_overall_stat_N(avg_med_data_4groups_overall, stat_to_plot = "MedianMetric")

# Print the Median plots if they were created
if(!is.null(p_fix_overall_med)) print(p_fix_overall_med)
if(!is.null(p_sacc_overall_med)) print(p_sacc_overall_med)


print("--- Generating OVERALL MEAN plots (using FILTERED data, across all recordings/participants) ---")
# Create the Mean plots using the overall data
p_fix_overall_mean <- plot_fix_4groups_overall_stat_N(avg_med_data_4groups_overall, stat_to_plot = "MeanMetric")
p_sacc_overall_mean <- plot_sacc_4groups_overall_stat_N(avg_med_data_4groups_overall, stat_to_plot = "MeanMetric")

# Print the Mean plots if they were created
if(!is.null(p_fix_overall_mean)) print(p_fix_overall_mean)
if(!is.null(p_sacc_overall_mean)) print(p_sacc_overall_mean)

# --- End of Overall Plot Generation ---

# --- Analyze Duration of Sequence 0 Event ---

# Make sure libraries are loaded
library(dplyr)
library(ggplot2)

# --- 1. Prepare Data for Sequence 0 Events ---
# Assumes 'ds2_sequenced_unified' exists from Step 2 of the pipeline
# (containing FixationSequence and SaccadeSequence columns)

print("--- Preparing data for Sequence 0 events ---")
if (!exists("ds2_sequenced_unified")) {
  stop("Error: 'ds2_sequenced_unified' dataframe not found. Please re-run Step 1 and Step 2 of the pipeline.")
}

# Create unified sequence, filter for Sequence == 0, keep relevant columns
# NOTE: This uses the 'Duration' column for ANY event type at Sequence 0.
sequence_0_data <- ds2_sequenced_unified %>%
  # Create unified Sequence column (handles both fixations and saccades)
  mutate(
    Sequence = case_when(
      !is.na(FixationSequence) ~ FixationSequence,
      !is.na(SaccadeSequence) ~ SaccadeSequence,
      TRUE ~ NA_integer_ # Assign NA if neither sequence exists for a row
    )
  ) %>%
  # Filter for Sequence 0 and ensure Duration is a valid number
  filter(Sequence == 0, !is.na(Duration), is.finite(Duration)) %>%
  # Select columns needed for this analysis
  select(Recording, Participant, Interval, TOI, Event_type, Duration)

# Check if any Sequence 0 data was found
if(nrow(sequence_0_data) == 0) {
  stop("No valid events with Sequence == 0 and finite Duration found in 'ds2_sequenced_unified'. Cannot proceed.")
} else {
  print(paste("Found", nrow(sequence_0_data), "events with Sequence == 0 and valid Duration."))
  print("Event types found at Sequence 0:")
  # Show the breakdown of event types at Sequence 0
  print(table(sequence_0_data$Event_type, useNA = "ifany"))
}


# --- 2. Calculate Average Duration per Recording ---
print("--- Calculating Average Duration of Sequence 0 Event per Recording ---")
avg_dur_seq0_per_recording <- sequence_0_data %>%
  group_by(Recording) %>%
  summarise(
    AvgDuration_Seq0 = mean(Duration, na.rm = TRUE),
    N_Seq0_Events = n(), # Count of Seq 0 events included in the average
    .groups = 'drop'
  ) %>%
  arrange(Recording) # Optional: arrange alphabetically

# Display the table
print(avg_dur_seq0_per_recording, n = Inf) # n=Inf shows all recordings


# --- 3. Calculate Overall Average Duration ---
print("--- Calculating Overall Average Duration of Sequence 0 Event ---")
avg_dur_seq0_overall <- sequence_0_data %>%
  summarise(
    OverallAvgDuration_Seq0 = mean(Duration, na.rm = TRUE),
    Total_N_Seq0_Events = n()
  )

# Display the overall average
print(avg_dur_seq0_overall)


# --- 4. Create Plot per Recording ---
print("--- Generating Plot of Average Duration of Sequence 0 Event per Recording ---")
plot_avg_dur_seq0_per_rec <- ggplot(avg_dur_seq0_per_recording,
                                    aes(x = Recording, y = AvgDuration_Seq0)) +
  # Use geom_col for bar chart
  geom_col(fill = "steelblue", color = "black", alpha = 0.8) +
  # Add text labels showing the count (N) above each bar
  geom_text(aes(label = N_Seq0_Events), vjust = -0.5, size = 3) +
  # Add a horizontal line for the overall average
  geom_hline(yintercept = avg_dur_seq0_overall$OverallAvgDuration_Seq0,
             linetype = "dashed", color = "red") +
  # Add text annotation for the overall average line
  annotate("text", x = Inf, y = avg_dur_seq0_overall$OverallAvgDuration_Seq0,
           label = paste("Overall Avg =", round(avg_dur_seq0_overall$OverallAvgDuration_Seq0, 1)),
           hjust = 1.1, vjust = -0.5, color = "red", size = 3.5) +
  labs(
    title = "Average Duration of Sequence 0 Event by Recording",
    subtitle = paste("N shown above bars. Dashed line = Overall Avg Duration (",
                     round(avg_dur_seq0_overall$OverallAvgDuration_Seq0, 1), " ms)", sep=""),
    x = "Recording",
    y = "Average Duration (ms)"
  ) +
  theme_minimal(base_size = 12) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) # Rotate x-axis labels

# Print the plot to the R output pane
print(plot_avg_dur_seq0_per_rec)

# --- End of Sequence 0 Duration Analysis ---

# --- Displaying the Sequence 0 Event Data in Detail ---

# Make sure dplyr is loaded
library(dplyr)

# Assumes 'ds2_sequenced_unified' exists from Step 2 of the pipeline.
if (!exists("ds2_sequenced_unified")) {
  stop("Error: 'ds2_sequenced_unified' dataframe not found. Please re-run Step 1 and Step 2 of the pipeline.")
}

print("--- Preparing detailed data for Sequence 0 events ---")

# Re-filter ds2_sequenced_unified to get Sequence 0 events, selecting more columns
sequence_0_data_detailed <- ds2_sequenced_unified %>%
  # Create unified Sequence column (handles both fixations and saccades)
  mutate(
    Sequence = case_when(
      !is.na(FixationSequence) ~ FixationSequence,
      !is.na(SaccadeSequence) ~ SaccadeSequence,
      TRUE ~ NA_integer_ # Assign NA if neither sequence exists for a row
    )
  ) %>%
  # Filter for Sequence 0 and ensure Duration is a valid number
  filter(Sequence == 0, !is.na(Duration), is.finite(Duration)) %>%
  # Select useful columns for inspection
  select(
    Recording, Participant, Interval, TOI,   # Identifiers
    Event_type, Start, Stop, Duration, Validity, # Event details
    FixationSequence, SaccadeSequence, Sequence, # Sequence numbers
    KeyEventStart, KeyEventStop, KeyEventType # Key event context
  ) %>%
  # Arrange for easier reading
  arrange(Recording, Participant, Interval, TOI, Start)

# Check if any data was found
if(nrow(sequence_0_data_detailed) == 0) {
  print("No events meeting Sequence == 0 criteria found in 'ds2_sequenced_unified'.")
} else {
  print(paste("Found", nrow(sequence_0_data_detailed), "events with Sequence == 0 and valid Duration."))
  print("--- Displaying first 50 rows of Sequence 0 events: ---")
  
  # Option 1: Print to console (first 50 rows)
  print(sequence_0_data_detailed, n = 50)
  
  # Option 2: Use RStudio Viewer for interactive Browse (RECOMMENDED if many rows)
  # View(sequence_0_data_detailed)
  # If using View(), you might want to comment out the print() line above.
  
  print("--- ---")
  print("Consider using View(sequence_0_data_detailed) in RStudio for interactive exploration.")
}

# --- End of Display Code ---