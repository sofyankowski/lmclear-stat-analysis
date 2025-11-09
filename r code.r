> 
#Sample size calculation
# Parameters
sigma_d <- 2.97
mu_d <- 0.5
alpha <- 0.05
power <- 0.80
n_questions <- 5

cohen_d <- mu_d / sigma_d

# Manual
Z_alpha <- qnorm(1 - alpha / 2)
Z_beta <- qnorm(power)
n_manual_h1 <- ((Z_alpha + Z_beta)^2 * sigma_d^2) / mu_d^2

print("Method 1: Manual")
print(paste("Total n =", ceiling(n_manual_h1)))

# power.t.test
result_base_h1 <- power.t.test(
  delta = mu_d,
  sd = sigma_d,
  sig.level = alpha,
  power = power,
  type = "paired",
  alternative = "two.sided"
)

# Quantitative outcome analysis
# Load required packages
> if (!require("lmerTest")) install.packages("lmerTest")
> library(lmerTest)
> library(dplyr)
> library(emmeans)
> library(effectsize)
> 
> # INT function
> invNormTrans <- function(x) {
+   qnorm((rank(x) - 0.5) / length(x))
+ }
> 
> outcomes <- c("public", "satisfied", "reassuring", "AI", "trust")
> outcomes <- c("public", "satisfied")
> 
> int_results <- list()
> 
> for (outcome_type in outcomes) {
+   cat(sprintf("\n\n========== OUTCOME: %s ==========\n", toupper(outcome_type)))
+   outcome_data <- data_qq_long %>% filter(outcome == outcome_type)
+   outcome_data$score <- as.numeric(outcome_data$score)
+   outcome_data$int_score <- invNormTrans(outcome_data$score)
+   
+   # Base INT model
+   m_base <- lmer(int_score ~ author * question + (1|participant_id), data = outcome_data)
+   
+   # Model with age, sex, country
+   m_predet <- lmer(int_score ~ author * question + age + sex + country + (1|participant_id), data = outcome_data)
+   
+   # Model comparison
+   cat("\n--- INT Model Comparison ---\n")
+   print(anova(m_base, m_predet))
+   cat("\nAIC:\n"); print(AIC(m_base, m_predet))
+   cat("\nBIC:\n"); print(BIC(m_base, m_predet))
+   
+   # Model summary and effect sizes
+   cat("\n--- INT Model Summary ---\n")
+   print(summary(m_predet))
+   cat("\n--- Effect Sizes ---\n")
+   print(eta_squared(m_predet))
+   
+   # Post-hoc for author and interaction
+   cat("\n--- Post-hoc: Author ---\n")
+   emm_author <- emmeans(m_predet, ~ author)
+   print(emm_author)
+   print(pairs(emm_author))
+   
+   cat("\n--- Post-hoc: Author | Question ---\n")
+   emm_interaction <- emmeans(m_predet, ~ author | question)
+   print(emm_interaction)
+   print(pairs(emm_interaction))
+   
+   # Store results
+   int_results[[outcome_type]] <- list(
+     base_model = m_base,
+     predet_model = m_predet,
+     summary = summary(m_predet)
+   )
+ }

#Qualitative outcome analysis
library(readxl)
library(dplyr)
library(tidyr)
library(stringr)
library(openxlsx)

DT45_clean <- read.csv("C:/Users/sofya/OneDrive/Documents/DT45_transformed_v3_clean_with_ids.csv", header = TRUE)

mapping_table_2 <- data.frame(
  id = c(1, 2, 3, 5, 6, 7, 8, 9, 10, 11, 12, 13, 14, 15, 16, 17, 18, 19, 20, 21, 24, 25, 26, 27, 28, 29, 30, 31, 32, 34, 35, 36, 37, 38, 39),
  author = c("GPT", "GPT", "GPT", "GPT", "GPT", "GPT", "GPT", "GPT", "GPT", "GPT", "GPT", "GPT", "GPT", "Human", "Human", "Human", "Human", "Human", "Human", "Human", "Human", "Human", "Human", "Human", "Human", "Human", "Neutral", "Neutral", "Neutral", "GPT", "Human", "Neutral", "Neutral", "Neutral", "Neutral"),
  theme = c("1-Comprehensibility & Style", "1-Comprehensibility & Style", "2-Information Depth & Rigor", "1-Comprehensibility & Style", "1-Comprehensibility & Style", "2-Information Depth & Rigor", "2-Information Depth & Rigor", "2-Information Depth & Rigor", "3-Humanity, Empathy & Trust", "3-Humanity, Empathy & Trust", "3-Humanity, Empathy & Trust", "3-Humanity, Empathy & Trust", "4-Global Satisfaction", "1-Comprehensibility & Style", "1-Comprehensibility & Style", "2-Information Depth & Rigor", "2-Information Depth & Rigor", "1-Comprehensibility & Style", "1-Comprehensibility & Style", "2-Information Depth & Rigor", "2-Information Depth & Rigor", "3-Humanity, Empathy & Trust", "3-Humanity, Empathy & Trust", "3-Humanity, Empathy & Trust", "3-Humanity, Empathy & Trust", "4-Global Satisfaction", "5-Patient Questions", "5-Patient Questions", "5-Patient Questions", "4-Global Satisfaction", "4-Global Satisfaction", "6-Meta-Commentary & Feedback", "6-Meta-Commentary & Feedback", "6-Meta-Commentary & Feedback", "6-Meta-Commentary & Feedback"),
  reference_subtheme = c("Clarity", "Clarity", "Detail/depth", "Structure", "Structure", "Detail/depth", "Detail/depth", "Scientific references", "Empathy/humanity", "Reassurance", "Reassurance", "Trust/credibility", "Global satisfaction", "Clarity", "Clarity", "Detail/depth", "Detail/depth", "Structure", "Structure", "Detail/depth", "Scientific references", "Empathy/humanity", "Reassurance", "Reassurance", "Trust/credibility", "Global satisfaction", "Medical-questions", "Medical-questions", "Medical-questions", "Global satisfaction", "Global satisfaction", "Meta-commentary", "General-feedback", "General-feedback", "Unclassified"),
  title = c("Clarity: Clear (G)", "Clarity: Unclear (G)", "Detail/depth: Too long (G)", "Structure: Poorly structured (G)", "Structure: Well structured (G)", "Detail/depth: Adequate (G)", "Detail/depth: Too superficial (G)", "Scientific references absent (G)", "Empathy/humanity: Impersonal (G)", "Reassurance: Reassuring (G)", "Reassurance: Worrying (G)", "Trust/credibility: Trustworthy (G)", "Satisfaction (G)", "Clarity: Clear (H)", "Clarity: Unclear (H)", "Detail/depth: Adequate (H)", "Detail/depth: Too long (H)", "Structure: Poorly structured (H)", "Structure: Well structured (H)", "Detail/depth: Too superficial (H)", "Scientific references absent (H)", "Empathy/humanity: Impersonal (H)", "Reassurance: Reassuring (H)", "Reassurance: Worrying (H)", "Trust/credibility: Doubtful (H)", "Satisfaction (H)", "Cancer risk in radiology", "Contrast media", "Dose", "Insatisfaction (G)", "Insatisfaction (H)", "Meta-commentary on responses", "General positive feedback", "General negative feedback", "Unclassified comment"),
  quirkID = c(41, 113, 123, 19, 33, 122, 49, 43, 23, 67, 20, 70, 6, 35, 32, 120, 121, 9, 98, 4, 2, 73, 61, 47, 44, 38, 24, 51, 39, 111, 112, 114, 115, 116, 117),
  code_5chars = c("CC1PG", "CC2NG", "ID3NG", "CS2NG", "CS1PG", "ID1PG", "ID2NG", "IF1NG", "EE1NG", "ER1PG", "ER2NG", "ET1PG", "GG1PG", "CC1PH", "CC2NH", "ID1PH", "ID3NH", "CS2NH", "CS1PH", "ID2NH", "IF1NH", "EE1NH", "ER1PH", "ER2NH", "ET2NH", "GG1PH", "QQ1CN", "QQ3CN", "QQ2CN", "GG2NG", "GG2NH", "QX1CN", "QX2CN", "QX3CN", "QX4CN"),
  valence = c("Positive", "Negative", "Negative", "Negative", "Positive", "Positive", "Negative", "Negative", "Negative", "Positive", "Negative", "Positive", "Positive", "Positive", "Negative", "Positive", "Negative", "Negative", "Positive", "Negative", "Negative", "Negative", "Positive", "Negative", "Negative", "Positive", "Category (Neutral)", "Category (Neutral)", "Category (Neutral)", "Negative", "Negative", "Category (Neutral)", "Category (Neutral)", "Category (Neutral)", "Category (Neutral)")
)

DT45_clean <- DT45_clean %>%
  mutate(
    code_so08_v3_clean = case_when(
      is.na(code_so08_v3_clean) | code_so08_v3_clean == "" ~ intermediaire45_code_5chars_v3_clean,
      TRUE ~ code_so08_v3_clean
    )
  )

extract_individual_codes <- function(df) {
  df %>%
    filter(!is.na(code_so08_v3_clean)) %>%
    separate_rows(code_so08_v3_clean, sep = "---") %>%
    mutate(code_so08_v3_clean = str_trim(code_so08_v3_clean)) %>%
    filter(code_so08_v3_clean != "" & !is.na(code_so08_v3_clean))
}

individual_codes <- extract_individual_codes(DT45_clean)

code_counts <- individual_codes %>%
  count(code_so08_v3_clean, name = "count_in_data") %>%
  arrange(desc(count_in_data))

mapping_with_counts <- mapping_table_2 %>%
  left_join(code_counts, by = c("code_5chars" = "code_so08_v3_clean")) %>%
  mutate(count_in_data = ifelse(is.na(count_in_data), 0, count_in_data)) %>%
  select(id, author, theme, reference_subtheme, title, code_5chars, valence, count_in_data) %>%
  arrange(desc(count_in_data))

codes_used <- mapping_with_counts %>%
  filter(count_in_data > 0) %>%
  arrange(theme, reference_subtheme)

code_to_metadata <- mapping_table_2 %>%
  mutate(
    category = str_replace_all(title, "\\s*\\([GH]\\)", ""),
    category = str_replace_all(category, "^[^:]+:\\s*", ""),
    category = str_trim(category),
    valence_simple = case_when(
      valence == "Positive" ~ "Positive",
      valence == "Negative" ~ "Negative", 
      TRUE ~ "Neutral"
    )
  ) %>%
  distinct(code_5chars, .keep_all = TRUE) %>%
  select(code_5chars, theme, reference_subtheme, category, valence_simple)

individual_codes_with_metadata <- individual_codes %>%
  left_join(code_to_metadata, by = c("code_so08_v3_clean" = "code_5chars"))

individual_codes_mapped <- individual_codes_with_metadata %>%
  filter(!is.na(theme))

table_4_counts <- individual_codes_mapped %>%
  count(theme, reference_subtheme, category, valence_simple, section) %>%
  pivot_wider(names_from = section, values_from = n, values_fill = 0) %>%
  rename(
    Theme = theme,
    Subtheme = reference_subtheme, 
    Category = category,
    Valence = valence_simple
  )

if(!"human" %in% names(table_4_counts)) table_4_counts$human <- 0
if(!"gpt" %in% names(table_4_counts)) table_4_counts$gpt <- 0
if(!"further_q" %in% names(table_4_counts)) table_4_counts$further_q <- 0

table_4_final_all_themes <- table_4_counts %>%
  select(Theme, Subtheme, Category, Valence, 
         Human = human, ChatGPT = gpt, Neutral = further_q) %>%
  mutate(Total = Human + ChatGPT + Neutral) %>%
  arrange(Theme, Subtheme, Category)

table_4_final_classic <- table_4_counts %>%
  select(Theme, Subtheme, Category, Valence, 
         Human = human, ChatGPT = gpt) %>%
  mutate(Total = Human + ChatGPT) %>%
  filter(Total > 0) %>%
  arrange(Theme, Subtheme, Category)

write.csv(table_4_final_all_themes, "Table_4_SO08_all_themes.csv", row.names = FALSE)
write.csv(table_4_final_classic, "Table_4_SO08_classic.csv", row.names = FALSE)
write.csv(codes_used, "codes_usedSO08.csv", row.names = FALSE)
