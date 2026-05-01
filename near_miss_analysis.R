# АНАЛИЗ NEAR MISS

library(tidyverse)
library(readxl)
library(lme4)
library(lmerTest)
library(broom)
library(broom.mix)
library(writexl)

PATH <- "/Users/aleksandrafilimonova/Desktop/учёба/диплом/эксперимент/анализ данных/all_users_bets_FINAL_itog.xlsx"

bets_raw <- read_excel(PATH, sheet = "Все ставки")

# =============================================================================
# 1. ФОРМИРОВАНИЕ ДАТАСЕТА NEAR MISS
# =============================================================================

nm_raw <- bets_raw %>%
  filter(`Матч для near miss` == "+") %>%
  transmute(
    user_id    = `ID пользователя`,
    match      = `Матч`,
    match_file = `Файл матча`,
    score_bet  = `Ставка счёт (K1:K2)`,
    goals1_bet = `Голы К1`,
    goals2_bet = `Голы К2`,
    score_real = `Реальный счёт матча`,
    won        = as.integer(`Ставка выиграла_`),
    
    # Расстояние от ставки до реального счёта
    real1      = as.integer(str_extract(score_real, "^\\d+")),
    real2      = as.integer(str_extract(score_real, "\\d+$")),
    score_dist = abs(goals1_bet - real1) + abs(goals2_bet - real2),
    
    # Опросные переменные
    q1_raw     = as.integer(`Вопрос 1`),   # 1= близко, 3=далеко
    q2_raw     = as.integer(`Вопрос 2`),   # 1=больше, 2=столько же, 3=меньше
    q3_raw     = as.character(`Вопрос 3`), 
    other_text = `4 (другое)`,
    
    # Демо и дизайн
    design         = as.integer(`Дизайн (трактмент)`),
    confidence     = as.numeric(`Уверенность (1-6)`),
    coef           = as.numeric(`Коэффициент`),
    amount         = as.numeric(`Сумма ставки`),
    time_on_page   = as.numeric(`Время на стр. (сек)`),
    gender         = `Пол`,
    age            = `Возраст`,
    betting_freq   = as.numeric(`Частота ставок (числ.)`),
    interess       = as.numeric(`Интерес к футболу (1-5)`),
    self_conf      = as.numeric(`Самооценка (1-5)`),
    impulsivity    = as.numeric(`Индекс импульсивности`),
    risk_q2        = as.numeric(`Риск Q2`)
  ) %>%
  mutate(
    treatment    = factor(design, levels = c(0, 1),
                          labels = c("control", "treatment")),
    gender_num   = ifelse(gender == "Мужской", 1L, 0L),
    
    q3_clean = as.integer(str_extract(q3_raw, "^\\d")),
    
    nm_close     = 4L - q1_raw,   # 3= близко, 1=далеко
    
    # Бинарные исходы
    escalation   = as.integer(q2_raw == 1),  #ставить больше
    chance_attr  = as.integer(q3_clean == 1), #случайность
    skill_attr   = as.integer(q3_clean %in% c(2L, 3L)),
    
    # Факторные версии для ordinal models
    q1_ord  = factor(q1_raw,  levels = 1:3,
                     labels = c("Очень близко","Довольно близко","Не близко")),
    q2_ord  = factor(q2_raw,  levels = 1:3,
                     labels = c("Больше","Столько же","Меньше/нет")),
    q3_fct  = factor(q3_clean, levels = 1:4,
                     labels = c("Случайность","Форма","Составы","Другое")),
    
    # Центрирование непрерывных ковариат
    conf_c   = confidence - mean(confidence, na.rm = TRUE),
    freq_c   = betting_freq - mean(betting_freq, na.rm = TRUE),
    inter_c  = interess - mean(interess, na.rm = TRUE),
    self_c   = self_conf - mean(self_conf, na.rm = TRUE),
    impuls_c = impulsivity - mean(impulsivity, na.rm = TRUE),
    risk_c   = risk_q2 - mean(risk_q2, na.rm = TRUE)
  )

# Проверки
cat("=== NEAR MISS ДАТАСЕТ ===\n")
cat("N строк:", nrow(nm_raw), "\n")
cat("N участников:", n_distinct(nm_raw$user_id), "\n")
cat("Все проиграли (won=0)?", all(nm_raw$won == 0, na.rm = TRUE), "\n")
cat("Dist = 1 у всех?",
    all(nm_raw$score_dist == 1, na.rm = TRUE), "\n\n")

cat("Пропуски в ключевых переменных:\n")
nm_raw %>%
  select(q1_raw, q2_raw, q3_clean, nm_close, escalation, chance_attr) %>%
  summarise(across(everything(), ~sum(is.na(.)))) %>%
  pivot_longer(everything(), names_to = "var", values_to = "n_na") %>%
  print()

# Рабочий датасет — только те, кто ответил на вопросы
nm <- nm_raw %>%
  filter(!is.na(q1_raw), !is.na(q2_raw), !is.na(q3_clean))

cat("\nПосле фильтрации (ответили на все 3 вопроса):", nrow(nm), "участников\n")

# 2. ОПИСАТЕЛЬНАЯ СТАТИСТИКА

cat("\n=== ОПИСАТЕЛЬНАЯ СТАТИСТИКА ===\n")

# Распределение ответов
cat("\n--- Q1: насколько близко к успеху? ---\n")
print(nm %>% count(q1_ord) %>% mutate(pct = n / sum(n) * 100))

cat("\n--- Q2: что сделаешь в следующем туре? ---\n")
print(nm %>% count(q2_ord) %>% mutate(pct = n / sum(n) * 100))

cat("\n--- Q3: что повлияло на проигрыш? ---\n")
print(nm %>% count(q3_fct) %>% mutate(pct = n / sum(n) * 100))

# По условиям эксперимента
cat("\n--- По дизайну ---\n")
nm %>%
  group_by(treatment) %>%
  summarise(
    n           = n(),
    mean_q1     = mean(q1_raw, na.rm = TRUE),
    sd_q1       = sd(q1_raw, na.rm = TRUE),
    mean_nm     = mean(nm_close, na.rm = TRUE),
    pct_escalate  = mean(escalation, na.rm = TRUE) * 100,
    pct_chance    = mean(chance_attr, na.rm = TRUE) * 100,
    pct_skill     = mean(skill_attr, na.rm = TRUE) * 100,
    .groups = "drop"
  ) %>% print()

cat("\n--- Q1 × Q2 (near-miss perception × эскалация) ---\n")
print(table(nm$q1_ord, nm$q2_ord))
cat("Chi-squared Q1 × Q2:\n")
print(chisq.test(nm$q1_raw, nm$q2_raw))

cat("\n--- Q1 × Q3 ---\n")
print(table(nm$q1_ord, nm$q3_fct))

# 3. NEAR MISS COMPOSITE SCORE  

nm <- nm %>%
  mutate(
    nm_close_z  = scale(nm_close)[, 1],   
    escalation_z = scale(escalation)[, 1], 
    nm_score    = (nm_close_z + escalation_z) / 2
  )

cat("\n=== NEAR-MISS SCORE (составной) ===\n")
nm %>%
  group_by(treatment) %>%
  summarise(
    n         = n(),
    mean_nm_score = mean(nm_score, na.rm = TRUE),
    sd_nm_score   = sd(nm_score, na.rm = TRUE),
    .groups = "drop"
  ) %>% print()

t_nm <- t.test(nm_score ~ treatment, data = nm)
cat(sprintf("\nt-тест nm_score: t=%.3f, df=%.1f, p=%.3f, d=%.2f\n",
            t_nm$statistic, t_nm$parameter, t_nm$p.value,
            abs(diff(t_nm$estimate)) /
              sqrt(mean(tapply(nm$nm_score, nm$treatment, var, na.rm=TRUE)))))

# 4. МОДЕЛИ: NEAR MISS PERCEPTION (Q1) = DESIGN + COVARIATES

cat("\n=== МОДЕЛИ: Q1 (near-miss perception) ===\n")

nm_polr <- nm %>%
  filter(!is.na(conf_c), !is.na(freq_c), !is.na(inter_c), !is.na(self_c))

nm_polr <- nm_polr %>%
  mutate(q1_ord2 = factor(q1_raw, levels = 1:3, ordered = TRUE))

m_q1_1 <- polr(q1_ord2 ~ treatment,
               data = nm_polr, Hess = TRUE)
m_q1_2 <- polr(q1_ord2 ~ treatment + freq_c + inter_c + self_c + conf_c,
               data = nm_polr, Hess = TRUE)

get_polr_coef <- function(m) {
  co  <- coef(summary(m))
  p   <- pnorm(abs(co[, "t value"]), lower.tail = FALSE) * 2
  cbind(co, p_value = p)
}

cat("\n--- Q1 ~ treatment (M1) ---\n")
print(get_polr_coef(m_q1_1))

cat("\n--- Q1 ~ treatment + ковариаты (M2) ---\n")
print(get_polr_coef(m_q1_2))

cat("\n--- OR для treatment (exp(β)) ---\n")
print(exp(coef(m_q1_2)["treatmenttreatment"]))

# Тест значимости treatment
cat("\nLRT treatment M1 vs intercept-only:\n")
m_q1_0 <- polr(q1_ord2 ~ 1, data = nm_polr, Hess = TRUE)
print(anova(m_q1_0, m_q1_1))

# 5. МОДЕЛИ:  (Q2=1) = NM_CLOSE + DESIGN  

cat("\n=== МОДЕЛИ: ЭСКАЛАЦИЯ (Q2=1) ===\n")

nm_esc <- nm %>%
  filter(!is.na(nm_close), !is.na(escalation),
         !is.na(freq_c), !is.na(inter_c))

m_esc_1 <- glm(escalation ~ nm_close,
               data = nm_esc, family = binomial)

m_esc_2 <- glm(escalation ~ nm_close + treatment,
               data = nm_esc, family = binomial)

m_esc_3 <- glm(escalation ~ nm_close * treatment + freq_c + inter_c + self_c,
               data = nm_esc, family = binomial)

cat("\n--- OR эскалации: nm_close + treatment (M2) ---\n")
print(exp(cbind(OR = coef(m_esc_2), confint(m_esc_2))))

cat("\n--- Взаимодействие nm_close × treatment (M3) ---\n")
print(summary(m_esc_3)$coefficients)
print(exp(cbind(OR = coef(m_esc_3), confint(m_esc_3))))

cat("\nLRT: nm_close значим?\n")
print(anova(m_esc_1, update(m_esc_1, . ~ 1), test = "Chisq"))

# 6. МОДЕЛИ: ATTRIBUTION (Q3) = DESIGN + NM_CLOSE

cat("\n=== МОДЕЛИ: ATTRIBUTION (Q3) ===\n")

m_attr_1 <- glm(chance_attr ~ treatment,
                data = nm, family = binomial)
m_attr_2 <- glm(chance_attr ~ treatment + nm_close + freq_c + inter_c,
                data = nm %>% filter(!is.na(nm_close)), family = binomial)

cat("\n--- chance_attr = treatment (M1) ---\n")
print(exp(cbind(OR = coef(m_attr_1), confint(m_attr_1))))
cat("p =", coef(summary(m_attr_1))["treatmenttreatment", "Pr(>|z|)"], "\n")

cat("\n--- chance_attr = treatment + nm_close + ков. (M2) ---\n")
print(summary(m_attr_2)$coefficients)

cat("\n--- Q3 распределение по дизайну ---\n")
print(table(nm$treatment, nm$q3_fct))
print(chisq.test(nm$treatment, nm$q3_fct))

# 7. МЕДИАЦИЯ

cat("\n=== МЕДИАЦИЯ: treatment → nm_close → escalation ===\n")

# Путь a: treatment = nm_close
path_a <- lm(nm_close ~ treatment, data = nm)
a <- coef(path_a)["treatmenttreatment"]
se_a <- sqrt(diag(vcov(path_a)))["treatmenttreatment"]
cat(sprintf("Путь a (treatment → nm_close): β=%.3f, se=%.3f, p=%.3f\n",
            a, se_a, 2 * pt(abs(a / se_a), df = nrow(nm) - 2, lower.tail = FALSE)))

# Путь b: nm_close = escalation (контроль treatment)
path_b <- glm(escalation ~ nm_close + treatment,
              data = nm, family = binomial)
b <- coef(path_b)["nm_close"]
se_b <- sqrt(diag(vcov(path_b)))["nm_close"]
cat(sprintf("Путь b (nm_close → escalation): β=%.3f (logit), se=%.3f, p=%.3f\n",
            b, se_b, 2 * pnorm(abs(b / se_b), lower.tail = FALSE)))

# Путь c (прямой): treatment = escalation при контроле nm_close
c_prime <- coef(path_b)["treatmenttreatment"]
cat(sprintf("Путь c' (прямой treatment → esc): β=%.3f (logit)\n", c_prime))

# Sobel test (приблизительный, для линейного пути a)
ab     <- a * b
se_ab  <- sqrt(b^2 * se_a^2 + a^2 * se_b^2)
z_sob  <- ab / se_ab
p_sob  <- 2 * pnorm(abs(z_sob), lower.tail = FALSE)
cat(sprintf("Sobel test: ab=%.4f, z=%.3f, p=%.3f\n", ab, z_sob, p_sob))

# 8. КОРРЕЛЯЦИИ И РЕЗЮМЕ

cat("\n=== КОРРЕЛЯЦИИ ===\n")
cor_vars <- nm %>%
  select(nm_close, escalation, chance_attr, skill_attr,
         confidence, betting_freq, interess, impulsivity) %>%
  cor(use = "pairwise.complete.obs")
print(round(cor_vars, 2))

# Итоговая таблица описательных по Q1/Q2/Q3 × дизайн × пол
cat("\n=== ИТОГОВАЯ ТАБЛИЦА: Q1 × Q2 × design ===\n")
summary_table <- nm %>%
  group_by(treatment) %>%
  summarise(
    n = n(),
    # Q1
    q1_очень_близко  = mean(q1_raw == 1, na.rm = TRUE),
    q1_довольно      = mean(q1_raw == 2, na.rm = TRUE),
    q1_не_близко     = mean(q1_raw == 3, na.rm = TRUE),
    mean_nm_close    = mean(nm_close, na.rm = TRUE),
    # Q2
    q2_больше        = mean(q2_raw == 1, na.rm = TRUE),
    q2_столько_же    = mean(q2_raw == 2, na.rm = TRUE),
    q2_меньше        = mean(q2_raw == 3, na.rm = TRUE),
    # Q3
    q3_случай        = mean(q3_clean == 1, na.rm = TRUE),
    q3_форма         = mean(q3_clean == 2, na.rm = TRUE),
    q3_составы       = mean(q3_clean == 3, na.rm = TRUE),
    q3_другое        = mean(q3_clean == 4, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  mutate(across(starts_with("q"), ~ round(. * 100, 1)))
print(summary_table)


# 9. ВИЗУАЛИЗАЦИЯ

col_ctrl <- "#4A90D9"
col_trt  <- "#E05A4E"

theme_dip <- theme_minimal(base_size = 13) +
  theme(plot.title     = element_text(face = "bold", size = 14),
        plot.subtitle  = element_text(color = "grey50", size = 11),
        legend.position = "bottom",
        panel.grid.minor = element_blank())

# Г1: Распределение Q1 по дизайну
g_q1 <- nm %>%
  count(treatment, q1_ord) %>%
  group_by(treatment) %>%
  mutate(pct = n / sum(n)) %>%
  ggplot(aes(x = q1_ord, y = pct, fill = treatment)) +
  geom_col(position = "dodge", alpha = .85, width = .7) +
  scale_fill_manual(values = c(col_ctrl, col_trt),
                    labels = c("Контроль", "Treatment")) +
  scale_y_continuous(labels = scales::percent) +
  labs(title    = "Q1: Насколько близким к успеху считаешь матч?",
       subtitle = "Near-miss perception по условиям дизайна",
       x = NULL, y = "Доля участников", fill = "Дизайн") +
  theme_dip
print(g_q1)

# Г2: Q2 по Q1
g_q2_q1 <- nm %>%
  count(q1_ord, q2_ord) %>%
  group_by(q1_ord) %>%
  mutate(pct = n / sum(n)) %>%
  ggplot(aes(x = q1_ord, y = pct, fill = q2_ord)) +
  geom_col(alpha = .85, width = .7) +
  scale_fill_manual(values = c("#E05A4E", "#F5A623", "#4A90D9"),
                    labels = c("Поставлю больше", "Столько же", "Меньше/нет")) +
  scale_y_continuous(labels = scales::percent) +
  labs(title    = "Q2: намерение ставить (по уровню Q1)",
       subtitle = "Выше near-miss feeling → больше эскалации?",
       x = "Q1: насколько близко к успеху", y = "Доля", fill = "Q2") +
  theme_dip
print(g_q2_q1)

# Г3: Q3 по дизайну
g_q3 <- nm %>%
  count(treatment, q3_fct) %>%
  group_by(treatment) %>%
  mutate(pct = n / sum(n)) %>%
  ggplot(aes(x = treatment, y = pct, fill = q3_fct)) +
  geom_col(alpha = .85, width = .5) +
  scale_fill_brewer(palette = "Set2") +
  scale_x_discrete(labels = c("Контроль", "Treatment")) +
  scale_y_continuous(labels = scales::percent) +
  labs(title    = "Q3: Что повлияло на проигрыш?",
       subtitle = "Attribution по условиям дизайна",
       x = NULL, y = "Доля", fill = "Q3") +
  theme_dip
print(g_q3)

# Г4: nm_score по дизайну
g_nm_score <- nm %>%
  ggplot(aes(x = treatment, y = nm_score, fill = treatment)) +
  geom_violin(alpha = .35, color = NA, trim = FALSE) +
  geom_boxplot(width = .15, outlier.shape = NA, alpha = .8) +
  geom_jitter(width = .08, alpha = .35, size = 1.5, aes(color = treatment)) +
  geom_hline(yintercept = 0, linetype = "dashed", color = "grey50") +
  scale_fill_manual(values  = c(col_ctrl, col_trt),
                    labels  = c("Контроль", "Treatment")) +
  scale_color_manual(values = c(col_ctrl, col_trt), guide = "none") +
  annotate("text", x = 1.5,
           y = max(nm$nm_score, na.rm = TRUE) * .9,
           label = sprintf("p = %.3f", t_nm$p.value),
           size = 4, color = "grey40") +
  labs(title    = "Near-miss score (составной индекс)",
       subtitle = "Среднее: (nm_close_z + escalation_z) / 2; выше = сильнее near-miss",
       x = NULL, y = "Near-miss score", fill = "Дизайн") +
  scale_x_discrete(labels = c("Контроль", "Treatment")) +
  theme_dip
print(g_nm_score)

# Г5:  диаграмма
cat("\n=== СХЕМА МЕДИАЦИИ ===\n")
cat(sprintf("
  treatment ──a(%.3f)──► nm_close ──b(%.3f logit)──► escalation
      └────────────c'(%.3f logit)────────────────────────┘
  Indirect (Sobel): ab=%.4f, p=%.3f
", a, b, c_prime, ab, p_sob))

