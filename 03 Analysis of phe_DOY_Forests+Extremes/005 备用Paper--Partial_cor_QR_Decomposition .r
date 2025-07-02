# 组合最终数据框
new_df_val <- data.frame(extremes_value = extremes_val, phe_value = phe_val, control_vals_df)
new_df_val$SDII = 0
new_df_val <- na.omit(new_df_val)  # 去除 NA 值

# 检查样本量
if (nrow(new_df_val) < ncol(new_df_val) + 2) next

# 检查控制变量的方差
variances <- apply(new_df_val[, -c(1, 2)], 2, var, na.rm = TRUE)
zero_var_cols <- names(variances[variances == 0])

# 移除方差为 0 的控制变量
if (length(zero_var_cols) > 0) {
  cat("Removing control variables with zero variance:", zero_var_cols, "\n")
  new_df_val <- new_df_val[, !colnames(new_df_val) %in% zero_var_cols]
}

# 检查变量之间的完全共线性 找到相关性绝对值大于 0.99 的控制变量-避免完全共线性问题
# 通过相关性矩阵检测并移除高度相关的变量对，避免完全共线性问题
cor_matrix <- cor(new_df_val[, -c(1, 2)], use = "complete.obs")
high_cor_pairs <- which(abs(cor_matrix) > 0.99 & lower.tri(cor_matrix), arr.ind = TRUE)

if (nrow(high_cor_pairs) > 0) {
  vars_to_remove <- unique(colnames(cor_matrix)[high_cor_pairs[, 2]])
  cat("Removing variables with perfect collinearity:", vars_to_remove, "\n")
  new_df_val <- new_df_val[, !colnames(new_df_val) %in% vars_to_remove]
}

# 通过 QR 分解检测并移除导致完全共线性的变量，确保模型的系数可以唯一确定
model <- lm(extremes_value ~ ., data = new_df_val[, -2])
qr_model <- qr(model)

if (qr_model$rank < ncol(model$model)) {
  aliased_vars <- colnames(model$model)[qr_model$pivot[(qr_model$rank + 1):ncol(model$model)]]
  cat("Removing aliased variables:", aliased_vars, "\n")
  new_df_val <- new_df_val[, !colnames(new_df_val) %in% aliased_vars]
}

# 计算偏相关性
partial_cor_test <- pcor.test(new_df_val$extremes_value, new_df_val$phe_value, new_df_val[, -c(1, 2)])

# 添加结果行
result_row <- data.frame(
  Variable = var_type,  # 动态使用 var_type
  Climate_Class = Climate_Class, 
  Forest_Class  = forest, 
  Partial_Correlation = partial_cor_test$estimate, 
  p_value = partial_cor_test$p.value,
  zero_var_cols = zero_var_cols)
