# COMPARE MODELS ON TEST SET - wrapped in a function
# https://topepo.github.io/caret/subsampling-for-class-imbalances.html

outside_models <- list(original = orig_fit,
                       down = down_outside,
                       up = up_outside,
                       SMOTE = smote_outside,
                       ROSE = rose_outside)

outside_resampling <- resamples(outside_models)

test_roc <- function(model, data) {
  library(pROC)
  roc_obj <- roc(data$Class, 
                 predict(model, data, type = "prob")[, "Class1"],
                 levels = c("Class2", "Class1"))
  ci(roc_obj)
}

outside_test <- lapply(outside_models, test_roc, data = imbal_test)
outside_test <- lapply(outside_test, as.vector)
outside_test <- do.call("rbind", outside_test)
colnames(outside_test) <- c("lower", "ROC", "upper")
outside_test <- as.data.frame(outside_test)

# compare on validation
outside_resampling

# compare on test
summary(outside_resampling, metric = "ROC")