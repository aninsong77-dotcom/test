# 웹알 시험용 모형: 브라우저(webR)와 진짜 R에서 똑같이 돌려 숫자를 비교
suppressPackageStartupMessages(library(lavaan))
d <- lavaan::HolzingerSwineford1939
cfa_model <- ' visual  =~ x1 + x2 + x3
               textual =~ x4 + x5 + x6
               speed   =~ x7 + x8 + x9 '
sem_model <- paste(cfa_model, ' speed ~ a * visual + b * textual
                                 ind := a * b ')
fm <- c("chisq", "df", "pvalue", "cfi", "tli", "rmsea", "rmsea.ci.lower", "rmsea.ci.upper", "srmr")
f1 <- cfa(cfa_model, data = d)
f2 <- sem(sem_model, data = d)
s1 <- standardizedSolution(f1); s2 <- parameterEstimates(f2)
out <- list(
  lavaan = as.character(packageVersion("lavaan")), R = paste(R.version$major, R.version$minor, sep = "."),
  cfa_fit = as.list(unname(fitMeasures(f1, fm))), cfa_fit_names = fm,
  cfa_std = s1$est.std[s1$op == "=~"], cfa_std_names = paste(s1$lhs, s1$rhs)[s1$op == "=~"],
  sem_est = s2$est[s2$op %in% c("~", ":=")], sem_se = s2$se[s2$op %in% c("~", ":=")], sem_names = paste(s2$lhs, s2$op, s2$rhs)[s2$op %in% c("~", ":=")]
)
as.character(jsonlite::toJSON(out, digits = NA, auto_unbox = TRUE))
