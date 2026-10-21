# 진짜 R에서 model.R을 돌려 ref.json 저장 (GitHub 자동 테스트에서 실행)
args <- commandArgs(FALSE); here <- dirname(sub("--file=", "", grep("--file=", args, value = TRUE)))
res <- eval(parse(file.path(here, "model.R")))
writeLines(res, file.path(here, "ref.json")); cat(res, "\n")
