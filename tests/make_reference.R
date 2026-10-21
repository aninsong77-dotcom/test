# 통 기준값 만들기 — 기본 R만 씀 (추가 패키지 없음)
#   Rscript tests/make_reference.R      → tests/reference_values.json
# 같은 자료를 통의 계산 함수에 넣어 이 값과 맞는지 tests/reference.mjs가 확인합니다.
set.seed(20261021)
J <- function(x) {                      # 아주 작은 JSON 작성기
  if (is.null(x)) return("null")
  if (is.list(x)) {
    if (!is.null(names(x))) return(paste0("{", paste0('"', names(x), '":', vapply(x, J, ""), collapse = ","), "}"))
    return(paste0("[", paste(vapply(x, J, ""), collapse = ","), "]"))
  }
  if (is.matrix(x)) return(J(lapply(seq_len(nrow(x)), function(i) x[i, ])))
  if (is.character(x)) return(if (length(x) == 1) paste0('"', x, '"') else paste0("[", paste0('"', x, '"', collapse = ","), "]"))
  if (is.logical(x) && length(x) == 1) return(if (is.na(x)) "null" else tolower(as.character(x)))
  v <- ifelse(is.na(x), "null", sprintf("%.15g", x))
  if (length(x) == 1) v else paste0("[", paste(v, collapse = ","), "]")
}
out <- list()

# ---- t검정: 챗GPT 리뷰의 예시 + 분산이 다른 모의 자료 ----
tt2 <- function(a, b) { s <- t.test(a, b, var.equal = TRUE); w <- t.test(a, b)
  list(a = a, b = b, tS = unname(s$statistic), dfS = unname(s$parameter), pS = s$p.value, tW = unname(w$statistic), dfW = unname(w$parameter), pW = w$p.value) }
out$ttest <- list(tt2(c(2, 4, 6, 8, 10), 1:5), tt2(round(rnorm(37, 3.2, 0.6), 2), round(rnorm(52, 2.9, 1.1), 2)))

# ---- 회귀: 보통 자료, 단위가 아주 큰 자료(조건수가 나쁨), 완전 공선성 ----
n <- 150; x1 <- rnorm(n); x2 <- 0.6 * x1 + rnorm(n, sd = 0.8); x3 <- rnorm(n); y <- 1 + 0.4 * x1 - 0.25 * x2 + 0.1 * x3 + rnorm(n)
regref <- function(y, X) { m <- lm(y ~ X); s <- summary(m)
  list(X = X, y = y, b = unname(coef(m)), se = unname(s$coefficients[, 2]), p = unname(s$coefficients[, 4]), R2 = s$r.squared, pF = unname(pf(s$fstatistic[1], s$fstatistic[2], s$fstatistic[3], lower.tail = FALSE))) }
out$ols <- list(regref(y, cbind(x1, x2, x3)), regref(y * 1e3, cbind(x1 * 1e6 + 5e7, x2, x3 * 1e-3)))
X4 <- cbind(x1, x2, x1 + 2 * x2)   # 세 번째 열 = 앞 두 열의 합 → lm은 NA
out$ols_singular <- list(X = X4, y = y, lm_na = which(is.na(coef(lm(y ~ X4)))) - 1)

# ---- 상관과 Shapiro–Wilk ----
out$corr <- list(a = x1, b = x2, r = cor(x1, x2), p = cor.test(x1, x2)$p.value)
sw <- round(rexp(60), 3); out$shapiro <- list(x = sw, W = unname(shapiro.test(sw)$statistic), p = shapiro.test(sw)$p.value)

# ---- 요인분석: 3요인 9문항 5점 척도 ----
N <- 250; F <- matrix(rnorm(N * 3), N); Lt <- matrix(0, 9, 3); Lt[1:3, 1] <- c(.8, .7, .6); Lt[4:6, 2] <- c(.75, .65, .55); Lt[7:9, 3] <- c(.7, .6, .7); Lt[3, 2] <- .3
Xi <- F %*% t(Lt) + matrix(rnorm(N * 9, sd = .6), N); Xi <- apply(Xi, 2, function(v) pmin(5, pmax(1, round(3 + v))))
R <- cor(Xi)
tidy <- function(L) { L <- unclass(L); ss <- colSums(L^2); L <- L[, order(-ss), drop = FALSE]; for (j in seq_len(ncol(L))) if (L[which.max(abs(L[, j])), j] < 0) L[, j] <- -L[, j]; L }
e <- eigen(R, symmetric = TRUE)
pca <- e$vectors[, 1:3] %*% diag(sqrt(e$values[1:3]))
# 주축요인추출: psych::fa(fm = "pa")와 같은 반복 (처음 공통성 = SMC, 공통성 합의 변화 < .001, 최대 50번)
paf <- function(R, nf, min.err = .001, max.iter = 50) {
  r <- R; diag(r) <- 1 - 1 / diag(solve(R)); comm <- sum(diag(r)); err <- comm; i <- 1
  while (err > min.err) { ev <- eigen(r, symmetric = TRUE); L <- ev$vectors[, 1:nf] %*% diag(sqrt(pmax(ev$values[1:nf], 0)))
    new <- diag(L %*% t(L)); comm1 <- sum(new); diag(r) <- new; err <- abs(comm - comm1); comm <- comm1; i <- i + 1; if (i > max.iter) break }
  list(L = L, iter = i - 1) }
pf3 <- paf(R, 3)
out$efa <- list(X = Xi, nf = 3, pca = tidy(varimax(pca, eps = 1e-5)$loadings), paf = tidy(varimax(pf3$L, eps = 1e-5)$loadings), paf_iter = pf3$iter,
  ml_factanal = tidy(factanal(covmat = R, factors = 3, rotation = "varimax")$loadings))

# ---- 베이즈 인자: R BayesFactor 패키지와 같은 식을 R의 integrate로 계산 ----
bf_t <- function(t, N, nu, r = sqrt(2) / 2) {
  f <- function(g) exp(-.5 * log1p(N * g * r^2) - (nu + 1) / 2 * log1p(t^2 / ((1 + N * g * r^2) * nu)) + (nu + 1) / 2 * log1p(t^2 / nu)) * (2 * pi)^-.5 * g^-1.5 * exp(-1 / (2 * g))
  log(integrate(f, 0, Inf, rel.tol = 1e-12)$value) }
bf_r <- function(r, n) {     # Ly et al. (2016), κ = 1
  a <- (n - 1) / 2; c <- (n + 2) / 2; z <- r^2
  lf <- function(t) (a - 1) * log(t) + .5 * log1p(-t) - a * log1p(-z * t)
  m <- optimize(lf, c(0, 1), maximum = TRUE)$objective
  l2f1 <- lgamma(c) - lgamma(a) - lgamma(c - a) + m + log(integrate(function(t) exp(lf(t) - m), 0, 1, rel.tol = 1e-12)$value)
  log(sqrt(pi) / 2) + lgamma((n + 1) / 2) - lgamma((n + 2) / 2) + l2f1 }
bf_reg <- function(R2, N, p, r = sqrt(2) / 4) {
  sc <- r^2 * N / 2; lg <- function(g) .5 * ((N - p - 1) * log1p(g) - (N - 1) * log1p(g * (1 - R2))) + .5 * log(sc) - lgamma(.5) - 1.5 * log(g) - sc / g
  m <- -optimize(function(u) -lg(exp(u)), c(-30, 60))$objective
  m + log(integrate(function(g) exp(lg(g) - m), 0, Inf, rel.tol = 1e-12)$value) }
sl <- c(0.7, -1.6, -0.2, -1.2, -0.1, 3.4, 3.7, 0.8, 0.0, 2.0) - c(1.9, 0.8, 1.1, 0.1, -0.1, 4.4, 5.5, 1.6, 4.6, 3.4)   # R의 sleep 자료 (대응 차이)
tsl <- mean(sl) / (sd(sl) / sqrt(10))
out$bayes <- list(
  t = lapply(list(c(1.8974, 2.5, 8), c(tsl, 10, 9), c(2.1, 50, 49), c(5.5, 75, 298), c(0.3, 60, 238)), function(x) list(t = x[1], N = x[2], nu = x[3], lbf = bf_t(x[1], x[2], x[3]))),
  sleep_BayesFactor_published = 17.25888,   # ttestBF(sleep$extra[1:10], sleep$extra[11:20], paired = TRUE)
  r = lapply(list(c(.3, 50), c(.1, 300), c(.5, 300), c(-.62, 20), c(.05, 100), c(.2, 5)), function(x) list(r = x[1], n = x[2], lbf = bf_r(x[1], x[2]))),
  reg = lapply(list(c(.1, 100, 3), c(.02, 300, 2), c(.35, 300, 5), c(.6, 40, 1)), function(x) list(R2 = x[1], N = x[2], p = x[3], lbf = bf_reg(x[1], x[2], x[3]))))
out$made <- paste("R", getRversion(), format(Sys.Date()))
dir <- dirname(sub("--file=", "", grep("--file=", commandArgs(FALSE), value = TRUE)))
writeLines(J(out), file.path(dir, "reference_values.json"))
cat("reference_values.json 저장\n")
