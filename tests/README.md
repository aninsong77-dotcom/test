# 기준값 테스트

통의 계산이 R과 같은지 확인합니다. 코드를 고칠 때마다 돌립니다.

```
cd tests
npm install            # 처음 한 번 (Playwright)
node reference.mjs     # 1부 + 2부
node reference.mjs --no-r   # R 없이 1부만
```

- **1부** (R 없이 됨): `reference_values.json`에 R로 미리 계산해 둔 값과 통의 계산 함수를 비교합니다.
  - t검정 (Student·Welch, 챗GPT 리뷰의 예시 포함)
  - 회귀: 보통 자료, 단위가 아주 다른 자료, 완전 공선성
  - 피어슨 r, Shapiro–Wilk
  - 요인분석: 주성분, 주축요인추출 (psych `fa(fm = "pa")`와 같은 반복)
  - 베이즈 인자: t, 상관, 회귀. BayesFactor 패키지의 공개값(sleep 자료 17.25888) 포함
- **2부** (Rscript 필요): 예시 자료로 화면에서 분석 19가지를 돌립니다. 그다음 통이 만든 `analysis.R`을 R로 실행해 효과 54개의 추정값, p, 신뢰구간을 비교합니다. 부트스트랩 신뢰구간은 R과 난수가 달라 0.02까지 허용합니다.

기준값을 새로 만들려면 `Rscript make_reference.R`을 실행합니다(기본 R만 사용).
