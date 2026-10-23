# 나중에 할 일

## 삼원분산분석 기능 추가 (2026-10-23 보류)

- 계기: 수업 자료(신약 "에이 프로틴"이 미토콘드리아에 미치는 영향, 성별 × 집단 × 기간 2×2×2 개체 간 설계)를 통으로 분석하려 했는데, 통에는 기준 두 개까지인 이원분산분석만 있음
- 예상 작업량: 30분~1시간, 구조방정식의 1/4~1/5 정도

### 할 것
1. 계산: `stats-ladder.html`의 `async factorial(r)`과 같은 방식(효과 코딩 + 회귀, 제III유형 제곱합)으로 세 기준까지 확장
   - 주효과 3, 2원 상호작용 3, 3원 상호작용 1, 부분 η²
   - 칸별 평균 (SD, n) 표, 빈 칸 오류 안내
2. 분석 화면: 기법 목록(`factorial` slots)에 "집단 기준 3"(선택) 추가, 또는 `factorial3` 따로
3. 그림: 세 번째 기준의 수준마다 A × B 평균 선 그림
4. 논문 문장, R 코드(`car::Anova(type = 3)`, `contr.sum`), SPSS 명령문(UNIANOVA 3요인)
5. 표본 수 계산(3원 상호작용), 질문 사다리 연결(`r_factorial_anova` 경로에서 기준이 셋일 때)
6. 테스트: `tests/reference.mjs`에 R과 숫자 비교 추가, `tests/edge.mjs`에 빈 칸 경우
7. 배포: tong 저장소 `index.html`, `review/tong_calc_core.js`, 테스트 파일 복사

### 지금 쓸 수 있는 SPSS 명령문
```
UNIANOVA mitochondria BY gender group duration
  /METHOD=SSTYPE(3)
  /PRINT=ETASQ HOMOGENEITY DESCRIPTIVE
  /DESIGN=gender group duration gender*group gender*duration group*duration gender*group*duration.
```
