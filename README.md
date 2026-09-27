# 짹짹이

<img src="logo.webp" width="96" alt="짹짹이 병아리 로고">

논문 파일을 열면 머리말·꼬리말, 쪽 번호, 각주, 그림·표 설명, 참고문헌은 빼고 본문만 브라우저 음성으로 읽어 줍니다.

- 지원 형식: PDF, 한글(HWP, HWPX), 엑셀(XLSX, XLS, CSV), TXT, 복사한 글 붙여넣기
- 본문 속 출처 표시([12], (Kim, 2021))를 빼고 읽거나 다 읽기 선택
- 시작 위치 선택(장·소제목 목록), 문장을 눌러 그 위치부터 듣기, 문장 하이라이트, 속도 조절
- 영어 논문 한 문장 + 한국어 번역 번갈아 읽기 (claude.ai 아티팩트로 열었을 때만)
- 스캔한 이미지 PDF 알아보기, Claude로 글자 인식해서 읽기 (claude.ai 아티팩트로 열었을 때만)

`index.html` 하나로 동작합니다. 브라우저로 열거나 GitHub Pages로 배포하면 PC와 휴대폰에서 모두 쓸 수 있습니다.

## 두 가지 버전

| | claude.ai 버전 | 웹 버전 (이 저장소, GitHub Pages) |
|---|---|---|
| 본문만 읽기, 출처 빼기, 시작 위치 선택 | 됨 | 됨 |
| 영어 한 문장 + 한국어 번역 | 됨 (Claude 로그인 필요) | 안 됨 — 버튼에 "번역 안 됨"으로 표시 |
| 스캔본 PDF 글자 인식 | 됨 (Claude 로그인 필요) | 안 됨 — claude.ai 버전으로 가는 링크 표시 |
| 홈 화면 아이콘 | claude.ai 아이콘 | 병아리 아이콘 |

## GitHub Pages로 올리기

1. 저장소 **Settings → General → Danger Zone → Change visibility**에서 공개로 바꿉니다. (비공개 저장소의 Pages는 GitHub 유료 요금제가 필요합니다.)
2. **Settings → Pages → Build and deployment**에서 Source를 **Deploy from a branch**, Branch를 `main` / `/ (root)`로 고르고 저장합니다.
3. 1~2분 뒤 `https://aninsong77-dotcom.github.io/test/`에서 열립니다.

