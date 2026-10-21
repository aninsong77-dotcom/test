# 통 사용 설명 영상

| 영상 | 파일 | 길이 | 만드는 스크립트 |
|---|---|---|---|
| 통 사용법 (12단계 전체) | `tong_tutorial.mp4` · 자막 `tong_tutorial.srt` · 대본 `tong_tutorial_script.md` | 약 9분 | `tutorial.mjs` |
| AI 연결하기 (네 가지 방법) | `tong_ai_connect.mp4` · 자막 `tong_ai_connect.srt` · 대본 `tong_ai_connect_script.md` | 약 7분 | `ai_connect.mjs` |

영상은 실제 화면을 장면마다 찍고 자막·강조를 얹어 만든다. 앱이 바뀌면 **스크립트를 고친 뒤 다시 돌리면** 영상도 바뀐다.

## 다시 만들기
1. 이 저장소 맨 위에서 화면 서버 켜기: `python3 -m http.server 8765` (`stats-ladder.html`을 띄움)
2. 이 폴더에서 Playwright 준비: `npm i playwright` (크로미움 경로는 스크립트의 `executablePath`를 환경에 맞게)
3. 장면 찍기: `node tutorial.mjs` → `tutorial_scenes.json`, `node ai_connect.mjs` → `ai_connect_scenes.json` (그림은 `frames/`)
4. 영상·자막·대본 만들기 (ffmpeg 필요): `python3 build.py tutorial`, `python3 build.py ai_connect`
5. 만든 mp4를 배포 저장소(`tong`)의 `video/`에도 복사한다. 사용 설명의 [영상으로 보기]가 그 주소를 가리킨다.

## 고칠 때
- 장면은 `shot('자막', ['강조할 요소'])`, 설명 카드는 `cardShot(html, 초, '자막')`, 장 제목은 `chapCard()`.
- 화면의 버튼·칸 이름(`data-…` 속성)이 바뀌면 스크립트의 선택자도 같이 고친다.
- 자막의 앱 이름은 '통'으로 쓴다 (로고 글씨는 화면에서만).
- `assets/scale_scan.png`: AI 영상에서 척도 사진 읽기 장면에 쓰는 예시 그림.
