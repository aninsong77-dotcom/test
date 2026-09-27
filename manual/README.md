# 곤글박이 사용 설명서 영상 제작 도구

`aninsong77-dotcom/gonglbaki` 웹앱을 브라우저에서 자동 조작·녹화하고, 자막·내레이션·배경음악(모차르트 K.545)을 입혀 1080p MP4를 만든다.

## 구성
- `video/record.js` — 시나리오(챕터 00~10). 앱을 조작하며 CDP 스크린캐스트로 30fps 녹화, 내레이션 시각을 `<출력>.narr.json`에 기록
- `video/director.js` — 페이지에 주입되는 연출 레이어(자막 바, 챕터 카드, 카메라 줌, 커서·마우스 아이콘)
- `video/common.js` — 로컬 정적 서버, Tailwind CDN 대체, 폰트(Pretendard)
- `tts/gen.py` — 문장 → wav. `GOOGLE_TTS_API_KEY`가 있으면 구글 Cloud TTS(기본 `ko-KR-Neural2-A`, `GOOGLE_TTS_VOICE`로 변경), 없으면 오프라인 KSS 음성
- `tts/mix.py` — 녹화 로그대로 내레이션 트랙 합성
- `tts/k545.mid` — 모차르트 피아노 소나타 K.545 1악장 제시부(music21 코퍼스에서 추출)

## 실행 순서 (클라우드 세션 기준)
```bash
apt-get install -y ffmpeg fluidsynth fluid-soundfont-gm
pip install sherpa-onnx soundfile numpy scipy
git clone --depth 1 https://github.com/aninsong77-dotcom/gonglbaki /home/user/aninsong77-dotcom/gonglbaki
cd manual/video && npm ci && npx tailwindcss -c tailwind.config.js -i tw.in.css -o tw.css --minify
# (구글 키가 없을 때만) 오프라인 음성 모델
# curl -L https://github.com/k2-fsa/sherpa-onnx/releases/download/tts-models/vits-mimic3-ko_KO-kss_low.tar.bz2 | tar xj -C ../tts

node record.js pass1.mjpeg    # 1차: 음성 클립 생성(캐시) — 타이밍이 늘어지므로 버림
node record.js pass2.mjpeg    # 2차: 캐시된 음성 길이로 정확한 타이밍 녹화
python3 ../tts/mix.py pass2.mjpeg.narr.json <seconds>
fluidsynth -ni -g 0.8 -r 44100 -F k545.wav /usr/share/sounds/sf2/FluidR3_GM.sf2 ../tts/k545.mid
ffmpeg -stream_loop 30 -i k545.wav -t <seconds> -af "aecho=0.8:0.5:60:0.18,loudnorm=I=-30,afade=t=in:d=1.5" -ar 44100 -ac 2 music_bg.wav
ffmpeg -i narration.wav -af "aresample=44100,loudnorm=I=-16:TP=-1.5" -ac 2 narr44.wav
ffmpeg -i music_bg.wav -i narr44.wav -filter_complex "[1]asplit=2[sc][v];[0][sc]sidechaincompress=threshold=0.02:ratio=6:attack=80:release=600[d];[d][v]amix=inputs=2:normalize=0,alimiter=limit=0.95[a]" -map "[a]" mix.wav
ffmpeg -f mjpeg -framerate 30 -i pass2.mjpeg -i mix.wav -vf "crop=1920:1080:0:0" -c:v libx264 -preset slow -crf 22 -pix_fmt yuv420p -c:a aac -b:a 128k -shortest -movflags +faststart 곤글박이_사용설명서.mp4
```

## 참고
- 앱의 인물 도형은 클릭만으로는 선택이 유지되지 않아(드래그/Shift+클릭 필요) 시나리오는 드래그 선택을 쓴다.
- 텍스트 상자는 만든 뒤 더블클릭해야 입력된다.
- 오프라인 KSS 음성을 쓰는 경우 마지막 화면에 출처(CC BY-NC-SA 4.0) 표기가 필요하다. 구글 음성을 쓰면 `record.js`의 해당 문구를 지워도 된다.
