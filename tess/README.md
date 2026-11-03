# tess/ — 브라우저 안 글자 인식기

[Tesseract.js](https://github.com/naptha/tesseract.js) 5.1.1과 tesseract.js-core 5.1.1(Apache-2.0, 라이선스 파일 함께 둠),
영어 데이터 `@tesseract.js-data/eng` 1.0.0의 `4.0.0_best_int/eng.traineddata.gz`.

- `eng.traineddata.gz.b64.txt`: 위 압축 파일을 base64 글자로 바꾼 것. claude.ai 아티팩트가 `.txt`는 받아 주지만 `.traineddata`는 받지 않아서.
  짹짹이가 받아서 바이트로 되돌린 뒤 `createWorker([{code: "eng", data}])`로 넘긴다.
- `worker.min.js`는 한 곳을 고쳤다: 언어를 바이트로 넘길 때 초기화에 쓰는 이름을 `l.data` 대신 `l.code`에서 읽게
  (원본 `src/worker-script/index.js`의 `_langs.map((l) => ((typeof l === 'string') ? l : l.data))` 버그).
