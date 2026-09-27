"""내레이션 한 문장 → wav (+ .dur 파일에 길이 기록)
GOOGLE_TTS_API_KEY 환경 변수가 있으면 구글 Cloud TTS, 없으면 오프라인 sherpa-onnx(KSS) 사용.
키는 요청 헤더로만 보내며 출력·저장하지 않는다."""
import sys, os, json, base64, urllib.request, io
import soundfile as sf
text, out = sys.argv[1], sys.argv[2]
key = os.environ.get("GOOGLE_TTS_API_KEY")
if key:
    voice = os.environ.get("GOOGLE_TTS_VOICE", "ko-KR-Neural2-A")
    body = {"input": {"text": text}, "voice": {"languageCode": "ko-KR", "name": voice},
            "audioConfig": {"audioEncoding": "LINEAR16", "sampleRateHertz": 24000, "speakingRate": float(os.environ.get("GOOGLE_TTS_RATE", "1.0"))}}
    req = urllib.request.Request("https://texttospeech.googleapis.com/v1/text:synthesize", data=json.dumps(body).encode(),
                                 headers={"Content-Type": "application/json", "X-Goog-Api-Key": key})
    try:
        res = json.load(urllib.request.urlopen(req, timeout=60))
    except urllib.error.HTTPError as e:
        sys.exit(f"Google TTS error {e.code}: {e.read().decode()[:300]}")
    data, sr = sf.read(io.BytesIO(base64.b64decode(res["audioContent"])))
else:
    import sherpa_onnx
    M = os.path.join(os.path.dirname(os.path.abspath(__file__)), "vits-mimic3-ko_KO-kss_low") + "/"
    tts = sherpa_onnx.OfflineTts(sherpa_onnx.OfflineTtsConfig(model=sherpa_onnx.OfflineTtsModelConfig(
        vits=sherpa_onnx.OfflineTtsVitsModelConfig(model=M + "ko_KO-kss_low.onnx", tokens=M + "tokens.txt", data_dir=M + "espeak-ng-data", length_scale=0.95), num_threads=4)))
    a = tts.generate(text, sid=0, speed=1.0); data, sr = a.samples, a.sample_rate
sf.write(out, data, sr)
open(out + ".dur", "w").write(str(len(data) / sr))
