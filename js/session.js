export const Session = (function () {

  var SOUNDS = ["starting", "interval-1", "interval-2", "interval-3", "final"];

  var ctx = null; // null when released
  var active = false; // a session is running
  var buffers = {};
  var playing = [];
  var pending = null;
  var keepAlive = null; // silent loop: iOS freezes the page w/o it after the last sound ends

  function isRunning(c) {
    return !!c && c.state === "running";
  }
  function isClosed(c) {
    return !c || c.state === "closed";
  }
  function isInterrupted(c) {
    return !!c && c.state === "interrupted";
  }
  function isSuspended(c) {
    return !!c && c.state === "suspended";
  }
  function isBroken(c) {
    return isClosed(c) || isInterrupted(c);
  }
  function isPaused(c) {
    return isSuspended(c) || isInterrupted(c);
  }

  function create() {
    if (ctx) ctx.close().catch(() => {});
    clearPlaying();
    keepAlive = null;

    var c = new AudioContext();
    c.onstatechange = () => onState(c);
    c.resume().catch(() => {});
    ctx = c;
    if (active) startKeepAlive();
  }

  function release() {
    if (active || playing.length > 0 || pending || !ctx) return;
    stopKeepAlive();
    ctx.close().catch(() => {});
    ctx = null;
  }

  function ready() {
    if (isBroken(ctx)) create();
    var c = ctx;
    var resumed = isRunning(c) ? Promise.resolve() : c.resume();
    return resumed.then(
      () => (c === ctx ? c : ready()),
      (err) => {
        if (c !== ctx) return ready();
        throw err;
      },
    );
  }

  function onState(c) {
    console.log("audio: ", c.state);
    if (c !== ctx) return;

    if (!active) {
      if (!isRunning(c)) release();
    } else if (isPaused(c)) {
      c.resume().catch(() => {});
    }
  }

  function onDeviceChange() {
    console.log("audio: device change");
    if (!ctx) return;
    if (active && playing.length === 0) create();
    else release();
  }

  function unlock() {
    if (!ctx) return;
    if (isBroken(ctx)) create();
    if (!isRunning(ctx)) ctx.resume().catch(() => {});
  }

  function startKeepAlive() {
    if (keepAlive || !ctx) return;
    var src = ctx.createBufferSource();
    src.buffer = ctx.createBuffer(1, ctx.sampleRate, ctx.sampleRate); // 1s of silence
    src.loop = true;
    src.connect(ctx.destination);
    src.start();
    keepAlive = src;
  }

  function stopKeepAlive() {
    if (!keepAlive) return;
    try {
      keepAlive.stop();
    } catch (e) {}
    keepAlive = null;
  }

  function finish(entry) {
    if (entry.done) return;
    entry.done = true;
    clearTimeout(entry.timer);
    playing = playing.filter((e) => e !== entry);
    release();
  }

  function clearPlaying() {
    playing.forEach((e) => {
      e.done = true;
      clearTimeout(e.timer);
    });
    playing = [];
  }

  function start(c, buf) {
    var gain = c.createGain();
    var src = c.createBufferSource();
    src.buffer = buf;
    src.connect(gain).connect(c.destination);

    var entry = { src: src, gain: gain, done: false };
    src.onended = () => finish(entry);
    // the context clock can stall (device switch, interruption), the wall clock can't
    entry.timer = setTimeout(() => finish(entry), buf.duration * 1000 + 1000);
    playing.push(entry);
    src.start();
  }

  function play(name) {
    var buf = buffers[name];
    if (!buf) {
      console.warn("Sound not loaded: ", name);
      return;
    }

    if (pending) pending.cancelled = true; // a newer bell makes the waiting one stale
    var request = { cancelled: false };
    pending = request;

    ready()
      .then((c) => {
        if (pending === request) pending = null;
        if (request.cancelled) {
          release();
          return;
        }
        start(c, buf);
      })
      .catch((err) => {
        if (pending === request) pending = null;
        console.warn("Bell failed: ", name, err);
        release();
      });
  }

  function stop() {
    if (pending) {
      pending.cancelled = true;
      pending = null;
    }
    if (!ctx) return;

    var t = ctx.currentTime;
    playing.forEach((e) => {
      e.gain.gain.setValueAtTime(e.gain.gain.value, t);
      e.gain.gain.linearRampToValueAtTime(0, t + 0.3);
      e.src.stop(t + 0.3);
      clearTimeout(e.timer);
      e.timer = setTimeout(() => finish(e), 1000); // in case onended stalls
    });
  }

  function setActive(on) {
    active = on;
    if (on)
      create(); // iOS can keep an old context bound to the previous output
    else setTimeout(release, 0); // let a final bell sent in the same update claim it first
  }

  var decoder = new OfflineAudioContext(1, 1, 48000); // only decodes audio
  SOUNDS.forEach((name) => {
    fetch("sounds/" + name + ".mp3")
      .then((r) => {
        if (!r.ok) throw new Error("HTTP " + r.status);
        return r.arrayBuffer();
      })
      .then((data) => decoder.decodeAudioData(data))
      .then((buf) => (buffers[name] = buf))
      .catch((err) => console.warn("Failed to load sound:", name, err));
  });

  ["click", "touchend", "keydown"].forEach((type) =>
    document.addEventListener(type, unlock),
  );
  if (navigator.mediaDevices)
    navigator.mediaDevices.addEventListener("devicechange", onDeviceChange);
  // iOS: play even when the ringer switch is on silent (Safari 17+)
  if (navigator.audioSession) navigator.audioSession.type = "playback";

  return { setActive: setActive, play: play, stop: stop };
})();
