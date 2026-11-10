// Entry point bundled by esbuild into app.js. "elm" is aliased (--alias) to the
// compiled Elm output: build/elm-debug.js in dev, build/elm-optimized.js in dist.
import { Elm } from "elm";
import { Session } from "./session.js";
import "./haptic.js";

// DEV is set by esbuild (--define); in the dist build this block is dropped entirely.
if (DEV) new EventSource("/esbuild").addEventListener("change", () => location.reload());

var stored = null;
try {stored = localStorage.getItem("settings");} catch (e) { }

var app = Elm.Main.init({node: document.getElementById("app"), flags: stored});

var wakeLock = null;
var wakeLockPending = false;
var wantAwake = false;

function acquireWakeLock() {
  if (wakeLock || wakeLockPending) return;
  wakeLockPending = true;

  navigator.wakeLock.request("screen")
    .then(function (lock) {
      wakeLockPending = false;
      if (!wantAwake) {lock.release(); return;}
      wakeLock = lock;
      lock.addEventListener("release", function () {
        wakeLock = null;
        if (wantAwake && document.visibilityState === "visible") acquireWakeLock();
      });
    })
    .catch(function (err) {
      wakeLockPending = false;
      console.warn("Wake lock failed:", err);
    });
}

document.addEventListener("visibilitychange", function () {
  if (document.visibilityState === "visible" && wantAwake) acquireWakeLock();
});

// While the screen is locked the animation timeline is frozen, but the session goes on (bells play).
// On unlock everything should look as if it had played in real time, so every CSS animation and
// transition (rings, warmup → session colour, fading buttons) is jumped to its end, whether it was
// mid-way, still in its delay, or only starting now. Elm redraws in requestAnimationFrame, which doesn't
// run while locked, so state changed meanwhile reaches the DOM on the first frame after unlock; Elm's
// redraw was queued earlier and runs before ours in that frame. The second frame is a fallback.
function finishAnimations() {
  document.getAnimations().forEach(function (a) {
    var timing = a.effect && a.effect.getComputedTiming();
    if (timing && timing.endTime !== Infinity) a.finish();
  });
}

document.addEventListener("visibilitychange", function () {
  if (document.visibilityState !== "visible") return;
  requestAnimationFrame(function () {
    finishAnimations();
    requestAnimationFrame(finishAnimations);
  });
});

app.ports.keepAwake.subscribe(function (awake) {
  Session.setActive(awake.session);

  wantAwake = awake.screen;
  if (!("wakeLock" in navigator)) return;

  if (wantAwake && !wakeLock) {
    acquireWakeLock();
  } else if (!wantAwake && wakeLock) {
    wakeLock.release();
    wakeLock = null;
  }
});


if (app.ports && app.ports.playBell) {
  app.ports.playBell.subscribe(name => Session.play(name));
}

if (app.ports && app.ports.stopBells) {
  app.ports.stopBells.subscribe(() => Session.stop());
}

// - Settings
if (app.ports && app.ports.saveSettings) {
  app.ports.saveSettings.subscribe(function (settings) {
    try {localStorage.setItem("settings", JSON.stringify(settings));}
    catch (e) {console.warn("Failed to save settings: ", e);}
  });
}

// Service worker enables offline use + installability. Skipped on
// localhost so it doesn't cache stale builds during elm-live sessions.
var isLocal = ["localhost", "127.0.0.1"].indexOf(location.hostname) !== -1;
if ("serviceWorker" in navigator && !isLocal) {
  navigator.serviceWorker.register("sw.js");
}
