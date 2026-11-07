// BUILD_ID is set by `task dist` (esbuild --define) to a hash of the build,
// so a changed release invalidates the old cache.
const CACHE = "timer-" + BUILD_ID;

// Only used in the `task dist` build, where all scripts are bundled into app.js.
const ASSETS = [
  "./",
  "index.html",
  "app.js",
  "style.css",
  "manifest.webmanifest",
  "icons/icon.svg",
  "icons/icon-180.png",
  "icons/icon-192.png",
  "icons/icon-512.png",
  "sounds/starting.mp3",
  "sounds/interval-1.mp3",
  "sounds/interval-2.mp3",
  "sounds/interval-3.mp3",
  "sounds/final.mp3",
  "fonts/archivo.woff2",
];

self.addEventListener("install", (event) => {
  // cache: "reload" bypasses the HTTP cache, so a fresh install never stores stale files
  event.waitUntil(
    caches
      .open(CACHE)
      .then((cache) =>
        cache.addAll(
          ASSETS.map((url) => new Request(url, { cache: "reload" })),
        ),
      ),
  );
  self.skipWaiting();
});

self.addEventListener("activate", (event) => {
  event.waitUntil(
    caches
      .keys()
      .then((keys) =>
        Promise.all(
          keys.filter((k) => k !== CACHE).map((k) => caches.delete(k)),
        ),
      ),
  );
  self.clients.claim();
});

// Cache-first: the app works fully offline once installed.
self.addEventListener("fetch", (event) => {
  if (event.request.method !== "GET") return;
  event.respondWith(
    caches.match(event.request).then((hit) => hit || fetch(event.request)),
  );
});
