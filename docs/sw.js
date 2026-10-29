// Set by `task build` to a hash of the build, so a changed release invalidates the old cache.
const CACHE = "timer-0f1f6655";

const ASSETS = [
  "./",
  "index.html",
  "elm.js",
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
];

self.addEventListener("install", (event) => {
  // cache: "reload" bypasses the HTTP cache, so a fresh install never stores stale files
  event.waitUntil(
    caches.open(CACHE).then((cache) =>
      cache.addAll(ASSETS.map((url) => new Request(url, { cache: "reload" }))),
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
