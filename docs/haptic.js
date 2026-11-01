var Haptics = (function () {
  "use strict";

  // a tap on the cover's label is forwarded to its switch as a second click; the button needs only one
  document.addEventListener(
    "click",
    function (e) {
      if (e.target.matches("[data-haptic-switch]")) e.stopPropagation();
    },
    true,
  );

  // Android: no switch haptics there, but vibrate() works
  document.addEventListener("click", function (e) {
    if (navigator.vibrate && e.target.closest("[data-haptic]"))
      navigator.vibrate(10);
  });
})();
