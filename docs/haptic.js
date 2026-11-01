var Haptics = (function () {
  "use strict";

  // iOS has no vibrate(); toggling a native switch from inside a tap gives a haptic tick (iOS 18+)
  var label = document.createElement("label");
  var input = document.createElement("input");
  input.type = "checkbox";
  input.setAttribute("switch", "");
  label.appendChild(input);
  label.style.cssText =
    "position:fixed;opacity:0;pointer-events:none;width:0;height:0;overflow:hidden";
  document.body.appendChild(label);

  function tap() {
    if (navigator.vibrate) navigator.vibrate(10);
    else label.click();
  }

  document.addEventListener("click", function (e) {
    if (e.target.closest("[data-haptic]")) tap();
  });

  return { tap: tap };
})();
