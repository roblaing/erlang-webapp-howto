"use strict";
window.addEventListener("DOMContentLoaded", (event) => {
  fetch("/logout",
    { method: "POST"
    , headers: {"Content-Type": "application/json"}
    , body: JSON.stringify({uuid: window.localStorage.getItem("uuid")})
    })
  .then((response) => response.json())
  .then((msg) => {
    if (msg.result === true) {
      window.localStorage.clear();
      document.location.replace("/login");
    }
  })
  .catch(error => alert('Caught Exception: ' + error.description));
});
