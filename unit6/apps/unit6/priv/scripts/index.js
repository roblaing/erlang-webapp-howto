"use strict";
window.addEventListener("DOMContentLoaded", (event) => {
  fetch("/",
    { method: "POST"
    , headers: {"Content-Type": "application/json"}
    , body: JSON.stringify({uuid: window.localStorage.getItem("uuid")})
    })
  .then((response) => response.json())
  .then((msg) => {
    if (msg.username === false || msg.username !== window.localStorage.getItem("username")) {
      window.localStorage.clear();
      document.location.replace("/login");
    } else {
      document.location.replace("/welcome/" + msg.username);
    }
  })
  .catch(error => alert('Caught Exception: ' + error.description));
});
