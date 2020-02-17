"use strict";
const websocket = new WebSocket("ws://localhost:3030/blog");

websocket.addEventListener("open", (event) => {
  var msg = {
    page: "logout",
    uuid: window.localStorage.getItem("uuid")
  };
  websocket.send(JSON.stringify(msg));
  window.localStorage.clear();
  document.location.replace("/");
});

window.addEventListener("unload", (event) => {
  websocket.close();
});

