"use strict";
const websocket = new WebSocket("ws://localhost:3030/blog");
const loginButton = document.querySelector("#loginButton");

async function checkId() {
  var id = await getId();
  var msg = { page: "login"
            , user_id: id
            , uuid: window.localStorage.getItem("uuid")
            };
  websocket.send(JSON.stringify(msg));  
}

loginButton.addEventListener("click", (event) => checkId());

window.addEventListener("unload", (event) => {
  websocket.close();
});

websocket.addEventListener("message", (event) => {
  var msg = JSON.parse(event.data);
  if (msg.uuid === false) {
    document.querySelector('#login_error').textContent = "Your user name or password is incorrect";
  } else {
    window.localStorage.setItem("username", document.querySelector("#username").value);
    window.localStorage.setItem("uuid", msg.uuid);
    document.location.replace("/");
  }
});

websocket.addEventListener("error", (event) => {
  window.alert('WebSocket error: ', event);
});


