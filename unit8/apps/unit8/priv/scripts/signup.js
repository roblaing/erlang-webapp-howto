"use strict";
const websocket = new WebSocket("ws://localhost:3030/blog");
const loginButton = document.querySelector("#signupButton");

async function checkId() {
  if (document.getElementById("password").value === document.getElementById("verify").value) {
    let id = await getId();
    let msg = {
      page: "signup",
      user_id: id,
      username: document.querySelector("#username").value,
      email: document.querySelector("#email").value,
      uuid: window.localStorage.getItem("uuid")
    };
    websocket.send(JSON.stringify(msg));    
  } else {
    document.getElementById('error_verify').textContent = "Password does not match";
    document.getElementById('error_username').textContent = "";
  }
}

signupButton.addEventListener("click", (event) => checkId());

window.addEventListener("unload", (event) => {
  websocket.close();
});

websocket.addEventListener("message", (event) => {
  var msg = JSON.parse(event.data);
  if (msg.uuid === false) {
    document.querySelector("#error_username").textContent = "Sorry, that name is already taken. Please pick another.";
  } else {
    window.localStorage.setItem("username", document.querySelector("#username").value);
    window.localStorage.setItem("uuid", msg.uuid);
    document.location.replace("/");
  }
});

websocket.addEventListener("error", (event) => {
  window.alert('WebSocket error: ', event);
});


