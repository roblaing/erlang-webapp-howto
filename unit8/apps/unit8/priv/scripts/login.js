"use strict";
const loginButton = document.querySelector("#loginButton");

loginButton.addEventListener("click", (event) => {
  let websocket = new WebSocket("ws://localhost:3030/blog");

  async function async_getId() {
    let id = await getId();
    let msg = { page: "login"
              , user_id: id
              , uuid: window.localStorage.getItem("uuid")
              };
    websocket.send(JSON.stringify(msg));
  }

  websocket.addEventListener("open", (event) => async_getId());

  websocket.addEventListener("message", (event) => {
    let msg = JSON.parse(event.data);
    if (msg.uuid === false) {
      document.querySelector('#login_error').textContent = "Your user name or password is incorrect";
    } else {
      window.localStorage.setItem("username", document.querySelector("#username").value);
      window.localStorage.setItem("uuid", msg.uuid);
      websocket.close();
      document.location.replace("/");
    }
  });

  websocket.addEventListener("error", (event) => {
    window.alert('WebSocket error: ', event);
  });
  
});

