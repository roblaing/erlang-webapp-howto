"use strict";
const signupButton = document.querySelector("#signupButton");

signupButton.addEventListener("click", (event) => {
  let websocket = new WebSocket("ws://localhost:3030/blog");

  async function async_getId() {
    if (document.querySelector("#password").value === document.querySelector("#verify").value) {
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
      document.querySelector("#error_verify").textContent = "Password does not match";
      document.querySelector("#error_username").textContent = "";
    }
  }
 
  websocket.addEventListener("open", (event) => async_getId());

  websocket.addEventListener("message", (event) => {
    let msg = JSON.parse(event.data);
    if (msg.uuid === false) {
      document.querySelector("#error_username").textContent = "Sorry, that name is already taken. Please pick another.";
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

