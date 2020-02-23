"use strict";

document.querySelector("#loginButton").addEventListener("click", (event) => {
  getId().then((id) => {
    const outmsg = { user_id: id
                   , uuid: window.localStorage.getItem("uuid")
                   };
    fetch("/login",
      { method: "POST"
      , headers: {"Content-Type": "application/json"}
      , body: JSON.stringify(outmsg)
      })
    .then((response) => response.json())
    .then((inmsg) => {
      if (inmsg.uuid === false) {
        document.querySelector('#login_error').textContent = "Your user name or password is incorrect";
      } else {
        window.localStorage.setItem("username", document.querySelector("#username").value);
        window.localStorage.setItem("uuid", inmsg.uuid);
        document.location.replace("/");
      }
    })
    .catch(error => alert('Caught Exception: ' + error.description));
  });
});

