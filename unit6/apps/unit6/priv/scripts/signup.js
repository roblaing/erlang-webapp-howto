"use strict";

document.querySelector("#signupButton").addEventListener("click", (event) => {
  if (document.querySelector("#password").value === document.querySelector("#verify").value) {
    getId().then((id) => {
      const outmsg = { user_id: id
                     , uuid: window.localStorage.getItem("uuid")
                     , username: document.querySelector("#username").value
                     , email: document.querySelector("#email").value
                     };
      fetch("/signup",
        { method: "POST"
        , headers: {"Content-Type": "application/json"}
        , body: JSON.stringify(outmsg)
        })
      .then((response) => response.json())
      .then((inmsg) => {
        if (inmsg.uuid === false) {
          document.querySelector('#error_username').textContent = "Sorry, that name is already taken";
        } else {
          window.localStorage.setItem("username", document.querySelector("#username").value);
          window.localStorage.setItem("uuid", inmsg.uuid);
          document.location.replace("/");
        }
      })
      .catch(error => alert('Caught Exception: ' + error.description));
    });
  } else {
    document.getElementById('error_verify').textContent = "Password does not match";
    document.getElementById('error_username').textContent = "";
  }
});

