(function() {
  document.getElementById("ajaxButton").onclick = function() {
    if (document.getElementById("password").value === document.getElementById("verify").value) {
      makeRequest();
    } else {
      document.getElementById('error_verify').textContent = "Password does not match";
      document.getElementById('error_username').textContent = "";
    }
  };

  async function makeRequest() 
    { fetch("/signup"
           , { method: "POST"
             , headers: {"Content-Type": "application/json"}
             , body: JSON.stringify(
               { "user_id": await writeCookie()
               , "username": document.getElementById("username").value
               , "email": document.getElementById("email").value
               })
             }
      ).then((response) => { return response.json(); } 
      ).then((myJson) => 
        { if (myJson.logged_in === false) {
            document.getElementById('error_username').textContent = "Sorry, that name is already taken. Please pick another.";
            document.getElementById('error_verify').textContent = "";
          } else {
            document.location.replace('/welcome/' + encodeURIComponent(document.getElementById("username").value));
          }
        }
      ).catch(error => alert('Caught Exception: ' + error.description)); 
    }

})();

