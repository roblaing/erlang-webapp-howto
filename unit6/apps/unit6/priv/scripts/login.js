(function() {
  document.getElementById("ajaxButton").onclick = function() {
    makeRequest();
  };

  async function makeRequest() 
    { fetch("/login"
           , { method: "POST"
             , headers: {"Content-Type": "application/json"}
             , body: JSON.stringify({"user_id": await writeCookie()})
             }
      ).then((response) => { return response.json(); } 
      ).then((myJson) => 
        { if (myJson.logged_in === false) {
            document.getElementById('login_error').textContent = "Your user name or password is incorrect";
          } else {
            document.location.replace('/welcome/' + encodeURIComponent(document.getElementById("username").value));
          }
        }
      ).catch(error => alert('Caught Exception: ' + error.description)); 
    }

})();
