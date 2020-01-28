function hexString(buffer) {
  const byteArray = new Uint8Array(buffer);
  const hexCodes = [...byteArray].map(value => {
    const hexCode = value.toString(16);
    const paddedHexCode = hexCode.padStart(2, '0');
    return paddedHexCode;
  });
  return hexCodes.join('');
}

function digestMessage(message) {
  const encoder = new TextEncoder();
  const data = encoder.encode(message);
  return window.crypto.subtle.digest('SHA-256', data);
}

function validateSignupForm() {
  var valid = true;
  if (document.forms.signup.elements.username.value.length === 0) {
    valid = false;
    document.getElementById('error_username').textContent = 'No Username';
  } else {
    fetch('/check_name', {
      method: 'POST',
      headers: {
        'Content-Type': 'application/json'
      },
      body: JSON.stringify({"userName": document.forms.signup.elements.username.value})
    })
    .then(res => res.json())
    .then(response =>
      {if (response.nameOk === document.forms.signup.elements.username.value) { 
        document.getElementById('error_username').textContent = ''    
      } else {
        valid = false;
        document.getElementById('error_username').textContent = response.nameOk;
      }})
    .catch(error => document.getElementById('error_email').textContent = 'Caught Exception: ' + error.description);
  }
  if (document.forms.signup.elements.password.value.length < 4) {
    valid = false;
    document.getElementById('error_password').textContent = 'Password too short';
  } else {
    document.getElementById('error_password').textContent = '';
  }
  if (document.forms.signup.elements.verify.value !== document.forms.signup.elements.password.value) {
    valid = false;
    document.getElementById('error_verify').textContent = 'Password does not match';
  } else {
    document.getElementById('error_verify').textContent = '';
  }
  if (valid) {
    const text = document.forms.signup.elements.username.value +
                 document.forms.signup.elements.salt.value +
                 document.forms.signup.elements.password.value;
    digestMessage(text).then(digestValue => {
      document.cookie = 'user_id=' + hexString(digestValue);
    });
    document.forms.signup.elements.password.value = '';
    document.forms.signup.elements.verify.value = '';
    document.forms.signup.elements.salt.value = '';
  }
  return valid;
}

function validateLoginForm() {
  var valid = true;
  if (document.forms.login.elements.username.value.length === 0) {
    valid = false;
    document.getElementById('login_error').textContent = 'No Username';
  }
  if (document.forms.login.elements.password.value.length === 0) {
    valid = false;
    document.getElementById('login_error').textContent = 'No Password';
  }
  if (valid) {
    const text = document.forms.login.elements.username.value +
                 document.forms.login.elements.salt.value +
                 document.forms.login.elements.password.value;
    digestMessage(text).then(digestValue => {
      document.cookie = 'user_id=' + hexString(digestValue);
    });
    document.forms.login.elements.password.value = '';
    document.forms.login.elements.salt.value = '';
  }
  return valid;  
}
