function validateSignupForm() {
  var valid = true;
  if (document.forms.login.elements.verify.value !== document.forms.login.elements.password.value) {
    valid = false;
    document.getElementById('error_verify').textContent = 'Password does not match';
  } else {
    document.getElementById('error_verify').textContent = '';
  }
  if (valid) {
    writeCookie();
    document.forms.login.elements.password.value = '';
    document.forms.login.elements.verify.value = '';
    document.forms.login.elements.salt.value = '';
  }
  return valid;
}

