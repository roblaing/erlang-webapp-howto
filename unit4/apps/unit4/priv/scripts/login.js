function validateLoginForm() {
  writeCookie();
  document.forms.login.elements.password.value = '';
  document.forms.login.elements.salt.value = '';
  return true;  
}

