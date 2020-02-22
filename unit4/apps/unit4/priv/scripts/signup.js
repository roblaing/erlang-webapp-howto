function validateSignupForm() {
  var valid = true;
  if (document.forms.signup.elements.verify.value !== document.forms.signup.elements.password.value) {
    valid = false;
    document.getElementById("error_verify").textContent = "Password does not match";
  } else {
    document.getElementById("error_verify").textContent = "";
  }
  if (valid) {
    write_cookie();
    document.forms.signup.elements.password.value = "";
    document.forms.signup.elements.verify.value = "";
    document.forms.signup.elements.salt.value = "";
  }
  return valid;
}

