function validateForm() {
  let valid = true;
  if (document.getElementById('name').value.length === 0) {
    document.getElementById('name_error').textContent = "Browser says you need to enter a name";
    valid = false;  
  } else {
    document.getElementById('name_error').textContent = "";
  }
  if (document.getElementById('mail').value.length === 0) {
    document.getElementById('mail_error').textContent = "Browser says you need to enter an email address";
    valid = false;  
  } else {
    document.getElementById('mail_error').textContent = "";
  }
  if (document.getElementById('msg').value.length === 0) {
    document.getElementById('msg_error').textContent = "Browser says you need to type a message";
    valid = false;  
  } else {
    document.getElementById('msg_error').textContent = "";
  }
  return valid;
}
