function validateForm() {
  let valid = true;
  if (document.getElementById('title').value.length === 0) {
    document.getElementById('title_error').textContent = "Please enter a title";
    valid = false;  
  } else {
    document.getElementById('title_error').textContent = "";
  }
  if (document.getElementById('art').value.length === 0) {
    document.getElementById('art_error').textContent = "Pleaser enter some art";
    valid = false;  
  } else {
    document.getElementById('art_error').textContent = "";
  }
  return valid;
}
