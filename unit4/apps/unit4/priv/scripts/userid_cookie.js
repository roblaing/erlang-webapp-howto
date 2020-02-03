function hexString(buffer) {
  const byteArray = new Uint8Array(buffer);
  const hexCodes = [...byteArray].map(value => {
    const hexCode = value.toString(16);
    const paddedHexCode = hexCode.padStart(2, "0");
    return paddedHexCode;
  });
  return hexCodes.join('');
}

function digestMessage(message) {
  const encoder = new TextEncoder();
  const data = encoder.encode(message);
  return window.crypto.subtle.digest("SHA-256", data);
}

function writeCookie() {
  const text = document.forms.login.elements.username.value +
               document.forms.login.elements.salt.value +
               document.forms.login.elements.password.value;
  digestMessage(text).then(digestValue => 
    {document.cookie = "user_id=" + hexString(digestValue);}
  );
}

