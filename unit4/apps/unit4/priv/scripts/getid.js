async function hexString(buffer) {
  let byteArray = new Uint8Array(buffer);
  let hexCodes = [...byteArray].map(value => {
    let hexCode = value.toString(16);
    let paddedHexCode = hexCode.padStart(2, "0");
    return paddedHexCode;
  });
  return hexCodes.join("");
}

async function digestMessage(message) {
  let encoder = new TextEncoder();
  let data = encoder.encode(message);
  return window.crypto.subtle.digest("SHA-256", data);
}

async function write_cookie() {
  let text = document.getElementById("username").value +
               document.getElementById("salt").value +
               document.getElementById("password").value;
  digestMessage(text)
    .then((digestValue) => hexString(digestValue))
    .then((hex) => {document.cookie = "user_id=" + hex});
}

