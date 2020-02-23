async function hexString(buffer) {
  const byteArray = new Uint8Array(buffer);
  const hexCodes = [...byteArray].map(value => {
    const hexCode = value.toString(16);
    const paddedHexCode = hexCode.padStart(2, "0");
    return paddedHexCode;
  });
  return hexCodes.join("");
}

async function digestMessage(message) {
  const encoder = new TextEncoder();
  const data = encoder.encode(message);
  return window.crypto.subtle.digest("SHA-256", data);
}

async function getId() {
  const text = document.getElementById("username").value +
               document.getElementById("salt").value +
               document.getElementById("password").value;
  return digestMessage(text).then((digestValue) => hexString(digestValue));
}

