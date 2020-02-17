"use strict";

function navbar(Uuid) {
  let template = document.querySelector("nav > template").content;
  if (Uuid !== false) {
    let val1 = template.querySelector("a[href='/login']");
    let val2 = template.querySelector("a[href='/signup']");
    val1.setAttribute("href", "/");;
    val1.textContent = window.localStorage.getItem("username");
    val2.setAttribute("href", "/logout");
    val2.textContent = "logout";     
  }
  let html = template.cloneNode(true);
  document.querySelector("nav").appendChild(html); 
}

function newpost(Uuid) {
  if (Uuid !== false) {
    let template = document.querySelector("section#newpost > template").content;
    let html = template.cloneNode(true);
    document.querySelector("section#newpost").appendChild(html);
  }
}

function posts(Posts) {
  Posts.forEach((post) => {
    let template = document.querySelector("section#posts > template").content;
    template.querySelector("a.post-title").textContent = post.title;
    template.querySelector("a.post-title").setAttribute("href", "/" + post.id);
    template.querySelector("span.post-date").textContent = post.created;
    template.querySelector("pre.post-content").textContent = post.art;
    let html = template.cloneNode(true);
    document.querySelector("section#posts").appendChild(html);
  });
}

window.addEventListener("DOMContentLoaded", (event) => {
  let websocket = new WebSocket("ws://localhost:3030/blog");
  
  websocket.addEventListener("open", (event) => {
    let msg = {
      page: "front"
    };
    let id = window.localStorage.getItem("uuid");
    if (id !== null) {
      msg.uuid = id;
    };
    websocket.send(JSON.stringify(msg));
  });
  
  websocket.addEventListener("message", (event) => {
    let id = window.localStorage.getItem("uuid");    
    let msg = JSON.parse(event.data);
    if (id !== null && id !== msg.uuid) {
      document.location.replace("/logout");
    } else {
      navbar(msg.uuid);
      newpost(msg.uuid);
      posts(msg.posts);
      websocket.close();
    }
  });
  
  websocket.addEventListener("error", (event) => {
    window.alert('WebSocket error: ', event);
  });

});

