import { Elm } from "./Main.elm";

document.addEventListener("DOMContentLoaded", async () => {
  Elm.Main.init({
    node: document.getElementById("main"),
    flag: {},
  });
});
