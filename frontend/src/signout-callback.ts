import { removeUser } from "./auth";

window.addEventListener("DOMContentLoaded", async () => {
  removeUser();
  setTimeout(() => {
    window.location.href = "/";
  }, 3000);
});
