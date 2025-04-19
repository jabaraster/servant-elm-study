import { signinRedirectCallback } from "./auth.ts";

window.addEventListener("DOMContentLoaded", async () => {
  await signinRedirectCallback();
  window.location.href = "/";
});
