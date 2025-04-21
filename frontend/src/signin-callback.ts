import { signinRedirectCallback } from "./auth.ts";

window.addEventListener("DOMContentLoaded", async () => {
  await signinRedirectCallback(); // これでWebStorageにトークン情報を保存しているみたい.
  window.location.href = "/";
});
