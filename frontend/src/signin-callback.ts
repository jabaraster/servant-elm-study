import { signinRedirectCallback, signoutRedirect } from "./auth.ts";

window.addEventListener("DOMContentLoaded", async () => {
  try {
    const user = await signinRedirectCallback(); // これでWebStorageにトークン情報を保存しているみたい.
    const res = await fetch("/api/authentication/status", {
      headers: {
        Authorization: `Bearer ${user.access_token}`,
      },
    });
    const authed = (await res.json()) as boolean;
    if (authed) {
      window.location.href = "/";
    } else {
      alert("認証に失敗しました。");
      await signoutRedirect();
    }
  } catch (e) {
    alert("認証に失敗しました。");
    await signoutRedirect();
  }
});
