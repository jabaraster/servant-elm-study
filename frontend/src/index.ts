import { signinRedirect, signoutRedirect, getUser } from "./auth.ts";
import { User } from "oidc-client-ts";

window.addEventListener("DOMContentLoaded", async () => {
  const mUser = await getUser();
  if (mUser && !mUser.expired) {
    loggedIn(mUser);
  } else {
    notLoggedIn();
  }
});

function notLoggedIn() {
  var signIn = document.getElementById("signIn") as HTMLButtonElement;
  signIn.classList.remove("is-hidden");
  signIn.addEventListener("click", async () => {
    await signinRedirect();
  });
}

function loggedIn(user: User) {
  const signOut = document.getElementById("signOut")! as HTMLButtonElement;
  signOut.classList.remove("is-hidden");
  signOut.addEventListener("click", async (e) => {
    await signoutRedirect(user.id_token!);
  });
  addScriptTag(new URL("./app.ts", import.meta.url));
}

function addScriptTag(url: URL) {
  const tag = document.createElement("script") as HTMLScriptElement;
  tag.src = url.href;
  if (location.hostname !== "localhost") {
    tag.type = "module";
  }
  document.getElementsByTagName("body")[0].appendChild(tag);
}
