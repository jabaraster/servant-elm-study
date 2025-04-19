import { UserManager, Log, User } from "oidc-client-ts";

if (process.env.OIDC_DEBUG === "true") {
  Log.setLevel(Log.DEBUG);
  Log.setLogger(console);
}

// process.env["XXX"]という書き方ではビルド時に環境変数を展開してくれない
const authority = req(process.env.OIDC_AUTHORITY, "OIDC_AUTHORITY");
const clientId = req(process.env.OIDC_CLIENT_ID, "OIDC_CLIENT_ID");
const signinCallbackUrl = `${location.origin}/signin-callback`;
const signoutCallbackUrl = `${location.origin}/signout-callback`;
const responseType = "code";
const scope = "openid";

const userManager = new UserManager({
  authority: authority,
  client_id: clientId,
  redirect_uri: signinCallbackUrl,
  response_type: responseType,
  scope: scope,
});

export async function getUser(): Promise<User | null> {
  return await userManager.getUser();
}

export async function signinRedirect(): Promise<void> {
  await userManager.signinRedirect();
}

export async function signinRedirectCallback(): Promise<User> {
  return await userManager.signinRedirectCallback();
}

export const singoutUrl = (() => {
  const cognitoDomain = process.env.COGNITO_DOMAIN;
  return `${cognitoDomain}/logout?client_id=${clientId}&logout_uri=${encodeURIComponent(signoutCallbackUrl)}`;
})();

export async function signoutRedirect(): Promise<void> {
  window.location.href = singoutUrl;
}

export async function removeUser(): Promise<void> {
  await userManager.removeUser();
}

function req(value: string | undefined, name: string): string {
  if (!value) {
    throw new Error(`${name} is not set`);
  }
  return value;
}
