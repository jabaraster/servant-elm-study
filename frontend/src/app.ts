import { getUser, singoutUrl } from "./auth.ts";
import { Elm } from "./Main.elm";

(async () => {
  const user = await getUser();
  Elm.Main.init({
    node: document.getElementById("elm"),
    flags: {
      singoutUrl,
    },
  });
})();
