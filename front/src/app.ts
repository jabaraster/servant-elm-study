import { Elm } from "./Main.elm";

interface User {
  id: number;
  firstName: string;
  lastName: string;
  registrationDate: string;
}

function tag<E extends HTMLElement>(id: string): E {
  const ret = document.getElementById(id);
  if (!ret) throw new Error("Element not found: " + id);
  return ret as E;
}

document.addEventListener("DOMContentLoaded", async () => {
  Elm.Main.init({
    node: document.getElementById("main"),
    flag: {},
  });
  const res = await fetch("/api/users");
  const users = (await res.json()) as User[];
  tag<HTMLTextAreaElement>("console").innerText = JSON.stringify(
    users,
    null,
    2,
  );
});
