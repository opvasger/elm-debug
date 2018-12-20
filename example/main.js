const { Elm } = require("./src/Main.elm");

const name = "example";

const main = Elm.Main.init({
  node: document.body,
  flags: {
    debug: localStorage.getItem(name)
  }
});

main.ports.debug.subscribe(function onUpdate(model) {
  localStorage.setItem(name, model);
});
