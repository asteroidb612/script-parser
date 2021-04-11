import "./style.css";
// @ts-ignore
const { Elm } = require("./src/Main.elm");
const pagesInit = require("elm-pages");

pagesInit({
  mainElmModule: Elm.Main
}).then(function(app) {
  //  ____            _
  // |  _ \ ___  _ __| |_ ___
  // | |_) / _ \| '__| __/ __|
  // |  __/ (_) | |  | |_\__ \
  // |_|   \___/|_|   \__|___/
  //

  // Save script pieces on change
  app.ports.storeScriptPiecesValue.subscribe(function(scriptPieces) {
    localStorage.setItem(
      "cuecannon-script-pieces",
      JSON.stringify(scriptPieces)
    );
  });
});
