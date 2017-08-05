require('./main.css');

var Elm = require('./Main.elm');

var root = document.getElementById('root');

var app = Elm.Main.embed(root);

app.ports.playSound.subscribe(function (soundName) {
	if (soundName) new Audio("assets/" + soundName).play();
});
