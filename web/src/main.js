import Elm from './Main.elm';
import './css/global.css';

var KEY = 'languages'

var app = Elm.Main.fullscreen();

app.ports.getLangs.subscribe(function() {
  var langs = localStorage.getItem(KEY)
  if (langs) {
    app.ports.onReceiveLangs.send(langs);
  }
})

app.ports.setLangs.subscribe(function(langs) {
  localStorage.setItem(KEY, langs)
})
