import Elm from './Main.elm'
import './css/global.css'

var KEY = 'languages'

var app = Elm.Main.fullscreen()

app.ports.getLangs.subscribe(function() {
  var langs = localStorage.getItem(KEY)
  if (langs) {
    setTimeout(function() {
      app.ports.onReceiveLangs.send(langs)
    })
  }
})

app.ports.setLangs.subscribe(function(langs) {
  localStorage.setItem(KEY, langs)
})


app.ports.setTitle.subscribe(function(title) {
  document.title = title
})
