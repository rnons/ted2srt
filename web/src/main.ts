/// <reference path="./main.d.ts" />

import Http from './models/http';
import Home from './home-page';
import './css/global.css';

const root = document.getElementById('root');

Http.get('/api/talks').then(talks => {
  const home = new Home(talks);
  root.innerHTML = home.render();
});
