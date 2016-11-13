/// <reference path="./main.d.ts" />

import Http from './services/http';
import HomePage from './home-page';
import TalkPage from './talk-page';
import './css/global.css';

const http = new Http();
const root = document.getElementById('root');


const TALK_PAGE_REGEXP = /#\/talks\/(\w+)/;


const routeHandler = () => {
  const hash = document.location.hash;
  let matches;

  if (hash === '#/') {
    new HomePage(http, root);
  } else if (matches = TALK_PAGE_REGEXP.exec(hash)) {
    const slug = matches[1];
    new TalkPage(http, root, slug);
  } else {
    document.location.hash = '#/';
  }
};

window.addEventListener('load', routeHandler);

window.addEventListener('hashchange', routeHandler);
