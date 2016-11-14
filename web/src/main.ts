/// <reference path="./main.d.ts" />

import Http from './services/http';
import { parseQueryString } from './services/utils';
import HomePage from './home-page';
import TalkPage from './talk-page';
import SearchPage from './search-page';
import './css/global.css';

const http = new Http();
const root = document.getElementById('root');


const TED_URL_REGEXP = /^https?:\/\/www.ted.com\/talks\/(\w+)/;
const TALK_PAGE_REGEXP = /#\/talks\/(\w+)/;


const routeHandler = () => {
  const hash = document.location.hash;
  let matches;

  if (hash === '#/') {
    new HomePage(http, root);
  } else if (matches = TALK_PAGE_REGEXP.exec(hash)) {
    const slug = matches[1];
    new TalkPage(http, root, slug);
  } else if (hash === '#/search') {
    const query: { q?: string } = parseQueryString();
    matches = TED_URL_REGEXP.exec(decodeURIComponent(query.q));
    if (matches) {
      document.location.href = '/#/talks/' + matches[1];
    }
    new SearchPage(http, root, query);
  } else {
    document.location.hash = '#/';
  }
};

window.addEventListener('load', routeHandler);

window.addEventListener('hashchange', routeHandler);
