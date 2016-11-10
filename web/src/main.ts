/// <reference path="./main.d.ts" />

import Store from './services/store';
import Http from './services/http';
import HomePage from './home-page';
import TalkPage from './talk-page';
import './css/global.css';

const root = document.getElementById('root');
const store = new Store(new Http());


const TALK_PAGE_REGEXP = /#\/talks\/(\w+)/;

const routeHandler = () => {
  const hash = document.location.hash;
  let matches;
  if (hash === '') {
    store.getNewest().then(talks => {
      const home = new HomePage(talks);
      root.innerHTML = home.render();
    });
  } else if (matches = TALK_PAGE_REGEXP.exec(hash)) {
    const slug = matches[1];
    store.getBySlug(slug).then(talk => {
      const rerender = () => {
        root.innerHTML = talkPage.render();
        talkPage.mounted();
      };
      const talkPage = new TalkPage(talk, rerender);
      rerender();
    });
  } else {
    document.location.hash = '';
  }
};

window.addEventListener('load', routeHandler);

window.addEventListener('hashchange', routeHandler);
