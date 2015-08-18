/* global beforeEach, describe, it */

import {expect} from 'chai';
import sinon from 'sinon';
import {TalksProvider} from '../../../app/scripts/models/talks';

let Http, Talks;

let newest = [
  {
    id: 1,
    name: ''
  }, {
    id: 2,
    name: ''
  }, {
    id: 3,
    name: ''
  }
];

let params = {
  id: 1993,
  description: `Web cartoonist Randall Munroe answers simple what-if questions ("what if you hit a baseball moving at the speed of light?") using math, physics, logic and deadpan humor. In this charming talk, a reader’s question about Google's data warehouse leads Munroe down a circuitous path to a hilariously over-detailed answer — in which, shhh, you might actually learn something.`,
  images: {
    medium: 'http://tedcdnpe-a.akamaihd.net/images/ted/b867da1b6773c92bb7342367fa411d21a0a44018_615x461.jpg',
    small: 'http://tedcdnpe-a.akamaihd.net/images/ted/b867da1b6773c92bb7342367fa411d21a0a44018_240x180.jpg'
  },
  mSlug: 'RandallMunroe_2014',
  name: 'Randall Munroe: Comics that ask "what if?"',
  publishedAt: '2014-05-08T14:57:50.000Z',
  slug: 'randall_munroe_comics_that_ask_what_if',
  languages: [
    {
      code: 'en',
      name: 'English',
    }, {
      code: 'zh-cn',
      name: 'Chinese Simplified'
    }
  ]
};

beforeEach(() => {
  Http = {
    get: (url) => {
      let re = /\/api\/(talks|search)\?/;
      if (re.exec(url)) {
        return Promise.resolve(newest);
      } else {
        return Promise.resolve(newest[0]);
      }
    }
  };
  Talks = new TalksProvider(Http);
});

describe('Talks model', () => {
  it('should correctly parse speaker and title', () => {
    let talk = Talks.add(params);
    expect(talk.speaker).to.equal('Randall Munroe');
    expect(talk.title).to.equal('Comics that ask "what if?"');
  });

  it('should call Http.get if newest is empty', (done) => {
    let spy = sinon.spy(Http, 'get');
    expect(Talks.newest.length).to.equal(0);
    Talks.fetch().then(() => {
      expect(spy.calledOnce).to.be.true;
      expect(Talks.newest.length).to.equal(3);
      done();
    })
    .catch(done);
  });

  it('should not call Http.get if newest has data', (done) => {
    let spy = sinon.spy(Http, 'get');
    Talks.newest = newest;
    expect(Talks.newest.length).to.equal(3);
    Talks.fetch().then(() => {
      expect(spy.called).to.be.false;
      done();
      expect(Talks.newest.length).to.equal(3);
    })
    .catch(done);
  });

  it('should call Http.get if slugToTalk is empty', (done) => {
    let spy = sinon.spy(Http, 'get');
    Talks.fetchBySlug(1).then(() => {
      expect(spy.calledOnce).to.be.true;
      done();
    })
    .catch(done);
  });

  it('should not call Http.get if slugToTalk has data', (done) => {
    Talks.slugToTalk[1] = {};
    let spy = sinon.spy(Http, 'get');
    Talks.fetchBySlug(1).then(() => {
      expect(spy.called).to.be.false;
      done();
    })
    .catch(done);
  });

  it('search keyword should be used as query string', (done) => {
    let spy = sinon.spy(Http, 'get');
    Talks.search('design').then(() => {
      expect(spy.calledWith('/api/search?q=design')).to.be.true;
      done();
    })
    .catch(done);
  });

  it('random should request random API', (done) => {
    let spy = sinon.spy(Http, 'get');
    Talks.random().then(() => {
      expect(spy.calledWith('/api/talks/random')).to.be.true;
      done();
    })
    .catch(done);
  });
});
