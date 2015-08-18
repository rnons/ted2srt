/* global beforeEach, describe, it */

import {expect} from 'chai';
import {TalksProvider} from '../../../app/scripts/models/talks';

let Talks;

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
  Talks = new TalksProvider();
});

describe('Talks model', () => {
  it('should correctly parse speaker and title', () => {
    let talk = Talks.add(params);
    expect(talk.speaker).to.equal('Randall Munroe');
    expect(talk.title).to.equal('Comics that ask "what if?"');
  });
});
