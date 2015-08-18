import Http from './http';

class Talk {
  constructor(params) {
    this.id = params.id;
    this.name = params.name;
    [this.speaker, this.title] = this.name.split(':');
    this.description = params.description;
    this.slug = params.slug;
    this.mSlug = params.mSlug;
    this.publishedAt = params.publishedAt;
    this.images = params.images;
    this.languages = params.languages;
  }
}

export class TalksProvider {
  constructor() {
    this.talks = {};
    this.newest = [];
    this.slugToTalk = {};
  }

  add(params) {
    let id, talk;
    id = params.id;
    if (this.talks[id] && this.talks[id].languages) {
      talk = this.talks[id];
    } else {
      talk = new Talk(params);
      this.talks[id] = talk;
    }
    return talk;
  }

  fetch() {
    if (this.newest.length) {
      return Promise.resolve(this.newest);
    } else {
      return Http.get('/api/talks?limit=5').then((data) => {
        this.newest = data.map(this.add, this);
        return Promise.resolve(this.newest);
      }).catch(err => {
        console.log(err);
      });
    }
  }

  fetchBySlug(slug) {
    let talk = this.slugToTalk[slug];
    if (talk) {
      return Promise.resolve(talk);
    } else {
      return Http.get(`/api/talks/${slug}`).then((data) => {
        talk = this.add(data);
        this.slugToTalk[talk.slug] = talk;
        return Promise.resolve(talk);
      }).catch(err => {
        console.log(err);
      });
    }
  }

  search(query) {
    return Http.get(`/api/search?q=${query}`).then((data) => {
      let talks = data.map(this.add, this);
      return Promise.resolve(talks);
    }).catch(err => {
      console.log(err);
    });
  }

  random() {
    return Http.get('/api/talks/random').then((data) => {
      let talk = this.add(data);
      return Promise.resolve(talk);
    }).catch(err => {
      console.log(err);
    });
  }
}
