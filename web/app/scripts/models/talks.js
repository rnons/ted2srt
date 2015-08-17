import Http from './http';

class Talk {
  constructor(params) {
    this.id = params.id;
    this.name = params.name;
    [this.speaker, this.title] = this.name.split(':');
    this.description = params.description;
    this.slug = params.slug;
    this.publishedAt = params.publishedAt;
    this.images = params.images;
    this.languages = params.languages;
  }
}

export class TalksProvider {
  constructor() {
    this.talks = {};
  }

  add(params) {
    let talk = new Talk(params);
    this.talks[talk.id] = talk;
    return talk;
  }

  fetch() {
    return Http.get('/api/talks?limit=5').then((data) => {
      let talks = data.map(this.add, this);
      return Promise.resolve(talks);
    }).catch(err => {
      console.log(err);
    });
  }
}
