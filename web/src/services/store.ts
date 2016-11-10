import Talk from '../models/talk';
import Http from './http';

class Store {
  slugToTalk = {};
  newest: Talk[] = [];

  constructor(private http: Http) {}

  add(data) {
    return new Talk(data);
  }

  getBySlug(slug) {
    let talk = this.slugToTalk[slug];
    if (talk) {
      return Promise.resolve(talk);
    } else {
      return this.http.get(`/api/talks/${slug}`).then(data => {
        talk = this.add(data);
        this.slugToTalk[talk.slug] = talk;
        return Promise.resolve(talk);
      }).catch(err => {
        console.log(err);
      });
    }
  }

  getNewest() {
    if (this.newest.length) {
      return Promise.resolve(this.newest);
    } else {
      return this.http.get('/api/talks?limit=5').then((data: any[]) => {
        this.newest = data.map(d => new Talk(d));
        return Promise.resolve(this.newest);
      }).catch(err => {
        console.log(err);
      });
    }
  }
}

export default Store;
