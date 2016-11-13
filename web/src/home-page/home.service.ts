import Talk from '../models/talk';
import Http from '../services/http';


class HomeService {
  talks: Talk[] = [];

  constructor(private http: Http) {}

  fetch() {
    if (this.talks.length) {
      return Promise.resolve(this.talks);
    } else {
      return this.http.getJson('/api/talks?limit=5').then((data: any[]) => {
        this.talks = data.map(d => new Talk(d));
        return Promise.resolve(this.talks);
      }).catch(err => {
        console.log(err);
      });
    }
  }
}

export default HomeService;
